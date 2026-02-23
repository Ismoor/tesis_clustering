# ==============================================================================
# evaluate_metrics.R
# Calcula: ASE, CVR, MaxDiff, PoF, CE (barrido lambda)
# ==============================================================================

# ---- Paquetes ----
pkgs <- c("dplyr", "readr", "stringr", "purrr", "tibble", "cluster", "proxy")
inst <- rownames(installed.packages())
to_install <- setdiff(pkgs, inst)
if (length(to_install) > 0) install.packages(to_install)

library(dplyr)
library(readr)
library(stringr)
library(purrr)
library(tibble)
library(cluster)
library(proxy)

# ---- Configuración ----
PRED_DIR <- "predictions"
OUT_DIR  <- "metrics"
dir.create(PRED_DIR, showWarnings = FALSE, recursive = TRUE)
dir.create(OUT_DIR, showWarnings = FALSE, recursive = TRUE)
ERR_LOG  <- file.path(OUT_DIR, "errors.log")

USE_DATA_FOR_INTERNAL <- TRUE
CE_LAMBDAS <- seq(0.25, 2.00, by = 0.25)

if (file.exists(ERR_LOG)) file.remove(ERR_LOG)

# -----------------------------
# Helpers generales
# -----------------------------
parse_int_vec <- function(x) {
  x <- trimws(as.character(x))
  if (is.na(x) || nchar(x) == 0) return(integer(0))
  x <- gsub("\\[|\\]", "", x)
  x <- gsub(",", " ", x)
  x <- gsub("\\s+", " ", x)
  xs <- trimws(strsplit(x, " ", fixed = TRUE)[[1]])
  xs <- xs[xs != ""]
  as.integer(xs)
}

# -----------------------------
# Baseline KMEANS precomputado (PoF)
# -----------------------------
KMEANS_PRED_PATH <- file.path(PRED_DIR, "pred_KMEANS.csv")
kmeans_map <- NULL

if (file.exists(KMEANS_PRED_PATH)) {
  df_km <- suppressMessages(readr::read_csv(KMEANS_PRED_PATH, show_col_types = FALSE))
  if (all(c("name", "y_predict") %in% names(df_km))) {
    kmeans_map <- setNames(df_km$y_predict, df_km$name)
  } else {
    cat(paste(Sys.time(), "[WARN] pred_KMEANS.csv sin columnas name/y_predict.\n"),
        file = ERR_LOG, append = TRUE)
  }
} else {
  cat(paste(Sys.time(), "[WARN] No existe pred_KMEANS.csv; PoF hará fallback.\n"),
      file = ERR_LOG, append = TRUE)
}

# -----------------------------
# Cargar odatasets_unique
# -----------------------------
odatasets_unique <- NULL
if (USE_DATA_FOR_INTERNAL) {
  if (file.exists("Openml.R")) {
    try({ source("Openml.R") }, silent = TRUE)
  }
  if (is.null(odatasets_unique) && file.exists("odatasets_unique.rds")) {
    odatasets_unique <- tryCatch(readRDS("odatasets_unique.rds"), error = function(e) NULL)
  }
}

# -----------------------------
# Preprocesamiento de X
# -----------------------------
make_numeric_X <- function(X) {
  X <- as.data.frame(X)
  
  X_num <- lapply(X, function(col) {
    if (is.numeric(col) || is.integer(col)) return(as.numeric(col))
    if (is.logical(col)) return(as.numeric(col))
    if (is.factor(col))  return(as.numeric(col))
    if (is.character(col)) return(as.numeric(as.factor(col)))
    return(NULL)
  })
  
  keep <- !vapply(X_num, is.null, logical(1))
  X_num <- X_num[keep]
  if (length(X_num) == 0) return(NULL)
  
  X_num <- as.data.frame(X_num)
  
  X_num <- X_num[, vapply(X_num, function(z) length(unique(z)) > 1, logical(1)), drop = FALSE]
  if (ncol(X_num) == 0) return(NULL)
  
  X_num
}

prepare_X_from_dataset <- function(dataset) {
  dset <- as.data.frame(dataset)
  if (ncol(dset) < 2) return(NULL)
  
  if ("class" %in% colnames(dset)) {
    X <- dset[, setdiff(colnames(dset), "class"), drop = FALSE]
  } else {
    X <- dset[, -ncol(dset), drop = FALSE]
  }
  
  make_numeric_X(X)
}

get_X_by_name_openml <- function(dataset_name) {
  if (is.null(odatasets_unique)) return(NULL)
  if (!"name" %in% colnames(odatasets_unique)) return(NULL)
  
  idx <- which(odatasets_unique$name == dataset_name)
  if (length(idx) == 0) return(NULL)
  
  dset <- odatasets_unique[idx[1], ]$dataset[[1]]
  prepare_X_from_dataset(dset)
}

# -----------------------------
# Datasets artificiales (testing)
# -----------------------------
set.seed(123)

make_synthetic_X <- function(n1, n2, sep = 6) {
  C1 <- cbind(rnorm(n1, mean = 0, sd = 0.4), rnorm(n1, mean = 0, sd = 0.4))
  C2 <- cbind(rnorm(n2, mean = sep, sd = 0.4), rnorm(n2, mean = sep, sd = 0.4))
  rbind(C1, C2)
}

X_feasible <- make_synthetic_X(5, 5, sep = 7)
y_true_feasible <- c(rep(1, 5), rep(2, 5))
y_pred_feasible <- y_true_feasible
tgt_feasible <- c(5, 5)

X_infeasible <- make_synthetic_X(5, 5, sep = 7)
y_true_infeasible <- c(rep(1, 5), rep(2, 5))
y_pred_infeasible <- c(rep(1, 8), rep(2, 2))
tgt_infeasible <- c(5, 5)

ART_X <- list(
  synthetic_feasible   = as.data.frame(X_feasible),
  synthetic_infeasible = as.data.frame(X_infeasible)
)

get_X_by_name <- function(dataset_name) {
  if (dataset_name %in% names(ART_X)) return(ART_X[[dataset_name]])
  get_X_by_name_openml(dataset_name)
}

pred_metric_test_path <- file.path(PRED_DIR, "pred_metric_test.csv")
metric_test_df <- tibble(
  name = c("synthetic_feasible", "synthetic_infeasible"),
  y_predict = c(
    paste(y_pred_feasible, collapse = " "),
    paste(y_pred_infeasible, collapse = " ")
  ),
  y_true = c(
    paste(y_true_feasible, collapse = " "),
    paste(y_true_infeasible, collapse = " ")
  ),
  target_cardinality = c(
    paste(tgt_feasible, collapse = " "),
    paste(tgt_infeasible, collapse = " ")
  ),
  Execution_Time = c(0.001, 0.001)
)
write_csv(metric_test_df, pred_metric_test_path)

# -----------------------------
# Métricas
# -----------------------------
ase_maxdiff_metrics <- function(card_pred, tgt) {
  card_pred <- as.numeric(card_pred)
  tgt <- as.numeric(tgt)
  
  if (length(card_pred) != length(tgt)) {
    stop("card_pred y tgt deben tener la misma longitud (k).")
  }
  
  ASE <- sum(abs(card_pred - tgt))
  MaxDiff <- if (length(card_pred) >= 2) max(card_pred) - min(card_pred) else NA_real_
  list(ASE = ASE, MaxDiff = MaxDiff)
}

cvr_metrics <- function(card_pred, tgt) {
  card_pred <- as.numeric(card_pred)
  tgt <- as.numeric(tgt)
  
  if (length(card_pred) != length(tgt)) {
    stop("card_pred y tgt deben tener la misma longitud (k).")
  }
  
  k <- length(tgt)
  violated <- sum(card_pred != tgt)
  CVR <- violated / k
  list(CVR = CVR, Violated = violated, Total = k)
}

sse_pof_metrics <- function(X, y_predict, k_target, dataset_name, kmeans_map) {
  if (is.null(X)) {
    return(list(SSE_res = NA_real_, SSE_kmeans = NA_real_, PoF = NA_real_))
  }
  
  Xmat <- as.matrix(X)
  n <- nrow(Xmat)
  if (length(y_predict) != n) stop("Longitud de y_predict no coincide con nrow(X).")
  
  min_lbl <- min(y_predict)
  expected_levels <- if (min_lbl == 0) 0:(k_target - 1) else 1:k_target
  
  SSE_res <- 0
  for (lbl in expected_levels) {
    idx <- which(y_predict == lbl)
    if (length(idx) == 0) next
    pts <- Xmat[idx, , drop = FALSE]
    mu  <- colMeans(pts)
    dif <- sweep(pts, 2, mu, FUN = "-")
    SSE_res <- SSE_res + sum(dif^2)
  }
  
  SSE_kmeans <- NA_real_
  km_labels_str <- NULL
  
  if (!is.null(kmeans_map) && dataset_name %in% names(kmeans_map)) {
    km_labels_str <- kmeans_map[[dataset_name]]
  }
  
  if (!is.null(km_labels_str) && nchar(trimws(km_labels_str)) > 0) {
    y_km <- parse_int_vec(km_labels_str)
    
    if (length(y_km) == n) {
      uk <- length(unique(y_km))
      if (uk == k_target) {
        min_km <- min(y_km)
        lv_km <- if (min_km == 0) 0:(k_target - 1) else 1:k_target
        
        SSE_km_tmp <- 0
        for (lbl in lv_km) {
          idx <- which(y_km == lbl)
          if (length(idx) == 0) next
          pts <- Xmat[idx, , drop = FALSE]
          mu  <- colMeans(pts)
          dif <- sweep(pts, 2, mu, FUN = "-")
          SSE_km_tmp <- SSE_km_tmp + sum(dif^2)
        }
        SSE_kmeans <- SSE_km_tmp
      } else {
        SSE_kmeans <- NA_real_
      }
    } else {
      SSE_kmeans <- NA_real_
    }
  }
  
  if (is.na(SSE_kmeans)) {
    set.seed(123)
    km <- tryCatch(
      kmeans(Xmat, centers = k_target, nstart = 10, iter.max = 50),
      error = function(e) NULL
    )
    if (!is.null(km)) SSE_kmeans <- sum(km$withinss)
  }
  
  PoF <- NA_real_
  if (!is.na(SSE_kmeans) && SSE_kmeans > 1e-12) {
    PoF <- SSE_res / SSE_kmeans
  }
  
  list(SSE_res = SSE_res, SSE_kmeans = SSE_kmeans, PoF = PoF)
}

silhouette_mean_cosine <- function(X, y_predict) {
  if (is.null(X)) return(NA_real_)
  Xmat <- as.matrix(X)
  n <- nrow(Xmat)
  
  if (length(y_predict) != n) stop("Longitud de y_predict no coincide con nrow(X).")
  if (length(unique(y_predict)) < 2) return(NA_real_)
  
  d <- tryCatch(proxy::dist(Xmat, method = "cosine"), error = function(e) NULL)
  if (is.null(d)) return(NA_real_)
  
  labs <- as.integer(as.factor(y_predict))
  sil <- tryCatch(cluster::silhouette(labs, d), error = function(e) NULL)
  if (is.null(sil)) return(NA_real_)
  
  mean(sil[, "sil_width"], na.rm = TRUE)
}

ce_enthalpy_metric <- function(Si_mean, ASE, n, lambda) {
  if (is.na(Si_mean) || is.na(ASE) || is.na(n) || n <= 0) return(NA_real_)
  (1 - Si_mean) + lambda * (ASE / n)
}

# -----------------------------
# Core: una fila base (sin lambda)
# -----------------------------
compute_metrics_one_base <- function(name, labels_str, y_true_str, target_card_str, exec_time) {
  
  y_predict <- parse_int_vec(labels_str)
  tgt       <- parse_int_vec(target_card_str)
  
  n <- length(y_predict)
  k_target <- length(tgt)
  
  if (n == 0) stop("y_predict vacío.")
  if (k_target == 0) stop("target_cardinality vacío.")
  
  min_lbl <- min(y_predict)
  expected_levels <- if (min_lbl == 0) 0:(k_target - 1) else 1:k_target
  
  y_factor  <- factor(y_predict, levels = expected_levels)
  card_pred <- as.numeric(table(y_factor))
  
  ase_out <- ase_maxdiff_metrics(card_pred, tgt)
  cvr_out <- cvr_metrics(card_pred, tgt)
  
  X <- NULL
  if (USE_DATA_FOR_INTERNAL) {
    X <- get_X_by_name(name)
    
    if (is.null(X)) {
      cat(paste(Sys.time(), "[X=NULL]", name, "\n"), file = ERR_LOG, append = TRUE)
    } else if (nrow(X) != n) {
      cat(paste(Sys.time(), "[X n mismatch]", name,
                "nrow(X)=", nrow(X), "n_pred=", n, "\n"),
          file = ERR_LOG, append = TRUE)
      X <- NULL
    }
  }
  
  sse_out <- list(SSE_res = NA_real_, SSE_kmeans = NA_real_, PoF = NA_real_)
  if (!is.null(X)) {
    sse_out <- sse_pof_metrics(X, y_predict, k_target, name, kmeans_map)
  }
  
  Si_mean <- NA_real_
  if (!is.null(X)) {
    Si_mean <- silhouette_mean_cosine(X, y_predict)
  }
  
  tibble(
    name                 = name,
    n                    = n,
    k_target             = k_target,
    cardinality_pred     = paste(card_pred, collapse = " "),
    cardinality_target   = paste(tgt, collapse = " "),
    ASE                  = ase_out$ASE,
    ASE_norm             = ase_out$ASE / n,
    CVR                  = cvr_out$CVR,
    MaxDiff_ClusterSizes = ase_out$MaxDiff,
    Silhouette_mean      = Si_mean,
    SSE_res              = sse_out$SSE_res,
    SSE_kmeans           = sse_out$SSE_kmeans,
    PoF                  = sse_out$PoF,
    Execution_Time       = as.numeric(exec_time)
  )
}

# -----------------------------
# Expandir CE para lambdas
# -----------------------------
expand_ce_lambdas <- function(df_base, lambda_grid) {
  bind_rows(lapply(lambda_grid, function(lam) {
    df_base %>%
      mutate(
        CE_lambda = lam,
        CE = ce_enthalpy_metric(Silhouette_mean, ASE, n, lam)
      )
  }))
}

# -----------------------------
# Evaluar un archivo pred_*.csv
# -----------------------------
evaluate_file <- function(path_csv) {
  algo <- gsub("^pred_|\\.csv$", "", basename(path_csv))
  
  message(">> Procesando: ", algo, " (", basename(path_csv), ")",
          " | CE_lambdas=", paste(CE_LAMBDAS, collapse = ","))
  
  df <- suppressMessages(readr::read_csv(path_csv, show_col_types = FALSE))
  
  req_cols <- c("name", "y_predict", "target_cardinality", "Execution_Time")
  if (!all(req_cols %in% names(df))) {
    msg <- paste("Faltan columnas en", basename(path_csv))
    cat(paste(Sys.time(), msg, "\n"), file = ERR_LOG, append = TRUE)
    return(NULL)
  }
  
  if (!"y_true" %in% names(df)) df$y_true <- NA
  
  res <- pmap_dfr(
    list(df$name, df$y_predict, df$y_true, df$target_cardinality, df$Execution_Time),
    function(nm, yp, yt, tc, et) {
      tryCatch({
        base_row <- compute_metrics_one_base(nm, yp, yt, tc, et)
        out <- expand_ce_lambdas(base_row, CE_LAMBDAS)
        out$algorithm <- algo
        out
      }, error = function(e) {
        err_msg <- paste0("[", algo, "] Dataset: ", nm, " -> Error: ", conditionMessage(e))
        cat(paste(Sys.time(), err_msg, "\n"), file = ERR_LOG, append = TRUE)
        
        bind_rows(lapply(CE_LAMBDAS, function(lam) {
          tibble(
            name                 = nm,
            algorithm            = algo,
            n                    = NA_integer_,
            k_target             = NA_integer_,
            cardinality_pred     = NA_character_,
            cardinality_target   = NA_character_,
            ASE                  = NA_real_,
            ASE_norm             = NA_real_,
            CVR                  = NA_real_,
            MaxDiff_ClusterSizes = NA_real_,
            Silhouette_mean      = NA_real_,
            SSE_res              = NA_real_,
            SSE_kmeans           = NA_real_,
            PoF                  = NA_real_,
            CE                   = NA_real_,
            CE_lambda            = lam,
            Execution_Time       = as.numeric(et)
          )
        }))
      })
    }
  )
  
  if (nrow(res) > 0) {
    write_csv(res, file.path(OUT_DIR, paste0("metrics_", algo, ".csv")))
  }
  res
}

# -----------------------------
# Ejecución principal
# -----------------------------
pred_files <- list.files(PRED_DIR, pattern = "^pred_.*\\.csv$", full.names = TRUE)

if (length(pred_files) == 0) {
  stop("No hay archivos en ", PRED_DIR)
}

message("Iniciando evaluación de ", length(pred_files), " archivos de predicción...")

all_metrics_list <- lapply(pred_files, evaluate_file)
all_metrics <- bind_rows(all_metrics_list)

if (nrow(all_metrics) > 0) {
  write_csv(all_metrics, file.path(OUT_DIR, "metrics_all.csv"))
  
  summary_by_algo_lambda <- all_metrics %>%
    group_by(algorithm, CE_lambda) %>%
    summarise(
      N_Datasets    = n(),
      ASE_avg       = mean(ASE, na.rm = TRUE),
      ASE_norm_avg  = mean(ASE_norm, na.rm = TRUE),
      CVR_avg       = mean(CVR, na.rm = TRUE),
      MaxDiff_avg   = mean(MaxDiff_ClusterSizes, na.rm = TRUE),
      Sil_avg       = mean(Silhouette_mean, na.rm = TRUE),
      PoF_avg       = mean(PoF, na.rm = TRUE),
      CE_avg        = mean(CE, na.rm = TRUE),
      SSE_res_avg   = mean(SSE_res, na.rm = TRUE),
      Time_avg      = mean(Execution_Time, na.rm = TRUE),
      .groups       = "drop"
    )
  
  write_csv(summary_by_algo_lambda, file.path(OUT_DIR, "summary_by_algo_lambda.csv"))
  
  message("\n========================================")
  message("Evaluación finalizada con éxito.")
  print(summary_by_algo_lambda)
  message("Resultados guardados en: ", OUT_DIR)
  message("Archivo de test generado en: ", pred_metric_test_path)
  message("CE lambdas: ", paste(CE_LAMBDAS, collapse = ", "))
} else {
  message("No se generaron métricas válidas. Revisa errors.log")
}