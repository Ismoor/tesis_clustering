# ==============================================================================
# kmeans.R — Clustering Normal (K-Means)
# ==============================================================================

# ---- Paquetes ----
pkgs <- c("dplyr", "readr")
inst <- rownames(installed.packages())
to_install <- setdiff(pkgs, inst)
if (length(to_install) > 0) install.packages(to_install)

library(dplyr)
library(readr)

# ---- Salida ----
dir.create("predictions", showWarnings = FALSE, recursive = FALSE)
.pred_kmeans_csv <- "predictions/pred_KMEANS.csv"

# ---- Configuración K-Means ----
USE_COSINE_LIKE <- TRUE
NSTART   <- 10
ITER_MAX <- 50
SEED     <- 123

# -----------------------------
# Helpers
# -----------------------------
l2_normalize_rows <- function(X) {
  X <- as.matrix(X)
  norms <- sqrt(rowSums(X^2))
  norms[norms == 0] <- 1
  X / norms
}

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
  
  # Drop constantes
  X_num <- X_num[, vapply(X_num, function(z) length(unique(z)) > 1, logical(1)), drop = FALSE]
  if (ncol(X_num) == 0) return(NULL)
  
  X_num
}

y_looks_like_class <- function(y, n) {
  y <- as.vector(y)
  if (is.factor(y) || is.character(y)) return(TRUE)
  u <- length(unique(y))
  if (u <= 20) return(TRUE)
  if (u > max(30, floor(0.3 * n))) return(FALSE)
  TRUE
}

prepare_data <- function(dataset) {
  dataset <- as.data.frame(dataset)
  if (ncol(dataset) < 2) return(NULL)
  
  if ("class" %in% colnames(dataset)) {
    y <- dataset$class
    X <- dataset[, setdiff(colnames(dataset), "class"), drop = FALSE]
  } else {
    y <- dataset[[ncol(dataset)]]
    X <- dataset[, -ncol(dataset), drop = FALSE]
  }
  
  X_num <- make_numeric_X(X)
  if (is.null(X_num)) return(NULL)
  
  n <- nrow(X_num)
  if (length(y) != n) return(NULL)
  
  list(X = X_num, y = y)
}

run_kmeans <- function(X, k) {
  Xmat <- as.matrix(X)
  
  if (USE_COSINE_LIKE) {
    Xmat <- l2_normalize_rows(Xmat)
  }
  
  n <- nrow(Xmat)
  if (k < 2) stop("k < 2")
  if (k >= n) stop(paste0("k=", k, " inválido para n=", n, " (k debe ser < n)"))
  
  set.seed(SEED)
  km <- kmeans(Xmat, centers = k, nstart = NSTART, iter.max = ITER_MAX)
  km$cluster
}

pick_k <- function(target_cardinality, y, n) {
  if (!is.null(target_cardinality) && length(target_cardinality) > 0 && !all(is.na(target_cardinality))) {
    tc <- as.integer(target_cardinality)
    if (length(tc) >= 2 && sum(tc) == n && all(tc > 0)) {
      return(list(k = length(tc), tc = tc, reason = "target_cardinality"))
    }
  }
  
  if (!is.null(y) && y_looks_like_class(y, n)) {
    yfac <- as.factor(y)
    k <- length(levels(yfac))
    if (k >= 2) return(list(k = k, tc = NULL, reason = "y_classes"))
  }
  
  list(k = NA_integer_, tc = NULL, reason = "no_k")
}

run_clustering_row <- function(dataset, target_cardinality, dataset_name) {
  data <- prepare_data(dataset)
  if (is.null(data)) return(NULL)
  
  X <- data$X
  y <- data$y
  n <- nrow(X)
  
  kk <- pick_k(target_cardinality, y, n)
  k <- kk$k
  tc_used <- kk$tc
  
  if (is.na(k) || k < 2) {
    cat("Dataset skipped (no se pudo definir k fiable).\n")
    return(NULL)
  }
  
  start_algo <- Sys.time()
  y_predict <- tryCatch(run_kmeans(X, k), error = function(e) {
    cat("kmeans error:", conditionMessage(e), "\n")
    NULL
  })
  end_algo <- Sys.time()
  
  if (is.null(y_predict)) return(NULL)
  
  y_true_int <- as.integer(as.factor(y))
  
  data.frame(
    name = dataset_name,
    n = length(y_predict),
    k = length(unique(y_predict)),
    y_predict = paste(as.integer(y_predict), collapse = " "),
    y_true = paste(as.integer(y_true_int), collapse = " "),
    target_cardinality = if (!is.null(tc_used)) paste(tc_used, collapse = " ") else "",
    Execution_Time = as.numeric(difftime(end_algo, start_algo, units = "secs")),
    stringsAsFactors = FALSE
  )
}

# ---- Cargar odatasets_unique si no existe ----
if (!exists("odatasets_unique")) {
  odatasets_unique <- NULL
  if (file.exists("Openml.R")) {
    try({ source("Openml.R") }, silent = TRUE)
  }
  if (is.null(odatasets_unique) && file.exists("odatasets_unique.rds")) {
    odatasets_unique <- tryCatch(readRDS("odatasets_unique.rds"), error = function(e) NULL)
  }
  if (is.null(odatasets_unique)) {
    stop("No se pudo cargar odatasets_unique (ni desde Openml.R ni desde odatasets_unique.rds).")
  }
}

if (!("dataset" %in% colnames(odatasets_unique)) || !("name" %in% colnames(odatasets_unique))) {
  stop("odatasets_unique debe contener columnas 'dataset' y 'name'.")
}

# ---- Loop principal ----
results_list <- list()

for (i in 1:nrow(odatasets_unique)) {
  cat("\n\n--- Executing KMEANS for dataset at position:", i, "---\n")
  
  tryCatch({
    dataset <- odatasets_unique$dataset[[i]]
    dataset_name <- odatasets_unique$name[[i]]
    
    target_cardinality <- NULL
    if ("class_distribution_vector" %in% colnames(odatasets_unique)) {
      target_cardinality <- odatasets_unique$class_distribution_vector[[i]]
    }
    
    row_result <- run_clustering_row(dataset, target_cardinality, dataset_name)
    
    if (!is.null(row_result)) {
      results_list[[i]] <- row_result
      cat("Time:", row_result$Execution_Time, "s\n")
    } else {
      cat("Dataset skipped.\n")
    }
  }, error = function(e) {
    cat("Error:", e$message, "\n")
  })
}

# ---- Escritura final ----
results_clean <- results_list[!sapply(results_list, is.null)]

cat("\nWorking directory:", getwd(), "\n")
cat("Resultados válidos:", length(results_clean), "\n")

if (length(results_clean) > 0) {
  final_df <- do.call(rbind, results_clean)
  write.table(final_df, .pred_kmeans_csv, sep = ",", row.names = FALSE, col.names = TRUE)
  cat("\nKMEANS finalizado. Archivo guardado exitosamente en:", .pred_kmeans_csv, "\n")
} else {
  cat("\nKMEANS finalizado sin resultados válidos. No se generó archivo CSV.\n")
}