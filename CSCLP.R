# -----------------------------------------------------------------------------
# Load libraries
# -----------------------------------------------------------------------------
library(cluster)
library(proxy)
library(lpSolve)
library(dplyr)

# Salida
dir.create("predictions", showWarnings = FALSE, recursive = FALSE)
.pred_csclp_csv <- "predictions/pred_CSCLP.csv"

# -----------------------------------------------------------------------------
# Helpers robustos
# -----------------------------------------------------------------------------
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
  
  # Eliminar columnas constantes
  X_num <- X_num[, vapply(X_num, function(z) length(unique(z)) > 1, logical(1)), drop = FALSE]
  if (ncol(X_num) == 0) return(NULL)
  
  X_num
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
  
  list(X = X_num, y = as.factor(y))
}

# -----------------------------------------------------------------------------
# CSCLP
# -----------------------------------------------------------------------------
run_CSCLP_algo <- function(dataset, target_cardinality, dist_method = "cosine", seed = 123) {
  
  prepared <- prepare_data(dataset)
  if (is.null(prepared)) stop("Dataset inválido (no se pudo preparar X/y).")
  
  X <- prepared$X
  y <- prepared$y
  
  Xmat <- as.matrix(X)
  n <- nrow(Xmat)
  k <- length(target_cardinality)
  
  if (is.na(k) || k < 2) stop("k inválido (<2).")
  if (k >= n) stop(paste0("k=", k, " inválido para n=", n, "."))
  if (any(is.na(target_cardinality))) stop("target_cardinality contiene NA.")
  if (sum(target_cardinality) != n) stop(paste0("target_cardinality suma ", sum(target_cardinality), " pero n=", n, "."))
  if (any(target_cardinality < 1)) stop("target_cardinality tiene valores < 1 (invalido).")
  
  set.seed(seed)
  pam_result <- cluster::pam(Xmat, k = k)
  c_idx <- pam_result$id.med
  v_idx <- setdiff(1:n, c_idx)
  m <- length(v_idx)
  
  cost_mat <- as.matrix(proxy::dist(Xmat[c_idx, , drop = FALSE],
                                    Xmat[v_idx, , drop = FALSE],
                                    method = dist_method))
  
  f <- as.vector(t(cost_mat))
  
  A_point <- matrix(0, nrow = m, ncol = k*m)
  for (i in 1:m) {
    for (j in 1:k) {
      A_point[i, (j - 1) * m + i] <- 1
    }
  }
  b_point <- rep(1, m)
  
  A_card <- matrix(0, nrow = k, ncol = k*m)
  for (j in 1:k) {
    A_card[j, ((j - 1) * m + 1):(j * m)] <- 1
  }
  b_card <- as.integer(target_cardinality) - 1
  if (any(b_card < 0)) stop("target_cardinality[j]-1 < 0, ILP inválido.")
  
  A <- rbind(A_point, A_card)
  rhs <- c(b_point, b_card)
  dir <- rep("==", nrow(A))
  
  start_alg <- Sys.time()
  result_lp <- lp(direction = "min",
                  objective.in = f,
                  const.mat = A,
                  const.dir = dir,
                  const.rhs = rhs,
                  all.bin = TRUE)
  end_alg <- Sys.time()
  alg_time <- as.numeric(difftime(end_alg, start_alg, units = "secs"))
  
  if (result_lp$status != 0) stop(paste0("ILP infeasible/failed. status=", result_lp$status))
  
  x <- result_lp$solution
  assign_mat <- matrix(x, nrow = k, byrow = TRUE)
  
  y_predict <- integer(n)
  
  for (j in 1:k) y_predict[c_idx[j]] <- j
  
  for (i in 1:m) {
    j_star <- which.max(assign_mat[, i])
    y_predict[v_idx[i]] <- j_star
  }
  
  list(
    y_predict = y_predict,
    y = y,
    Execution_Time = alg_time
  )
}

# -----------------------------------------------------------------------------
# Runner: devuelve fila
# -----------------------------------------------------------------------------
run_clustering_row <- function(dataset, target_cardinality, dataset_name) {
  result <- run_CSCLP_algo(dataset, target_cardinality)
  
  y_predict <- result$y_predict
  y_true <- as.integer(result$y)
  
  data.frame(
    name = dataset_name,
    n = length(y_predict),
    k = length(unique(y_predict)),
    y_predict = paste(as.integer(y_predict), collapse = " "),
    y_true = paste(as.integer(y_true), collapse = " "),
    target_cardinality = paste(as.integer(target_cardinality), collapse = " "),
    Execution_Time = result$Execution_Time,
    stringsAsFactors = FALSE
  )
}

# -----------------------------------------------------------------------------
# Loop principal
# -----------------------------------------------------------------------------
results_list <- list()

for (i in 1:nrow(odatasets_unique)) {
  cat("\n\n--- Running CSCLP for dataset at position:", i, "---\n")
  
  tryCatch({
    dataset <- odatasets_unique$dataset[[i]]
    dataset_name <- odatasets_unique$name[[i]]
    target_cardinality <- odatasets_unique$class_distribution_vector[[i]]
    
    if (is.null(dataset) || is.null(target_cardinality) || all(is.na(target_cardinality)) || length(target_cardinality) < 2) {
      cat("Dataset o cardinalidad inválida. Skipping...\n")
      next
    }
    
    row_result <- run_clustering_row(dataset, target_cardinality, dataset_name)
    
    if (!is.null(row_result)) {
      results_list[[i]] <- row_result
      cat("Time (LP):", row_result$Execution_Time, "s\n")
    } else {
      cat("Dataset skipped.\n")
    }
    
  }, error = function(e) {
    cat("Error processing dataset at position", i, ":", e$message, "\n")
  })
}

# -----------------------------------------------------------------------------
# Escritura final
# -----------------------------------------------------------------------------
results_clean <- results_list[!sapply(results_list, is.null)]

if (length(results_clean) > 0) {
  final_df <- do.call(rbind, results_clean)
  write.table(final_df, .pred_csclp_csv, sep = ",", row.names = FALSE, col.names = TRUE)
  cat("\nCSCLP finalizado. Archivo guardado exitosamente en:", .pred_csclp_csv, "\n")
} else {
  cat("\nCSCLP finalizado sin resultados válidos. No se generó archivo CSV.\n")
}