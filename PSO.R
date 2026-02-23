# ==============================================================================
# PSO.R — PSO con Restricciones de Cardinalidad
# ==============================================================================

library(cluster)
library(pso)
library(proxy)
library(dplyr)

# Salida
dir.create("predictions", showWarnings = FALSE, recursive = FALSE)
.pred_pso_csv <- "predictions/pred_PSO.csv"

# -----------------------------
# Helpers robustos
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

# -----------------------------
# PSO
# -----------------------------
run_PSO <- function(X, target_cardinality) {
  Xmat <- as.matrix(X)
  
  if (!all(is.finite(Xmat))) stop("X contiene NA/Inf")
  if (!all(vapply(as.data.frame(Xmat), is.numeric, logical(1)))) stop("X debe ser numérico")
  
  n <- nrow(Xmat)
  k <- length(target_cardinality)
  
  if (k < 2) stop("k < 2")
  if (sum(target_cardinality) != n) stop("sum(target_cardinality) != n (inconsistente)")
  
  D <- as.matrix(proxy::dist(Xmat, method = "cosine"))
  
  cost_function <- function(par, dist_matrix, cluster_sizes, k) {
    n <- length(par)
    cluster_assignment <- integer(n)
    
    ord <- order(par)
    start_idx <- 1
    for (i in 1:k) {
      end_idx <- start_idx + cluster_sizes[i] - 1
      cluster_assignment[ord[start_idx:end_idx]] <- i
      start_idx <- end_idx + 1
    }
    
    total_distance <- 0
    for (i in 1:k) {
      ci <- which(cluster_assignment == i)
      if (length(ci) > 1) {
        cd <- dist_matrix[ci, ci, drop = FALSE]
        total_distance <- total_distance + sum(cd[lower.tri(cd, diag = FALSE)])
      }
    }
    total_distance
  }
  
  pso_result <- psoptim(
    par = runif(n),
    fn = cost_function,
    dist_matrix = D,
    cluster_sizes = as.integer(target_cardinality),
    k = k,
    lower = 0,
    upper = 1,
    control = list(maxit = 20, s = 40)
  )
  
  label_pred <- integer(n)
  ord <- order(pso_result$par)
  start_idx <- 1
  for (i in 1:k) {
    end_idx <- start_idx + target_cardinality[i] - 1
    label_pred[ord[start_idx:end_idx]] <- i
    start_idx <- end_idx + 1
  }
  
  list(y_predict = label_pred)
}

# -----------------------------
# Runner: devuelve fila
# -----------------------------
run_clustering_row <- function(dataset, target_cardinality, dataset_name) {
  data <- prepare_data(dataset)
  if (is.null(data)) return(NULL)
  
  X <- data$X
  y <- data$y
  
  if (is.null(target_cardinality) || all(is.na(target_cardinality))) return(NULL)
  target_cardinality <- as.integer(target_cardinality)
  
  if (sum(target_cardinality) != nrow(X)) return(NULL)
  if (length(target_cardinality) < 2) return(NULL)
  
  start_algo <- Sys.time()
  results <- tryCatch(run_PSO(X, target_cardinality), error = function(e) NULL)
  end_algo <- Sys.time()
  
  if (is.null(results) || is.null(results$y_predict)) return(NULL)
  
  y_int <- as.integer(y)
  
  data.frame(
    name = dataset_name,
    n = length(results$y_predict),
    k = length(unique(results$y_predict)),
    y_predict = paste(as.integer(results$y_predict), collapse = " "),
    y_true = paste(as.integer(y_int), collapse = " "),
    target_cardinality = paste(as.integer(target_cardinality), collapse = " "),
    Execution_Time = as.numeric(difftime(end_algo, start_algo, units = "secs")),
    stringsAsFactors = FALSE
  )
}

# -----------------------------
# Loop principal
# -----------------------------
results_list <- list()

for (i in 1:nrow(odatasets_unique)) {
  cat("\n\n--- Executing PSO for dataset at position:", i, "---\n")
  
  tryCatch({
    dataset <- odatasets_unique$dataset[[i]]
    dataset_name <- odatasets_unique$name[[i]]
    target_cardinality <- odatasets_unique$class_distribution_vector[[i]]
    
    if (is.null(dataset) || is.null(target_cardinality) || all(is.na(target_cardinality))) {
      cat("Dataset o cardinalidad inválida. Skipping.\n")
      next
    }
    
    row_result <- run_clustering_row(dataset, target_cardinality, dataset_name)
    
    if (!is.null(row_result)) {
      results_list[[i]] <- row_result
      cat("Time:", row_result$Execution_Time, "seconds\n")
    } else {
      cat("Dataset skipped.\n")
    }
    
  }, error = function(e) {
    cat("Error:", e$message, "\n")
  })
}

# -----------------------------
# Escritura final
# -----------------------------
results_clean <- results_list[!sapply(results_list, is.null)]

if (length(results_clean) > 0) {
  final_df <- do.call(rbind, results_clean)
  write.table(final_df, .pred_pso_csv, sep = ",", row.names = FALSE, col.names = TRUE)
  cat("\nPSO finalizado. Archivo guardado exitosamente en:", .pred_pso_csv, "\n")
} else {
  cat("\nPSO finalizado sin resultados válidos. No se creó el CSV.\n")
}