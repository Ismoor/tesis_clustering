# ==============================================================================
# HCAKC.R — Clustering con Restricciones de Cardinalidad (HCAKC)
# ==============================================================================

library(cluster)
library(proxy)
library(dplyr)

# Salida
dir.create("predictions", showWarnings = FALSE, recursive = FALSE)
.pred_hcakc_csv <- "predictions/pred_HCAKC.csv"

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
# HCAKC
# -----------------------------
run_HCAKC_algo <- function(X, y, target_cardinality) {
  if (is.null(X) || is.null(y) || is.null(target_cardinality)) stop("Datos nulos detectados.")
  X <- as.matrix(X)
  
  if (nrow(X) != length(y)) stop("nrow(X) != length(y)")
  if (any(is.na(target_cardinality))) stop("target_cardinality contiene NA")
  if (sum(target_cardinality) != nrow(X)) stop("Suma de cardinalidades != nrow(X)")
  
  K <- length(target_cardinality)
  if (K < 2) stop("K < 2")
  
  M <- matrix(0, K, K)
  C <- matrix(0, K, K)
  
  IS <- function(x, cluster, centroids) {
    a <- sqrt(sum((x - centroids[cluster, ])^2))
    b <- min(sapply(1:nrow(centroids), function(i) {
      if (i == cluster) Inf else sqrt(sum((x - centroids[i, ])^2))
    }))
    (b - a) / max(a, b)
  }
  
  CUCMC <- function(Xm, M, C) {
    for (i in seq_len(nrow(Xm))) {
      for (j in seq_len(ncol(Xm))) {
        if (M[i, j] == 1) Xm[i, j] <- 1 else if (C[i, j] == 1) Xm[i, j] <- 0
      }
    }
    Xm
  }
  
  HCAKC_specified <- function(data, K, cluster_sizes, M, C) {
    n <- nrow(data); p <- ncol(data)
    clusters <- integer(n)
    centroids <- matrix(NA_real_, nrow = K, ncol = p)
    
    start_idx <- 1
    for (k in seq_len(K)) {
      end_idx <- start_idx + cluster_sizes[k] - 1
      if (end_idx > n) stop("Índice fuera de rango en inicialización.")
      clusters[start_idx:end_idx] <- k
      centroids[k, ] <- colMeans(data[start_idx:end_idx, , drop = FALSE])
      start_idx <- end_idx + 1
    }
    
    IS_values <- sapply(seq_len(n), function(i) IS(data[i, ], clusters[i], centroids))
    
    Xmat <- matrix(0, K, K)
    for (i in seq_len(K)) {
      for (j in seq_len(K)) {
        IS_i <- IS_values[clusters == i]
        IS_j <- IS_values[clusters == j]
        if (length(IS_i) == 0 || length(IS_j) == 0) next
        min_len <- min(length(IS_i), length(IS_j))
        Xmat[i, j] <- sum(head(IS_i, min_len) * head(IS_j, min_len), na.rm = TRUE)
      }
    }
    Xmat <- CUCMC(Xmat, M, C)
    
    list(clusters = clusters, centroids = centroids)
  }
  
  result_specified <- HCAKC_specified(X, K, as.integer(target_cardinality), M, C)
  result_specified$clusters
}

# -----------------------------
# Runner: devuelve fila
# -----------------------------
run_clustering_row <- function(dataset, target_cardinality, dataset_name) {
  data <- prepare_data(dataset)
  if (is.null(data)) return(NULL)
  
  X <- data$X
  y <- data$y
  
  if (is.null(target_cardinality) || all(is.na(target_cardinality)) || length(target_cardinality) < 2) return(NULL)
  if (sum(target_cardinality) != nrow(X)) return(NULL)
  
  start_algo <- Sys.time()
  clusters <- tryCatch(run_HCAKC_algo(X, y, as.integer(target_cardinality)), error = function(e) NULL)
  end_algo <- Sys.time()
  
  if (is.null(clusters)) return(NULL)
  
  exec_time <- as.numeric(difftime(end_algo, start_algo, units = "secs"))
  y_int <- as.integer(y)
  
  data.frame(
    name = dataset_name,
    n = length(clusters),
    k = length(unique(clusters)),
    y_predict = paste(as.integer(clusters), collapse = " "),
    y_true = paste(as.integer(y_int), collapse = " "),
    target_cardinality = paste(as.integer(target_cardinality), collapse = " "),
    Execution_Time = exec_time,
    stringsAsFactors = FALSE
  )
}

# -----------------------------
# Loop principal
# -----------------------------
results_list <- list()

for (i in 1:nrow(odatasets_unique)) {
  cat("\n\n--- Executing HCAKC for dataset at position:", i, "---\n")
  
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
    cat("Error processing dataset at position", i, ":", e$message, "\n")
  })
}

# -----------------------------
# Escritura final
# -----------------------------
results_clean <- results_list[!sapply(results_list, is.null)]

if (length(results_clean) > 0) {
  final_df <- do.call(rbind, results_clean)
  write.table(final_df, .pred_hcakc_csv, sep = ",", row.names = FALSE, col.names = TRUE)
  cat("\nHCAKC finalizado. Archivo guardado exitosamente en:", .pred_hcakc_csv, "\n")
} else {
  cat("\nHCAKC finalizado sin resultados válidos. No se generó archivo CSV.\n")
}