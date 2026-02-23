# ==============================================================================
# KM-MILP.R
# ==============================================================================

library(lpSolve)
library(proxy)
library(dplyr)

# Salida
dir.create("predictions", showWarnings = FALSE, recursive = FALSE)
.pred_km_milp_csv <- "predictions/pred_KM-MILP.csv"

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
# MILP: asignación con tamaño <=
# -----------------------------
solve_milp_assignment <- function(data, centroids, size_constraints) {
  data <- as.matrix(data)
  centroids <- as.matrix(centroids)
  
  n <- nrow(data)
  k <- nrow(centroids)
  
  if (length(size_constraints) != k) stop("size_constraints length != k")
  if (n <= 1 || k < 2) stop("n <= 1 o k < 2")
  
  cost_mat <- as.matrix(proxy::dist(data, centroids, method = "cosine"))
  cost_vec <- as.vector(t(cost_mat))
  
  constr1 <- matrix(0, n, n * k)
  for (i in 1:n) {
    constr1[i, ((i - 1) * k + 1):(i * k)] <- 1
  }
  
  constr2 <- matrix(0, k, n * k)
  for (j in 1:k) {
    constr2[j, seq(j, n * k, by = k)] <- 1
  }
  
  result <- lp(
    direction = "min",
    objective.in = cost_vec,
    const.mat = rbind(constr1, constr2),
    const.dir = c(rep("=", n), rep("<=", k)),
    const.rhs = c(rep(1, n), as.numeric(size_constraints)),
    all.bin = TRUE
  )
  
  if (result$status != 0) stop("MILP failed")
  
  sol <- matrix(result$solution, nrow = n, byrow = TRUE)
  p <- apply(sol, 1, which.max)
  list(p = p)
}

# -----------------------------
# Loop tipo KMeans: centroides + MILP
# -----------------------------
clustering_with_size_constraints <- function(data, size_constraints, max_iter = 10, seed = 123) {
  data <- as.matrix(data)
  n <- nrow(data)
  k <- length(size_constraints)
  
  if (k < 2) stop("k < 2")
  if (sum(size_constraints) < n) stop("sum(size_constraints) < n")
  
  set.seed(seed)
  centroids <- data[sample(1:n, k), , drop = FALSE]
  p <- rep(1L, n)
  
  for (iter in 1:max_iter) {
    res <- solve_milp_assignment(data, centroids, size_constraints)
    p_new <- res$p
    
    new_centroids <- centroids
    for (j in 1:k) {
      pts <- data[p_new == j, , drop = FALSE]
      if (nrow(pts) > 0) new_centroids[j, ] <- colMeans(pts)
    }
    
    if (max(abs(centroids - new_centroids)) < 1e-4) {
      p <- p_new
      break
    }
    
    centroids <- new_centroids
    p <- p_new
  }
  
  list(p = p)
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
  
  k <- length(target_cardinality)
  if (k < 2) return(NULL)
  
  if (sum(target_cardinality) != nrow(X)) return(NULL)
  
  start_algo <- Sys.time()
  res <- tryCatch(
    clustering_with_size_constraints(X, target_cardinality, max_iter = 10, seed = 123),
    error = function(e) NULL
  )
  end_algo <- Sys.time()
  
  if (is.null(res) || is.null(res$p)) return(NULL)
  
  y_int <- as.integer(y)
  
  data.frame(
    name = dataset_name,
    n = length(res$p),
    k = length(unique(res$p)),
    y_predict = paste(as.integer(res$p), collapse = " "),
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
  cat("\n\n--- Executing KM-MILP for dataset at position:", i, "---\n")
  
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
  write.table(final_df, .pred_km_milp_csv, sep = ",", row.names = FALSE, col.names = TRUE)
  cat("\nKM-MILP finalizado. Archivo guardado exitosamente en:", .pred_km_milp_csv, "\n")
} else {
  cat("\nKM-MILP finalizado sin resultados válidos. No se creó el CSV.\n")
}