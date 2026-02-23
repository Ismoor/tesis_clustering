# ==============================================================================
# KMEDOIDS.R — K-Medoids con Restricciones de Tamaño (K-MedoidsSC)
# ==============================================================================

library(cluster)
library(proxy)
library(dplyr)

# Salida
dir.create("predictions", showWarnings = FALSE, recursive = FALSE)
.pred_KMEDOIDS_csv <- "predictions/pred_KMEDOIDS.csv"

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
# SC_medoids (lógica original)
# -----------------------------
SC_medoids <- function(D, k, E, C = NULL) {
  if (is.null(C)) C <- sample(1:nrow(D), k)
  
  cl <- max.col(-D[, C, drop = FALSE])
  sorted_points <- order(apply(D[, C, drop = FALSE], 1, min))
  
  for (i in 1:k) {
    cl[sorted_points[1:E[i]]] <- i
    sorted_points <- sorted_points[-(1:E[i])]
  }
  
  for (point in sorted_points) {
    cl[point] <- which.min(D[point, C])
  }
  
  y_predict <- numeric(nrow(D))
  for (i in 1:k) {
    ii <- which(cl == i)
    y_predict[ii] <- i
  }
  
  list(medoids = C, clustering = cl, y_predict = y_predict)
}

# -----------------------------
# Runner: devuelve fila
# -----------------------------
run_KmedoidsSC_row <- function(dataset, target_cardinality, dataset_name) {
  data <- prepare_data(dataset)
  if (is.null(data)) return(NULL)
  
  X <- data$X
  y <- data$y
  
  if (is.null(target_cardinality) || all(is.na(target_cardinality))) return(NULL)
  if (sum(target_cardinality) != nrow(X)) return(NULL)
  
  k <- length(target_cardinality)
  if (k < 2) return(NULL)
  
  D <- proxy::dist(as.matrix(X), method = "cosine")
  D <- as.matrix(D)
  
  pam_result <- pam(X, k)
  C <- pam_result$id.med
  
  start_algo <- Sys.time()
  result <- SC_medoids(D, k, as.integer(target_cardinality), C)
  end_algo <- Sys.time()
  
  exec_time <- as.numeric(difftime(end_algo, start_algo, units = "secs"))
  y_int <- as.integer(y)
  
  data.frame(
    name = dataset_name,
    n = length(result$y_predict),
    k = length(unique(result$y_predict)),
    y_predict = paste(as.integer(result$y_predict), collapse = " "),
    y_true = paste(y_int, collapse = " "),
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
  cat("\n\n--- Executing KMEDOIDS for dataset at position:", i, "---\n")
  
  tryCatch({
    dataset <- odatasets_unique$dataset[[i]]
    dataset_name <- odatasets_unique$name[[i]]
    target_cardinality <- odatasets_unique$class_distribution_vector[[i]]
    
    if (is.null(dataset) || is.null(target_cardinality) || all(is.na(target_cardinality))) {
      cat("Dataset o cardinalidad inválida. Skipping.\n")
      next
    }
    
    row_result <- run_KmedoidsSC_row(dataset, target_cardinality, dataset_name)
    
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
  write.table(final_df, .pred_KMEDOIDS_csv,
              sep = ",", row.names = FALSE, col.names = TRUE)
  cat("\nK-Medoids finalizado. Archivo guardado exitosamente en:",
      .pred_KMEDOIDS_csv, "\n")
} else {
  cat("\nK-Medoids finalizado sin resultados válidos. No se creó el CSV.\n")
}