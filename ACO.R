library(cluster)
library(proxy)

# Salida
dir.create("predictions", showWarnings = FALSE, recursive = FALSE)
.pred_aco_csv <- "predictions/pred_ACO.csv"

# -----------------------------
# Helpers: X numérico robusto
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
# ACO
# -----------------------------
run_ACO <- function(X, target_cardinality) {
  
  Xmat <- as.matrix(X)
  n <- nrow(Xmat)
  k <- length(target_cardinality)
  
  if (is.na(k) || k < 2) stop("k inválido (<2).")
  if (k >= n) stop(paste0("k=", k, " inválido para n=", n, "."))
  
  n_ants <- 50
  max_iterations <- 20
  penalty_weight <- 100
  
  evaluate_solution <- function(cluster_assignment) {
    if (length(unique(cluster_assignment)) < 2) return(-Inf)
    
    d <- proxy::dist(Xmat, method = "cosine")
    ss <- tryCatch(cluster::silhouette(cluster_assignment, d), error = function(e) NULL)
    if (is.null(ss)) return(-Inf)
    
    penalty <- 0
    for (j in 1:k) {
      cardinality_diff <- abs(sum(cluster_assignment == j) - target_cardinality[j])
      if (cardinality_diff > 0) penalty <- penalty + cardinality_diff * penalty_weight
    }
    
    mean(ss[, "sil_width"]) - penalty
  }
  
  generate_initial_solution <- function() {
    km <- kmeans(Xmat, centers = k, nstart = 5, iter.max = 30)
    cluster_assignment <- km$cluster
    
    # Ajuste simple de cardinalidades (heurístico)
    for (j in 1:k) {
      while (sum(cluster_assignment == j) > target_cardinality[j]) {
        idx <- which(cluster_assignment == j)
        cluster_assignment[sample(idx, 1)] <- sample(setdiff(1:k, j), 1)
      }
    }
    cluster_assignment
  }
  
  perturb_solution <- function(cluster_assignment) {
    new_cluster_assignment <- cluster_assignment
    for (j in 1:n) {
      if (runif(1) < 0.1) new_cluster_assignment[j] <- sample(1:k, 1)
    }
    new_cluster_assignment
  }
  
  best_score <- -Inf
  best_cluster_assignment <- NULL
  
  for (iteration in 1:max_iterations) {
    ants <- vector("list", n_ants)
    
    for (i in 1:n_ants) {
      ca <- generate_initial_solution()
      ca <- perturb_solution(ca)
      ants[[i]] <- ca
    }
    
    for (i in 1:n_ants) {
      score <- evaluate_solution(ants[[i]])
      if (is.finite(score) && score > best_score) {
        best_score <- score
        best_cluster_assignment <- ants[[i]]
      }
    }
  }
  
  list(best_solution = best_cluster_assignment, best_score = best_score)
}

run_clustering_row <- function(dataset, target_cardinality, dataset_name) {
  data <- prepare_data(dataset)
  if (is.null(data)) return(NULL)
  
  X <- data$X
  y <- data$y
  
  if (is.null(target_cardinality) || length(target_cardinality) < 2 || all(is.na(target_cardinality))) return(NULL)
  
  start_algo <- Sys.time()
  results <- tryCatch(run_ACO(X, as.integer(target_cardinality)), error = function(e) NULL)
  end_algo <- Sys.time()
  
  if (is.null(results)) return(NULL)
  y_predict <- results$best_solution
  if (is.null(y_predict)) return(NULL)
  
  data.frame(
    name = dataset_name,
    n = length(y_predict),
    k = length(unique(y_predict)),
    y_predict = paste(as.integer(y_predict), collapse = " "),
    y_true = paste(as.integer(y), collapse = " "),
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
  cat("\n\n--- Executing ACO for dataset at position:", i, "---\n")
  
  tryCatch({
    dataset <- odatasets_unique$dataset[[i]]
    dataset_name <- odatasets_unique$name[[i]]
    target_cardinality <- odatasets_unique$class_distribution_vector[[i]]
    
    if (is.null(target_cardinality) || all(is.na(target_cardinality)) || length(target_cardinality) < 2) {
      cat("Target cardinality inválida. Skipping.\n")
      next
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

results_clean <- results_list[!sapply(results_list, is.null)]
if (length(results_clean) > 0) {
  write.table(do.call(rbind, results_clean), .pred_aco_csv,
              sep = ",", row.names = FALSE, col.names = TRUE)
  cat("\nACO Guardado en:", .pred_aco_csv, "\n")
} else {
  cat("\nACO finalizado sin resultados válidos. No se generó CSV.\n")
}