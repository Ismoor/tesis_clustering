library(cluster)
library(proxy)
library(dplyr)

# Salida
dir.create("predictions", showWarnings = FALSE, recursive = FALSE)
.pred_bat_csv <- "predictions/pred_BAT.csv"

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
# Auxiliares BAT
# -----------------------------
calculate_centroids <- function(X, cluster_assignment, k) {
  X <- as.data.frame(X)
  centroids_df <- aggregate(X, by = list(cluster = cluster_assignment), FUN = mean)
  
  centroids_df <- centroids_df[order(centroids_df$cluster), , drop = FALSE]
  centroids <- as.matrix(centroids_df[, -1, drop = FALSE])
  
  if (nrow(centroids) < k) {
    tmp <- matrix(NA_real_, nrow = k, ncol = ncol(centroids))
    rownames(tmp) <- as.character(1:k)
    tmp[as.integer(centroids_df$cluster), ] <- centroids
    centroids <- tmp
  }
  
  if (any(is.na(centroids))) {
    global_mu <- colMeans(as.matrix(X), na.rm = TRUE)
    for (r in 1:nrow(centroids)) {
      if (any(is.na(centroids[r, ]))) centroids[r, ] <- global_mu
    }
  }
  
  centroids
}

adjust_cardinality <- function(cluster_assignment, X, centroids, target_cardinality) {
  max_iterations <- 1000
  iteration <- 0
  k <- length(target_cardinality)
  
  cluster_sizes <- sapply(1:k, function(c) sum(cluster_assignment == c, na.rm = TRUE))
  
  while (any(cluster_sizes > target_cardinality) && iteration < max_iterations) {
    iteration <- iteration + 1
    for (j in which(cluster_sizes > target_cardinality)) {
      idx <- which(cluster_assignment == j)
      if (length(idx) == 0) next
      element <- idx[1]
      
      distances <- colSums((t(centroids) - as.numeric(X[element, ]))^2)
      
      available_clusters <- which(cluster_sizes < target_cardinality)
      valid_clusters <- setdiff(available_clusters, j)
      
      if (length(valid_clusters) == 0) {
        cluster_assignment[element] <- -1
        cluster_sizes[j] <- cluster_sizes[j] - 1
      } else {
        chosen <- valid_clusters[which.min(distances[valid_clusters])]
        cluster_assignment[element] <- chosen
        cluster_sizes[j] <- cluster_sizes[j] - 1
        cluster_sizes[chosen] <- cluster_sizes[chosen] + 1
      }
    }
  }
  
  unassigned_idx <- which(cluster_assignment == -1)
  if (length(unassigned_idx) > 0) {
    for (element in unassigned_idx) {
      available_clusters <- which(cluster_sizes < target_cardinality)
      if (length(available_clusters) > 0) {
        chosen <- available_clusters[which.min(cluster_sizes[available_clusters])]
        cluster_assignment[element] <- chosen
        cluster_sizes[chosen] <- cluster_sizes[chosen] + 1
      } else {
        cluster_assignment[element] <- 1
        cluster_sizes[1] <- cluster_sizes[1] + 1
      }
    }
  }
  
  if (iteration >= max_iterations) warning("The adjustment process reached the maximum number of iterations.")
  cluster_assignment
}

generate_initial_solution <- function(X, target_cardinality, seed = 45) {
  set.seed(seed)
  Xmat <- as.matrix(X)
  k <- length(target_cardinality)
  
  km <- kmeans(Xmat, centers = k, nstart = 5, iter.max = 30)
  cluster_assignment <- km$cluster
  centroids <- calculate_centroids(Xmat, cluster_assignment, k)
  
  adjust_cardinality(cluster_assignment, Xmat, centroids, target_cardinality)
}

evaluate_solution <- function(cluster_assignment, X, target_cardinality, penalty_weight = 10) {
  if (length(unique(cluster_assignment)) < 2) return(-Inf)
  
  d <- proxy::dist(as.matrix(X), method = "cosine")
  ss <- tryCatch(cluster::silhouette(cluster_assignment, d), error = function(e) NULL)
  if (is.null(ss)) return(-Inf)
  
  current_counts <- tabulate(cluster_assignment, nbins = length(target_cardinality))
  penalty <- penalty_weight * sum(abs(current_counts - target_cardinality))
  
  mean(ss[, "sil_width"]) - penalty
}

# -----------------------------
# BAT
# -----------------------------
run_bat_algorithm <- function(X, y, target_cardinality,
                              n_bats = 30, max_iterations = 20,
                              f_min = 0, f_max = 2, loudness = 0.5, pulse_rate = 0.5,
                              alpha = 0.9, gamma = 0.9) {
  set.seed(1521)
  Xmat <- as.matrix(X)
  k <- length(target_cardinality)
  
  seeds <- sample(1:10000, n_bats, replace = FALSE)
  
  bats <- lapply(1:n_bats, function(i) {
    list(
      position = generate_initial_solution(Xmat, target_cardinality, seed = seeds[i]),
      velocity = rep(0, nrow(Xmat)),
      frequency = runif(1, f_min, f_max),
      loudness = loudness,
      pulse_rate = pulse_rate,
      seed = seeds[i]
    )
  })
  
  best_solution <- bats[[1]]$position
  best_score <- evaluate_solution(best_solution, Xmat, target_cardinality)
  best_seed <- bats[[1]]$seed
  
  for (iteration in 1:max_iterations) {
    for (i in 1:n_bats) {
      bat <- bats[[i]]
      bat$frequency <- runif(1, f_min, f_max)
      
      bat$velocity <- bat$velocity + (bat$position - best_solution) * bat$frequency
      new_position <- round(bat$position + bat$velocity)
      new_position <- pmin(pmax(new_position, 1), k)
      
      if (runif(1) > bat$pulse_rate) {
        new_position <- sample(1:k, nrow(Xmat), replace = TRUE)
      }
      
      for (j in 1:k) {
        while (sum(new_position == j) > target_cardinality[j]) {
          idx <- which(new_position == j)
          if (length(idx) > 0) {
            new_position[sample(idx, 1)] <- sample(1:k, 1)
          } else {
            break
          }
        }
      }
      
      new_score <- evaluate_solution(new_position, Xmat, target_cardinality)
      
      if (is.finite(new_score) && new_score > best_score && runif(1) < bat$loudness) {
        bats[[i]]$position <- new_position
        bats[[i]]$loudness <- alpha * bat$loudness
        bats[[i]]$pulse_rate <- pulse_rate * (1 - exp(-gamma * iteration))
        
        best_solution <- new_position
        best_score <- new_score
        best_seed <- bats[[i]]$seed
      }
    }
  }
  
  list(best_solution = best_solution, best_score = best_score, best_seed = best_seed, seeds = seeds)
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
  
  start_algo <- Sys.time()
  results <- tryCatch(run_bat_algorithm(X, y, as.integer(target_cardinality)), error = function(e) NULL)
  end_algo <- Sys.time()
  
  if (is.null(results) || is.null(results$best_solution)) return(NULL)
  
  y_predict <- results$best_solution
  y_int <- as.integer(y)
  
  data.frame(
    name = dataset_name,
    n = length(y_predict),
    k = length(unique(y_predict)),
    y_predict = paste(as.integer(y_predict), collapse = " "),
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
  cat("\n\n--- Executing BAT for dataset at position:", i, "---\n")
  tryCatch({
    dataset <- odatasets_unique$dataset[[i]]
    dataset_name <- odatasets_unique$name[[i]]
    target_cardinality <- odatasets_unique$class_distribution_vector[[i]]
    
    if (is.null(target_cardinality) || all(is.na(target_cardinality)) || length(target_cardinality) < 2) {
      cat("Target cardinality not available/invalid. Skipping.\n")
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

results_clean <- results_list[!sapply(results_list, is.null)]

if (length(results_clean) > 0) {
  final_df <- do.call(rbind, results_clean)
  write.table(final_df, .pred_bat_csv, sep = ",", row.names = FALSE, col.names = TRUE)
  cat("\nBAT Algorithm finalizado. Archivo guardado exitosamente en:", .pred_bat_csv, "\n")
} else {
  cat("\nBAT Algorithm finalizado sin resultados válidos. No se generó archivo CSV.\n")
}