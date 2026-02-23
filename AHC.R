# ==============================================================================
# AHC.R — Clustering Normal (Agglomerative Hierarchical Clustering)
# ==============================================================================

# ---- Paquetes ----
pkgs <- c("proxy", "dplyr", "readr")
inst <- rownames(installed.packages())
to_install <- setdiff(pkgs, inst)
if (length(to_install) > 0) install.packages(to_install)

library(proxy)
library(dplyr)
library(readr)

# ---- Configuración ----
dir.create("predictions", showWarnings = FALSE, recursive = FALSE)
.pred_ahc_csv <- "predictions/pred_AHC.csv"

DIST_METHOD <- "cosine"
LINKAGE <- "complete"
MAX_N_FOR_DIST <- 5000

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
# AHC
# -----------------------------
run_ahc <- function(X, k, dist_method = "cosine", linkage = "complete") {
  Xmat <- as.matrix(X)
  n <- nrow(Xmat)
  
  if (n > MAX_N_FOR_DIST) {
    stop(paste0("n=", n, " supera MAX_N_FOR_DIST=", MAX_N_FOR_DIST, " (AHC con dist sería muy pesado)."))
  }
  
  if (k >= n) stop(paste0("k=", k, " es inválido para n=", n))
  
  d <- proxy::dist(Xmat, method = dist_method)
  hc <- hclust(d, method = linkage)
  cutree(hc, k = k)
}

run_clustering_row <- function(dataset, target_cardinality, dataset_name) {
  data <- prepare_data(dataset)
  if (is.null(data)) return(NULL)
  
  X <- data$X
  y <- data$y
  
  k <- length(target_cardinality)
  if (is.na(k) || k < 2) return(NULL)
  
  start_algo <- Sys.time()
  y_pred <- tryCatch(run_ahc(X, k, dist_method = DIST_METHOD, linkage = LINKAGE), error = function(e) NULL)
  end_algo <- Sys.time()
  
  if (is.null(y_pred)) return(NULL)
  
  data.frame(
    name = dataset_name,
    n = length(y_pred),
    k = length(unique(y_pred)),
    y_predict = paste(as.integer(y_pred), collapse = " "),
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
  cat("\n\n--- Executing AHC for dataset at position:", i, "---\n")
  
  tryCatch({
    dataset <- odatasets_unique$dataset[[i]]
    dataset_name <- odatasets_unique$name[[i]]
    target_cardinality <- odatasets_unique$class_distribution_vector[[i]]
    
    if (is.null(target_cardinality) || all(is.na(target_cardinality))) next
    
    row_result <- run_clustering_row(dataset, target_cardinality, dataset_name)
    if (!is.null(row_result)) results_list[[i]] <- row_result
    
  }, error = function(e) cat("Error:", e$message, "\n"))
}

results_clean <- results_list[!sapply(results_list, is.null)]

if (length(results_clean) > 0) {
  write.table(do.call(rbind, results_clean),
              .pred_ahc_csv, sep = ",",
              row.names = FALSE, col.names = TRUE)
  cat("\nAHC Guardado en:", .pred_ahc_csv, "\n")
} else {
  cat("\nAHC finalizado sin resultados válidos. No se generó CSV.\n")
}