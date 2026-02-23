library(proxy)
library(mlr3oml)
library(mlr3)
library(pryr)
library(dplyr)

dir.create("datasets_local", showWarnings = FALSE, recursive = TRUE)

filter_unique_datasets_metadata <- function(dataset) {
  
  dataset %>%
    distinct(NumberOfClasses, NumberOfInstances, .keep_all = TRUE) %>%
    mutate(name_lower = tolower(name)) %>%
    distinct(name_lower, .keep_all = TRUE) %>%
    select(-name_lower) %>%
    filter(!is.na(NumberOfSymbolicFeatures), NumberOfSymbolicFeatures <= 1) %>%
    filter(!is.na(NumberOfMissingValues), NumberOfMissingValues == 0)
}

load_dataset <- function(id) {
  
  local_path <- file.path("datasets_local", paste0("dataset_", id, ".rds"))
  
  if (file.exists(local_path)) {
    return(readRDS(local_path))
  }
  
  tryCatch({
    odata <- OMLData$new(id = id)
    dataset <- odata$data
    
    if (any(duplicated(names(dataset)))) {
      warning(paste("Dataset ID", id, ": Duplicate columns found. Renaming columns..."))
      names(dataset) <- make.unique(names(dataset))
    }
    
    saveRDS(dataset, file = local_path)
    message(paste("Dataset ID", id, "downloaded and saved locally."))
    return(dataset)
    
  }, error = function(e) {
    warning(paste("Error loading dataset ID", id, ":", e$message))
    return(NULL)
  })
}

local_list_path <- "datasets_local/oml_list_cache.rds"

if (file.exists(local_list_path)) {
  message("Cargando lista de datasets desde caché local...")
  odatasets <- readRDS(local_list_path)
} else {
  message("Consultando API de OpenML para listar datasets...")
  tryCatch({
    odatasets <- list_oml_data(
      number_features  = c(4, 20),
      number_instances = c(200, 4000),
      number_classes   = c(2, 10)
    )
    saveRDS(odatasets, local_list_path)
    message("Lista guardada en caché local.")
  }, error = function(e) {
    stop("Error crítico: No se pudo conectar con OpenML para obtener la lista y no hay caché local. ", e$message)
  })
}

odatasets_unique <- filter_unique_datasets_metadata(odatasets)

get_class_distributions <- function(id, expected_classes) {
  
  dataset <- load_dataset(id)
  
  if (is.null(dataset)) {
    return(list(text = "Error", vector = NA, data = NULL))
  }
  
  if ("class" %in% colnames(dataset)) {
    target <- dataset$class
  } else {
    target <- dataset[[ncol(dataset)]]
  }
  
  class_dist <- table(target)
  
  num_classes_found <- length(class_dist)
  if (num_classes_found != expected_classes) {
    warning(paste("Dataset ID", id, "has a discrepancy in the number of classes:",
                  "expected =", expected_classes, ", found =", num_classes_found))
    return(list(text = "Error", vector = NA, data = NULL))
  }
  
  class_dist_text <- paste(names(class_dist), as.integer(class_dist), sep = ":", collapse = "; ")
  class_dist_vector <- as.integer(class_dist)
  
  return(list(text = class_dist_text, vector = class_dist_vector, data = dataset))
}

start_time <- Sys.time()

class_distributions <- mapply(
  FUN = get_class_distributions,
  id = odatasets_unique$data_id,
  expected_classes = odatasets_unique$NumberOfClasses,
  SIMPLIFY = FALSE
)

end_time <- Sys.time()
execution_time <- end_time - start_time
print(paste("Total loading execution time:", execution_time))

valid_idx <- !sapply(class_distributions, function(x) is.null(x$data))
odatasets_unique <- odatasets_unique[valid_idx, ]
valid_datasets <- class_distributions[valid_idx]

odatasets_unique$class_distribution <- sapply(valid_datasets, `[[`, "text")
odatasets_unique$class_distribution_vector <- lapply(valid_datasets, `[[`, "vector")
odatasets_unique$dataset <- lapply(valid_datasets, `[[`, "data")

odatasets_unique <- odatasets_unique %>%
  mutate(
    class_dist_signature = sapply(class_distribution_vector, function(v) {
      if (all(is.na(v))) return(NA_character_)
      paste(sort(as.integer(v)), collapse = "-")
    })
  ) %>%
  distinct(NumberOfClasses, NumberOfInstances, class_dist_signature, .keep_all = TRUE)

message(paste("Datasets finales tras firma digital:", nrow(odatasets_unique)))