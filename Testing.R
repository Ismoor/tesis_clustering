# -----------------------------------------------------------------------------
# Testing.R
# -----------------------------------------------------------------------------

# Instalar librerías de paralelismo
pkgs <- c("future", "future.apply", "peakRAM")
inst <- rownames(installed.packages())
to_install <- setdiff(pkgs, inst)
if (length(to_install) > 0) install.packages(to_install)

library(rstudioapi)
library(peakRAM)
library(future)
library(future.apply)

run_algoritmo_future <- function(numalt, datos_para_script) {
  
  odatasets_unique <- datos_para_script
  
  scripts <- c(
    "1"  = "Bat.R",
    "2"  = "CSCLP.R",
    "3"  = "Kmedoids.R",
    "4"  = "ACO.R",
    "5"  = "PSO.R",
    "6"  = "HCAKC.R",
    "7"  = "KM-MILP.R",
    "8"  = "SCK1_final.R",
    "9"  = "kmeans.R",
    "10" = "kmedoids_n.R",
    "11" = "AHC.R"
  )
  
  archivo_algoritmo <- scripts[as.character(numalt)]
  if (is.na(archivo_algoritmo)) return(paste("Opción inválida:", numalt))
  
  cat(paste(">>> Iniciando", archivo_algoritmo, "en PID:", Sys.getpid(), "\n"))
  
  tryCatch({
    resultado <- peakRAM(source(archivo_algoritmo, local = TRUE))
    
    resultado$Algoritmo <- archivo_algoritmo
    resultado$Fecha <- Sys.time()
    
    write.table(
      resultado,
      file = "peakRAM_log.csv",
      sep = ",",
      row.names = FALSE,
      col.names = !file.exists("peakRAM_log.csv"),
      append = TRUE
    )
    
    return(paste("EXITO:", archivo_algoritmo))
    
  }, error = function(e) {
    return(paste("ERROR en", archivo_algoritmo, ":", e$message))
  })
}

Execute_Test <- function(numalt) {
  if (interactive() && requireNamespace("rstudioapi", quietly = TRUE)) {
    script_dir <- dirname(rstudioapi::getActiveDocumentContext()$path)
    if (file.exists(script_dir)) setwd(script_dir)
  }
  
  message("Cargando datasets...")
  source("Openml.R")
  
  if (!exists("odatasets_unique")) stop("Openml.R no generó 'odatasets_unique'.")
  
  if (as.character(numalt) == "12") {
    message("\n--- Iniciando Ejecución Paralela Robusta (future) ---")
    
    n_cores <- max(1, parallel::detectCores() - 1)
    plan(multisession, workers = n_cores)
    message(paste("Utilizando", n_cores, "núcleos."))
    
    opciones <- 1:11
    
    resultados <- future_lapply(opciones, function(x) {
      run_algoritmo_future(x, odatasets_unique)
    }, future.seed = TRUE)
    
    message("\n--- Resumen de Ejecución ---")
    print(unlist(resultados))
    
    plan(sequential)
    
  } else {
    cat(paste("Ejecutando individualmente opción:", numalt, "\n"))
    print(run_algoritmo_future(numalt, odatasets_unique))
  }
}

Execute_Test(12)