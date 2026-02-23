# Framework de Clustering con Restricciones de Cardinalidad

## Descripción General

Este repositorio implementa un framework experimental completo para la evaluación de algoritmos de clustering, tanto con restricciones de cardinalidad como sin ellas.

El proyecto integra:

- Descarga automática de datasets desde OpenML  
- Caché local para reproducibilidad  
- Ejecución paralela de múltiples algoritmos  
- Medición de consumo de memoria (peak RAM)  
- Exportación estandarizada de predicciones  
- Evaluación avanzada con métricas sensibles a restricciones  

El objetivo principal es permitir una comparación controlada, reproducible y estructurada entre diferentes enfoques de clustering.

---

## Algoritmos Implementados

### Clustering con Restricciones

- K-Medoids con restricciones de tamaño (SC_medoids)  
- KM-MILP (K-Means + Programación Lineal Entera Mixta)  
- SCK1 (Asignación exacta vía ILP)  
- PSO con restricciones de cardinalidad  
- CSCLP  
- HCAKC  
- BAT  
- ACO  

### Clustering sin Restricciones (Baselines)

- K-Means  
- K-Medoids (PAM)  
- Clustering Jerárquico Aglomerativo (AHC)  

Todos los algoritmos exportan sus predicciones en un formato unificado dentro del directorio `predictions/`.

---

## Estructura del Proyecto

```
CODIGOT1-MAIN/
│
├── Openml.R               # Descarga, filtrado y caché de datasets
├── Testing.R              # Controlador de ejecución (individual y paralela)
├── evaluate_metrics.R     # Cálculo de métricas
│
├── Kmedoids.R             # K-Medoids con restricciones
├── KM-MILP.R              # Clustering con MILP
├── SCK1_final.R           # Asignación exacta ILP
├── PSO.R                  # PSO con restricciones
├── kmeans.R               # K-Means estándar
├── kmedoids_n.R           # PAM estándar
├── AHC.R                  # Clustering jerárquico
│
├── predictions/           # Archivos generados con etiquetas
├── metrics/               # Métricas calculadas
├── datasets_local/        # Caché local de datasets
```

---

## Pipeline de Datasets

1. Filtrado por metadatos desde OpenML:
   - 4–20 características  
   - 200–4000 instancias  
   - 2–10 clases  
   - Sin valores faltantes  
   - Máximo 1 atributo simbólico  

2. Deduplicación por:
   - Estructura (clases e instancias)  
   - Nombre  
   - Firma digital basada en cardinalidades ordenadas  

3. Almacenamiento local para evitar descargas repetidas.

---

## Ejecución

### Ejecutar un algoritmo específico

```r
source("Testing.R")
Execute_Test(3)  # Ejemplo: ejecuta Kmedoids.R
```

### Ejecutar todos los algoritmos en paralelo

```r
Execute_Test(12)
```

La ejecución paralela utiliza:

- future  
- future.apply  
- Detección automática de núcleos  
- Registro de uso de memoria con peakRAM  

---

## Métricas de Evaluación

El módulo `evaluate_metrics.R` calcula:

- ASE (Absolute Size Error)  
- CVR (Constraint Violation Rate)  
- MaxDiff (diferencia máxima entre tamaños de clusters)  
- SSE_res (error cuadrático interno del modelo evaluado)  
- SSE_kmeans (baseline K-Means)  
- PoF (Penalty of Feasibility)  
- Silhouette Mean (distancia coseno)  
- CE (Clustering Enthalpy)  

### Clustering Enthalpy (CE)

\[
CE = (1 - Silhouette_{mean}) + \lambda \cdot \frac{ASE}{n}
\]

Donde λ se evalúa en el rango:

```
0.25, 0.50, ..., 2.00
```

Los resultados generados incluyen:

- metrics_all.csv  
- summary_by_algo_lambda.csv  

---

## Reproducibilidad

El framework asegura:

- Uso de semillas fijas  
- Control explícito de cardinalidades objetivo  
- Formato estandarizado de predicciones  
- Caché local de datasets  
- Fallback controlado para cálculo de baseline  

---

## Requisitos

Paquetes principales utilizados:

- mlr3  
- mlr3oml  
- proxy  
- cluster  
- lpSolve  
- pso  
- future  
- future.apply  
- peakRAM  
- dplyr  
- readr  
- purrr  
- tibble  

---

## Propósito

Este proyecto está orientado a:

- Comparación experimental de clustering con y sin restricciones  
- Evaluación de calidad estructural de particiones  
- Investigación en métricas sensibles a cardinalidad  
- Construcción de pipelines reproducibles para benchmarking  