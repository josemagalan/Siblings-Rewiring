library(dplyr)
library(tidyr)
library(readr)
library(purrr)
library(stringr)
library(igraph)
library(tidygraph)

source("SAfunctions.R")

# Configuración de SLURM: Cada tarea procesa UN archivo
task_id <- as.numeric(Sys.getenv("SLURM_ARRAY_TASK_ID", unset = 1))  # ID del archivo
input_dir <- "results/"
output_dir <- "results/final_solutions"
output_csv <- paste0("results/experiment_results_", task_id, ".csv")

dir.create(output_dir, showWarnings = FALSE)

# Parámetros
max_iter <- 10000
Tf <- 0.01
perc <- 0.01
pesos <- seq(0, 1, by = 0.1)

# Obtener lista de archivos
files <- list.files(input_dir, pattern = "_datosNivelados.rds", full.names = TRUE)
if (task_id > length(files)) {
  cat("ID fuera de rango, terminando ejecución.\n")
  quit()
}
file <- files[task_id]

# Extraer parámetros del nombre del archivo
extraer_parametros <- function(filename) {
  nombre <- basename(filename)
  partes <- strsplit(nombre, "_")[[1]]
  list(
    grupos = as.numeric(partes[2]),
    alumnos = as.numeric(partes[3]),
    cursos = as.numeric(partes[4]),
    poison = as.numeric(partes[5]),
    semilla = as.numeric(partes[6])
  )
}

params <- extraer_parametros(file)
cat(sprintf("Procesando archivo: %s\n", file))

datos_nivelados <- readRDS(file)
resultados <- list()

for (i in seq_along(pesos)) {
  w1 <- pesos[i]
  w2 <- 1 - w1
  
  set.seed(params$semilla)
  
  # Ejecutar Recocido Simulado
  sa_result <- simulated_annealing(
    datos_inicial = datos_nivelados,
    w1 = w1,
    w2 = w2,
    max_iter = max_iter
  )
  
  # Evaluar solución inicial y final
  res_inicial <- construir_grafo_y_componentes(datos_nivelados)
  res_final <- construir_grafo_y_componentes(sa_result$best_solution)
  
  # Ejecutar Búsqueda Local
  ls_result <- local_search_first_improvement(
    asignaciones_df = sa_result$best_solution,
    w1 = w1,
    w2 = w2,
    perc = perc
  )
  
  res_final_ls <- construir_grafo_y_componentes(ls_result$solution)
  
  # Guardar resultados
  resultados[[i]] <- data.frame(
    archivo = basename(file),
    grupos = params$grupos,
    alumnos = params$alumnos,
    cursos = params$cursos,
    poison = params$poison,
    semilla = params$semilla,
    w1 = w1,
    w2 = w2,
    fitness_inicial = calcular_fitness_normalizado(datos_nivelados, w1, w2),
    fitness_final = ls_result$fitness,
    n_componentes_inicial = res_inicial$n_components,
    n_componentes_final = res_final_ls$n_components,
    varianza_inicial = var(res_inicial$sizes),
    varianza_final = var(res_final_ls$sizes)
  )
  
  cat(sprintf("  -> Peso %d/%d (w1=%.1f, w2=%.1f): Fitness final %.4f\n",
              i, length(pesos), w1, w2, ls_result$fitness))
  
  # Guardar la mejor solución final en un archivo RDS
  output_rds <- paste0(output_dir, "/", basename(file), "_w1_", w1, "_w2_", w2, "_final.rds")
  saveRDS(ls_result$solution, output_rds)
}

# Guardar resultados en CSV
final_df <- bind_rows(resultados)
write_csv(final_df, output_csv)

cat("Finalizado archivo", basename(file), "Resultados guardados en", output_csv, "\n")