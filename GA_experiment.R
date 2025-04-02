library(dplyr)
library(tidyr)
library(readr)
library(purrr)
library(stringr)
library(igraph)
library(tidygraph)

source("GA_functions.R")

# Configuración de SLURM: Cada tarea procesa UN archivo
task_id <- as.numeric(Sys.getenv("SLURM_ARRAY_TASK_ID", unset = 1))  # ID del archivo
input_dir <- "datasets/"
output_dir <- "results/final_solutions_GA"
output_csv <- paste0("results/GA/experiment_results_GA_", task_id, ".csv")

dir.create(output_dir, showWarnings = FALSE)


# Obtener lista de archivos
files <- list.files(input_dir, pattern = ".csv", full.names = TRUE)
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
    semilla = gsub("\\.csv$", "", partes[6])
  )
}

params <- extraer_parametros(file)
cat(sprintf("Procesando archivo: %s\n", file))


resultados <- list()




archivo <- file


# Extraer el número máximo de grupos y el tamaño máximo del grupo
max_grupos <- params$grupos
max_grupo_size <- params$alumnos

datos <- readr::read_csv(archivo,
                         col_types = cols(sibling_ids = col_character())) %>% 
  mutate(node = paste(course, group, sep = "-"))


pop_size = 50
num_generations = 1000 
p_mut = 0.1 
p_familia = 0.5 
p_cross = 0.2




# Ejecutar NSGA-II
resultados <- nsga2(datos, 
                    pop_size = pop_size, 
                    num_generations = num_generations, 
                    num_grupos = max_grupos, 
                    max_grupo_size = max_grupo_size, 
                    p_mut = p_mut, 
                    p_familia = p_familia, 
                    p_cross = p_cross)

# Extraer la evaluación final y la población final
final_evals <- resultados$evaluaciones
final_poblacion <- resultados$poblacion

# Extraer el frente de Pareto final
pareto_front_indices <- fast_non_dominated_sort(final_evals)[[1]]
pareto_front <- final_evals[pareto_front_indices, ]


# ======================== BLOQUE FINAL ADAPTADO ========================
# Crear lista para almacenar los resultados por solución
resultados <- list()

# Obtener los índices del frente de Pareto
pareto_front_indices <- fast_non_dominated_sort(final_evals)[[1]]
cat("Número de soluciones en el frente de Pareto:", length(pareto_front_indices), "\n")

# Procesar cada solución del frente de Pareto
for (i in seq_along(pareto_front_indices)) {
  idx <- pareto_front_indices[i]
  solucion <- final_poblacion[[idx]]
  
  # Evaluar solución con la función existente (ya calcula componentes y varianza)
  evaluacion <- evaluar_solucion(solucion)
  
  # Guardar solución como archivo .rds
  nombre_rds <- paste0(output_dir, "/", tools::file_path_sans_ext(basename(file)), "_pareto_", i, ".rds")
  saveRDS(solucion, nombre_rds)
  
  # Guardar resultado en la lista
  resultados[[i]] <- tibble(
    archivo = basename(file),
    grupos = params$grupos,
    alumnos = params$alumnos,
    cursos = params$cursos,
    poison = params$poison,
    semilla = params$semilla,
    solucion_id = i,
    n_componentes_final = evaluacion$n_componentes,
    varianza_final = evaluacion$varianza_tamanio
  )
}

# Combinar todos los resultados y guardar en CSV
final_df <- bind_rows(resultados)
write_csv(final_df, output_csv)

cat("Finalizado archivo", basename(file), "\n")
cat("Número de soluciones en el frente de Pareto:", nrow(final_df), "\n")
cat("Resultados guardados en", output_csv, "\n")
