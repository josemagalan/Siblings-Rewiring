library(dplyr)
library(tidyr)
library(readr)
library(purrr)
library(stringr)
library(igraph)
library(tidygraph)

source("GA_functions.R")

# SLURM configuration: Each task processes ONE file
task_id <- as.numeric(Sys.getenv("SLURM_ARRAY_TASK_ID", unset = 1))  # Task ID = index of the file to process
input_dir <- "datasets/"                                             # Directory with input datasets
output_dir <- "results/final_solutions_GA"                           # Output directory for final RDS solutions
output_csv <- paste0("results/GA/experiment_results_GA_", task_id, ".csv")  # Summary CSV for the task

# Create output directory if it doesn't exist
dir.create(output_dir, showWarnings = FALSE)

# Get the list of available files
files <- list.files(input_dir, pattern = ".csv", full.names = TRUE)

# Exit if task_id exceeds number of available files
if (task_id > length(files)) {
  cat("Task ID out of range, exiting execution.\n")
  quit()
}

file <- files[task_id]

# Extract parameters encoded in the filename
extraer_parametros <- function(filename) {
  nombre <- basename(filename)
  partes <- strsplit(nombre, "_")[[1]]
  list(
    grupos = as.numeric(partes[2]),                         # Number of groups
    alumnos = as.numeric(partes[3]),                        # Number of students
    cursos = as.numeric(partes[4]),                         # Number of courses
    poison = as.numeric(partes[5]),                         # Noise level
    semilla = gsub("\\.csv$", "", partes[6])                # Random seed (from filename)
  )
}

params <- extraer_parametros(file)
cat(sprintf("Processing file: %s\n", file))

# Initialize result list
resultados <- list()

archivo <- file

# Extract maximum number of groups and max group size from filename parameters
max_grupos <- params$grupos
max_grupo_size <- params$alumnos

# Read the input dataset
datos <- readr::read_csv(archivo,
                         col_types = cols(sibling_ids = col_character())) %>% 
  mutate(node = paste(course, group, sep = "-"))  # Add node identifier

# Genetic algorithm hyperparameters
pop_size = 50               # Population size
num_generations = 1000      # Number of generations
p_mut = 0.1                 # Mutation probability
p_familia = 0.5             # Probability of using family-based mutation
p_cross = 0.2               # Crossover probability

# Execute the NSGA-II algorithm
resultados <- nsga2(datos, 
                    pop_size = pop_size, 
                    num_generations = num_generations, 
                    num_grupos = max_grupos, 
                    max_grupo_size = max_grupo_size, 
                    p_mut = p_mut, 
                    p_familia = p_familia, 
                    p_cross = p_cross)

# Extract final population and evaluations
final_evals <- resultados$evaluaciones
final_poblacion <- resultados$poblacion

# Extract the final Pareto front
pareto_front_indices <- fast_non_dominated_sort(final_evals)[[1]]
pareto_front <- final_evals[pareto_front_indices, ]

# ======================== FINAL BLOCK ========================

# Create list to store solution metrics
resultados <- list()

# Display number of solutions on the Pareto front
pareto_front_indices <- fast_non_dominated_sort(final_evals)[[1]]
cat("Number of solutions in the Pareto front:", length(pareto_front_indices), "\n")

# Process each Pareto-optimal solution
for (i in seq_along(pareto_front_indices)) {
  idx <- pareto_front_indices[i]
  solucion <- final_poblacion[[idx]]
  
  # Evaluate the solution (e.g., number of components and size variance)
  evaluacion <- evaluar_solucion(solucion)
  
  # Save the solution to an RDS file
  nombre_rds <- paste0(output_dir, "/", tools::file_path_sans_ext(basename(file)), "_pareto_", i, ".rds")
  saveRDS(solucion, nombre_rds)
  
  # Store metadata and evaluation in the result list
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

# Combine all records and export to CSV
final_df <- bind_rows(resultados)
write_csv(final_df, output_csv)

cat("Finished processing", basename(file), "\n")
cat("Number of solutions in the Pareto front:", nrow(final_df), "\n")
cat("Results saved to", output_csv, "\n")
