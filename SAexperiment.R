# Load required libraries
library(dplyr)
library(tidyr)
library(readr)
library(purrr)
library(stringr)
library(igraph)
library(tidygraph)

# Load custom Simulated Annealing functions and utilities
source("SAfunctions.R")

# SLURM configuration: Each array task processes ONE input file
task_id <- as.numeric(Sys.getenv("SLURM_ARRAY_TASK_ID", unset = 1))  # Get task ID from environment
input_dir <- "results/"                  # Directory containing input .rds files
output_dir <- "results/final_solutions"  # Directory to store final solutions
output_csv <- paste0("results/experiment_results_", task_id, ".csv")  # Output file for results summary

# Create output directory if it doesn't already exist
dir.create(output_dir, showWarnings = FALSE)

# Algorithm parameters
max_iter <- 10000                  # Maximum iterations for Simulated Annealing
Tf <- 0.01                         # Final temperature (currently unused)
perc <- 0.01                       # Percentage of neighborhood evaluated in local search
pesos <- seq(0, 1, by = 0.1)       # Range of weight combinations (for multi-objective fitness function)

# Get the list of input files
files <- list.files(input_dir, pattern = "_datosNivelados.rds", full.names = TRUE)

# Check if task_id is within valid range
if (task_id > length(files)) {
  cat("Task ID out of range, terminating execution.\n")
  quit()
}

# Select the input file for this task
file <- files[task_id]

# Extract metadata (number of groups, students, courses, etc.) from the filename
extract_parameters <- function(filename) {
  name <- basename(filename)
  parts <- strsplit(name, "_")[[1]]
  list(
    grupos = as.numeric(parts[2]),     # Number of groups
    alumnos = as.numeric(parts[3]),    # Number of students
    cursos = as.numeric(parts[4]),     # Number of courses
    poison = as.numeric(parts[5]),     # Noise/randomness parameter
    semilla = as.numeric(parts[6])     # Random seed
  )
}

params <- extract_parameters(file)
cat(sprintf("Processing file: %s\n", file))

# Load data
datos_nivelados <- readRDS(file)
results <- list()

# Iterate over all weight combinations
for (i in seq_along(pesos)) {
  w1 <- pesos[i]
  w2 <- 1 - w1
  
  set.seed(params$semilla)  # Ensure reproducibility for each run
  
  # Run Simulated Annealing
  sa_result <- simulated_annealing(
    datos_inicial = datos_nivelados,
    w1 = w1,
    w2 = w2,
    max_iter = max_iter
  )
  
  # Evaluate initial and final graph solutions
  res_inicial <- construir_grafo_y_componentes(datos_nivelados)
  res_final <- construir_grafo_y_componentes(sa_result$best_solution)
  
  # Run Local Search for further refinement
  ls_result <- local_search_first_improvement(
    asignaciones_df = sa_result$best_solution,
    w1 = w1,
    w2 = w2,
    perc = perc
  )
  
  res_final_ls <- construir_grafo_y_componentes(ls_result$solution)
  
  # Record results
  results[[i]] <- data.frame(
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
  
  cat(sprintf("  -> Weight %d/%d (w1=%.1f, w2=%.1f): Final fitness %.4f\n",
              i, length(pesos), w1, w2, ls_result$fitness))
  
  # Save the best solution after local search
  output_rds <- paste0(output_dir, "/", basename(file), "_w1_", w1, "_w2_", w2, "_final.rds")
  saveRDS(ls_result$solution, output_rds)
}

# Combine all results and save to CSV
final_df <- bind_rows(results)
write_csv(final_df, output_csv)

cat("Finished processing", basename(file), "Results saved to", output_csv, "\n")
