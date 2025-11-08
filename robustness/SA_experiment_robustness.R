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
input_dir <- "resultsRobustness/"                  # Directory containing input .rds files
output_dir <- "resultsRobustness/final_solutions"  # Directory to store final solutions
output_csv <- paste0("resultsRobustness/experiment_results_", task_id, ".csv")  # Output file for results summary

# Create output directory if it doesn't already exist
dir.create(output_dir, showWarnings = FALSE)

# Algorithm parameters
max_iter <- 10000# Maximum iterations for Simulated Annealing
Tf <- 0.01                         # Final temperature (currently unused)
perc <- 0.01                       # Percentage of neighborhood evaluated in local search
pesos <- seq(0, 1, by = 0.1)       # Range of weight combinations (for multi-objective fitness function)

# Get the list of input files
files <- list.files(
  input_dir,
  pattern = "^ds_(GEOM|NEGBIN|EMP)_.*_datosNivelados\\.rds$",
  full.names = TRUE
)

# Check if task_id is within valid range
if (task_id > length(files)) {
  cat("Task ID out of range, terminating execution.\n")
  quit()
}

# Select the input file for this task
file <- files[task_id]

extract_parameters <- function(file_path) {
  fn   <- basename(file_path)
  base <- stringr::str_remove(fn, "_datosNivelados\\.rds$")
  parts <- stringr::str_split(base, "_", simplify = TRUE)
  
  dist <- parts[2]
  
  groups  <- as.numeric(stringr::str_remove(parts[3], "G$"))
  spg     <- as.numeric(stringr::str_remove(parts[4], "S$"))  # students per group
  courses <- as.numeric(stringr::str_remove(parts[5], "C$"))
  
  mu_target <- NA_real_
  r_disp    <- NA_real_
  p_param   <- NA_real_
  emp_type  <- NA_character_
  seed      <- NA_real_
  
  if (dist == "GEOM") {
    mu_target <- as.numeric(stringr::str_remove(parts[6], "^mu"))
    p_param   <- as.numeric(stringr::str_remove(parts[7], "^p"))
    seed      <- as.numeric(parts[8])
  } else if (dist == "NEGBIN") {
    mu_target <- as.numeric(stringr::str_remove(parts[6], "^mu"))
    r_disp    <- as.numeric(stringr::str_remove(parts[7], "^r"))
    p_param   <- as.numeric(stringr::str_remove(parts[8], "^p"))
    seed      <- as.numeric(parts[9])
  } else if (dist == "EMP") {
    emp_type  <- parts[6]  # fixed | geomTail
    seed      <- as.numeric(parts[7])
  }
  
  list(
    filename = fn,
    distribution = dist,
    emp_type = emp_type,
    groups = groups,
    students_per_group = spg,
    courses = courses,
    alumnos_totales = groups * spg * courses,
    mu_target = mu_target,
    r = r_disp,
    p = p_param,
    seed = seed,
    fullpath = file_path
  )
}

params <- extract_parameters(file)
cat(sprintf("Processing file: %s\n", file))

# Load data
if (task_id > length(files)) {
  cat("Task ID out of range, terminating execution.\n")
  quit()
}
file <- files[task_id]
params <- extract_parameters(file)
cat(sprintf("Processing file: %s\n", file))

datos_nivelados <- readRDS(file)

results <- list()

# Iterate over all weight combinations
for (i in seq_along(pesos)) {
  w1 <- pesos[i]
  w2 <- 1 - w1
  
  set.seed(params$seed)  # Ensure reproducibility for each run
  
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
  
  results[[i]] <- data.frame(
    archivo = params$filename,
    distribution = params$distribution,
    emp_type = params$emp_type,
    grupos = params$groups,
    students_per_group = params$students_per_group,
    cursos = params$courses,
    alumnos_totales = params$alumnos_totales,
    mu_target = params$mu_target,
    r = params$r,
    p = params$p,
    semilla = params$seed,
    w1 = w1, w2 = w2,
    fitness_inicial = calcular_fitness_normalizado(datos_nivelados, w1, w2),
    fitness_final = ls_result$fitness,
    n_componentes_inicial = res_inicial$n_components,
    n_componentes_final = res_final_ls$n_components,
    varianza_inicial = stats::var(res_inicial$sizes),
    varianza_final = stats::var(res_final_ls$sizes)
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
