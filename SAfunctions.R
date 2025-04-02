# Load required libraries
library(dplyr)
library(tidyr)
library(readr)
library(purrr)
library(stringr)
library(igraph)
library(tidygraph)

##############################
# Auxiliary functions
##############################

# Function to build a graph from a data frame with columns:
# (student_id, family_id, course, group, node = paste(course, group)).
# Returns the graph, number of components, and the size of each component.

construir_grafo_y_componentes <- function(asignaciones_df) {
  # Create edges connecting students from the same family
  edges <- asignaciones_df %>%
    group_by(family_id) %>%
    filter(n() > 1) %>%
    summarise(
      combinations = list(as.data.frame(t(combn(student_id, 2)))),
      .groups = "drop"
    ) %>%
    unnest(combinations) %>%
    rename(student_id1 = V1, student_id2 = V2) %>%
    inner_join(asignaciones_df, by = c("student_id1" = "student_id")) %>%
    inner_join(asignaciones_df, by = c("student_id2" = "student_id"),
               suffix = c("_src", "_dst")) %>%
    transmute(from = node_src, to = node_dst)
  
  # Create node table
  nodes <- asignaciones_df %>%
    distinct(node, course, group)
  
  # Build graph
  g <- graph_from_data_frame(edges, vertices = nodes, directed = FALSE)
  
  # Compute connected components
  comp <- components(g)
  num_comp <- comp$no
  sizes <- comp$csize
  
  return(list(graph = g, n_components = num_comp, sizes = sizes))
}

# Multi-objective fitness function
# We want to:  fitness = w1 * n_components - w2 * var_size

calcular_fitness_normalizado <- function(asignaciones_df, w1 = 1, w2 = 1) {
  res <- construir_grafo_y_componentes(asignaciones_df)
  
  n_comp <- res$n_components
  var_comp_size <- var(res$sizes)
  if (is.na(var_comp_size)) var_comp_size <- 0
  
  # Total number of nodes = groups x courses (or nrow(nodes) if calculated directly)
  n <- vcount(res$graph)
  
  # Theoretical maximum variance
  if (n > 2) {
    var_max <- ((n - 2)/2)^2
  } else {
    var_max <- 0
  }
  
  # Normalize in [0, 1]
  n_comp_norm <- n_comp / n
  var_comp_norm <- if (var_max > 0) (var_comp_size / var_max) else 0
  
  # Normalized fitness
  fit <- w1 * n_comp_norm - w2 * var_comp_norm
  return(fit)
}

# Neighbor solution generator:
# Swaps the group between two students who:
# - Are in the same course
# - Belong to different groups

generar_vecino <- function(asignaciones_df) {
  new_df <- asignaciones_df
  cursos_disponibles <- unique(new_df$course)
  curso_elegido <- sample(cursos_disponibles, 1)
  df_curso <- new_df %>% filter(course == curso_elegido)
  
  grupos_curso <- df_curso %>% group_split(group)
  if (length(grupos_curso) < 2) return(new_df)
  
  idx_g1 <- sample(seq_along(grupos_curso), 1)
  idx_g2 <- sample(setdiff(seq_along(grupos_curso), idx_g1), 1)
  
  sub_g1 <- grupos_curso[[idx_g1]]
  sub_g2 <- grupos_curso[[idx_g2]]
  
  if (nrow(sub_g1) == 0 || nrow(sub_g2) == 0) return(new_df)
  
  alumno1 <- sub_g1 %>% sample_n(1)
  alumno2 <- sub_g2 %>% sample_n(1)
  
  g1_temp <- alumno1$group
  g2_temp <- alumno2$group
  
  new_df <- new_df %>%
    mutate(
      group = case_when(
        student_id == alumno1$student_id ~ g2_temp,
        student_id == alumno2$student_id ~ g1_temp,
        TRUE ~ group
      )
    ) %>%
    mutate(node = paste(course, group, sep = "-"))
  
  return(new_df)
}

##############################
# Simulated Annealing
##############################

simulated_annealing <- function(
    datos_inicial,
    w1 = 1,
    w2 = 1,
    max_iter = 5000
) {
  current_solution <- datos_inicial
  current_fitness <- calcular_fitness_normalizado(current_solution, w1, w2)
  
  best_solution <- current_solution
  best_fitness <- current_fitness
  
  T0 <- 0.3 * current_fitness
  if (T0 <= 0) T0 <- 0.3
  
  Tf <- 0.01
  alpha <- (Tf / T0)^(1 / max_iter)
  T <- T0
  
  for (iter in seq_len(max_iter)) {
    neighbor_solution <- generar_vecino(current_solution)
    neighbor_fitness <- calcular_fitness_normalizado(neighbor_solution, w1, w2)
    
    delta <- neighbor_fitness - current_fitness
    
    if (delta >= 0 || runif(1) < exp(delta / T)) {
      current_solution <- neighbor_solution
      current_fitness <- neighbor_fitness
    }
    
    if (current_fitness > best_fitness) {
      best_fitness <- current_fitness
      best_solution <- current_solution
    }
    
    T <- T * alpha
    
    if (iter %% 1000 == 0) {
      cat(sprintf("Iter: %d, Current fitness: %.3f, Best fitness: %.3f, T=%.4f\n",
                  iter, current_fitness, best_fitness, T))
    }
  }
  
  list(
    best_solution = best_solution,
    best_fitness = best_fitness
  )
}

##############################
# Local Search: First Improvement
##############################

# Generate ALL valid student swap pairs (same course, different group)
generar_vecinos_todos <- function(asignaciones_df) {
  mini_df <- asignaciones_df %>% select(student_id, course, group)
  
  pairs <- mini_df %>%
    inner_join(mini_df, by = "course", suffix = c("_i", "_j")) %>%
    filter(student_id_i < student_id_j) %>%
    filter(group_i != group_j) %>%
    sample_frac(1)
  
  return(pairs)
}

# Apply a group swap between two students
aplicar_swap <- function(asignaciones_df, student_i, student_j) {
  alumno_i <- asignaciones_df %>% filter(student_id == student_i)
  alumno_j <- asignaciones_df %>% filter(student_id == student_j)
  
  if (nrow(alumno_i) == 0 || nrow(alumno_j) == 0) return(asignaciones_df)
  
  g1 <- alumno_i$group
  g2 <- alumno_j$group
  
  df_swap <- asignaciones_df %>%
    mutate(
      group = case_when(
        student_id == student_i ~ g2,
        student_id == student_j ~ g1,
        TRUE ~ group
      )
    ) %>%
    mutate(node = paste(course, group, sep = "-"))
  
  return(df_swap)
}

# First improvement local search using a percentage of the total possible swaps
local_search_first_improvement <- function(asignaciones_df, w1 = 1, w2 = 1, perc = 0.01) {
  current_solution <- asignaciones_df
  current_fitness <- calcular_fitness_normalizado(current_solution, w1, w2)
  
  improvement <- TRUE
  iteration_count <- 0
  
  while (improvement) {
    improvement <- FALSE
    pairs <- generar_vecinos_todos(current_solution)
    total_pairs <- nrow(pairs)
    max_moves <- floor(perc * total_pairs)
    
    if (max_moves < 1) break
    
    for (row_idx in seq_len(max_moves)) {
      i <- pairs$student_id_i[row_idx]
      j <- pairs$student_id_j[row_idx]
      
      neighbor_sol <- aplicar_swap(current_solution, i, j)
      neighbor_fit <- calcular_fitness_normalizado(neighbor_sol, w1, w2)
      iteration_count <- iteration_count + 1
      
      if (neighbor_fit > current_fitness) {
        current_solution <- neighbor_sol
        current_fitness <- neighbor_fit
        improvement <- TRUE
        break
      }
    }
  }
  
  list(
    solution = current_solution,
    fitness  = current_fitness,
    iterations = iteration_count
  )
}
