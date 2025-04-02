library(dplyr)
library(tidyr)
library(readr)
library(purrr)
library(stringr)
library(igraph)
library(tidygraph)

##############################
# Funciones auxiliares
##############################

# Función para construir el grafo a partir de un data frame con columnas:
# (student_id, family_id, course, group, node = paste(course, group)).
# Retorna el grafo y también un vector con los tamaños de cada componente.

construir_grafo_y_componentes <- function(asignaciones_df) {
  # Creamos los edges considerando que alumnos de la misma familia están conectados
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
  
  # Creamos la tabla de nodos
  nodes <- asignaciones_df %>%
    distinct(node, course, group)
  
  # Construimos el grafo
  g <- graph_from_data_frame(edges, vertices = nodes, directed = FALSE)
  
  # Obtenemos los componentes
  comp <- components(g)
  num_comp <- comp$no
  # Tamaños de cada componente
  sizes <- comp$csize
  
  return(list(graph = g, n_components = num_comp, sizes = sizes))
}

# Función de fitness multicriterio
# Queremos:  fitness = w1 * n_components - w2 * var_size
calcular_fitness_normalizado <- function(asignaciones_df, w1 = 1, w2 = 1) {
  res <- construir_grafo_y_componentes(asignaciones_df)
  
  n_comp <- res$n_components
  var_comp_size <- var(res$sizes)
  if (is.na(var_comp_size)) var_comp_size <- 0
  
  # Número total de nodos = groups x courses
  #   (O podría ser nrow(nodes) si lo calculas directamente)
  n <- vcount(res$graph)  # si 'graph' tiene todos los nodos
  # O si lo prefieres, n <- groups * courses
  
  # Varianza máxima teórica
  if (n > 2) {
    var_max <- ((n - 2)/2)^2
  } else {
    # Si n <= 2, la varianza máxima es 0 (no se puede formar 2 componentes distintos de tamaños n-1 y 1)
    var_max <- 0
  }
  
  # Normalizaciones en [0, 1]
  n_comp_norm <- n_comp / n
  var_comp_norm <- if (var_max > 0) (var_comp_size / var_max) else 0
  
  # Fitness normalizado
  fit <- w1 * n_comp_norm - w2 * var_comp_norm
  return(fit)
}

# Generador de soluciones vecinas:
# Cambiamos el grupo entre dos alumnos que:
#   - Estén en el mismo curso
#   - Pertenecen a diferentes grupos
generar_vecino <- function(asignaciones_df) {
  # Copia para no modificar el original directamente
  new_df <- asignaciones_df
  
  # 1. Seleccionamos un curso aleatoriamente
  cursos_disponibles <- unique(new_df$course)
  curso_elegido <- sample(cursos_disponibles, 1)
  
  # 2. Filtramos alumnos de ese curso
  df_curso <- new_df %>% filter(course == curso_elegido)
  # Necesitamos al menos 2 personas con grupos distintos para poder intercambiar
  # (En la práctica, convendría más control, pero lo haremos sencillo)
  
  # 3. Seleccionamos dos alumnos de grupos distintos
  # Dividimos en subgrupos por 'group'
  grupos_curso <- df_curso %>% group_split(group)
  
  # Si hay pocos grupos o no existe la posibilidad de swap, simplemente devolvemos la misma solución
  if (length(grupos_curso) < 2) {
    return(new_df)  # No hay cambio posible
  }
  
  # Elegimos dos alumnos al azar de dos grupos distintos
  # (forma sencilla: elegimos un grupo al azar y otro distinto)
  idx_g1 <- sample(seq_along(grupos_curso), 1)
  idx_g2 <- sample(setdiff(seq_along(grupos_curso), idx_g1), 1)
  
  sub_g1 <- grupos_curso[[idx_g1]]
  sub_g2 <- grupos_curso[[idx_g2]]
  
  # Si alguno no tiene alumnos (podría ser un corner case), devolvemos la misma
  if (nrow(sub_g1) == 0 || nrow(sub_g2) == 0) {
    return(new_df)
  }
  
  alumno1 <- sub_g1 %>% sample_n(1)
  alumno2 <- sub_g2 %>% sample_n(1)
  
  # 4. Intercambiamos sus grupos
  #   - Los dos tienen el mismo 'course'
  #   - Cambiamos la columna 'group' y el 'node'
  g1_temp <- alumno1$group
  g2_temp <- alumno2$group
  
  # Modificamos en new_df
  # Para ello, localizamos las filas exactas por student_id
  new_df <- new_df %>%
    mutate(
      group = case_when(
        student_id == alumno1$student_id ~ g2_temp,
        student_id == alumno2$student_id ~ g1_temp,
        TRUE ~ group
      )
    ) %>%
    mutate(node = paste(course, group, sep = "-"))  # Actualizamos node
  
  return(new_df)
}

##############################
# Recocido Simulado
##############################

simulated_annealing <- function(
    datos_inicial,
    w1 = 1,
    w2 = 1,
    max_iter = 5000
) {
  # 1. Calculamos fitness inicial
  current_solution <- datos_inicial
  current_fitness <- calcular_fitness_normalizado(current_solution, w1, w2)
  
  # Guardamos la mejor solución
  best_solution <- current_solution
  best_fitness <- current_fitness
  
  # 2. Definimos temperatura inicial
  T0 <- 0.3 * current_fitness
  # Si la solución inicial tiene fitness 0 o negativo, evitamos T0 = 0
  if (T0 <= 0) {
    T0 <- 0.3  # fallback para no tener T=0
  }
  
  # 3. Temperatura final y factor de enfriamiento
  Tf <- 0.01
  alpha <- (Tf / T0)^(1 / max_iter)
  
  # 4. Bucle principal
  T <- T0
  for (iter in seq_len(max_iter)) {
    # Generamos un vecino
    neighbor_solution <- generar_vecino(current_solution)
    neighbor_fitness <- calcular_fitness_normalizado(neighbor_solution, w1, w2)
    
    # Delta de fitness
    delta <- neighbor_fitness - current_fitness
    
    # Criterio de aceptación
    if (delta >= 0) {
      # Aceptamos siempre si mejora
      current_solution <- neighbor_solution
      current_fitness <- neighbor_fitness
    } else {
      # Aceptación probabilística
      if (runif(1) < exp(delta / T)) {
        current_solution <- neighbor_solution
        current_fitness <- neighbor_fitness
      }
    }
    
    # Actualizamos la mejor solución
    if (current_fitness > best_fitness) {
      best_fitness <- current_fitness
      best_solution <- current_solution
    }
    
    # Enfriamos la temperatura
    T <- T * alpha
    
    # (Opcional) puedes imprimir cada cierto número de iteraciones
    if (iter %% 1000 == 0) {
      cat(sprintf("Iter: %d, Current fitness: %.3f, Best fitness: %.3f, T=%.4f\n",
                  iter, current_fitness, best_fitness, T))
    }
  }
  
  # Retornamos la mejor solución y su fitness
  list(
    best_solution = best_solution,
    best_fitness = best_fitness
  )
}




# Genera TODOS los pares (i, j) que pueden intercambiarse (mismo curso, grupo distinto)
generar_vecinos_todos <- function(asignaciones_df) {
  mini_df <- asignaciones_df %>% select(student_id, course, group)
  
  pairs <- mini_df %>%
    inner_join(mini_df, by = "course", suffix = c("_i", "_j")) %>%
    filter(student_id_i < student_id_j) %>%
    filter(group_i != group_j)
  
  # Mezclamos aleatoriamente para no recorrer siempre en el mismo orden
  pairs <- pairs %>% sample_frac(1)
  pairs
}

# Aplica un swap entre dos alumnos dados
aplicar_swap <- function(asignaciones_df, student_i, student_j) {
  alumno_i <- asignaciones_df %>% filter(student_id == student_i)
  alumno_j <- asignaciones_df %>% filter(student_id == student_j)
  
  if (nrow(alumno_i) == 0 || nrow(alumno_j) == 0) {
    return(asignaciones_df)
  }
  
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
  
  df_swap
}



local_search_first_improvement <- function(asignaciones_df, w1 = 1, w2 = 1, 
                                           perc = 0.01) {
  current_solution <- asignaciones_df
  current_fitness <- calcular_fitness_normalizado(current_solution, w1, w2)
  
  improvement <- TRUE
  iteration_count <- 0
  
  while (improvement) {
    improvement <- FALSE
    
    # Generamos todos los vecinos posibles con la SOLUCIÓN ACTUAL
    pairs <- generar_vecinos_todos(current_solution)
    total_pairs <- nrow(pairs)
    
    # Definimos cuántos pares vamos a evaluar en este ciclo
    max_moves <- floor(perc * total_pairs)
    
    # Si max_moves < 1, no vale la pena avanzar
    if (max_moves < 1) {
      break
    }
    
    # Recorremos hasta max_moves o hasta encontrar la primera mejora
    for (row_idx in seq_len(max_moves)) {
      i <- pairs$student_id_i[row_idx]
      j <- pairs$student_id_j[row_idx]
      
      # Aplicamos swap
      neighbor_sol <- aplicar_swap(current_solution, i, j)
      neighbor_fit <- calcular_fitness_normalizado(neighbor_sol, w1, w2)
      iteration_count <- iteration_count + 1
      
      # Chequeamos mejora
      if (neighbor_fit > current_fitness) {
        current_solution <- neighbor_sol
        current_fitness <- neighbor_fit
        improvement <- TRUE
        break  # Salimos del for, volvemos a while
      }
    }
  }
  
  list(
    solution = current_solution,
    fitness  = current_fitness,
    iterations = iteration_count
  )
}




