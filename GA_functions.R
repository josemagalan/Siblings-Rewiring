library(dplyr)
library(tidyr)
library(readr)
library(purrr)
library(stringr)
library(igraph)
library(tidygraph)

# Evaluate a solution: compute number of components and size variance
evaluar_solucion <- function(asignaciones_df) {
  res <- construir_grafo_y_componentes(asignaciones_df)
  
  n_comp <- res$n_components
  var_comp_size <- var(res$sizes)
  
  if (is.na(var_comp_size)) var_comp_size <- 0
  
  tibble(n_componentes = n_comp, varianza_tamanio = var_comp_size)
}

# Build a graph from assignments based on family relationships
construir_grafo_y_componentes <- function(asignaciones_df) {
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
  
  nodes <- asignaciones_df %>%
    distinct(node, course, group)
  
  g <- graph_from_data_frame(edges, vertices = nodes, directed = FALSE)
  
  comp <- components(g)
  num_comp <- comp$no
  sizes <- comp$csize
  
  return(list(graph = g, n_components = num_comp, sizes = sizes))
}

# Generate an initial random solution based on family order and cyclic assignment
generar_solucion_aleatoria <- function(datos, num_grupos, max_grupo_size, semilla = NULL, debug = FALSE) {
  if (!is.null(semilla)) set.seed(semilla)
  
  families <- datos %>% 
    group_by(family_id) %>% 
    summarise(n_family = n(), .groups = "drop") %>% 
    mutate(rand = runif(n())) %>% 
    arrange(desc(n_family), rand) %>% 
    mutate(grupo_asignado = rep(1:num_grupos, length.out = n()))
  
  sol <- datos %>% 
    left_join(families %>% select(family_id, grupo_asignado), by = "family_id")
  sol$group <- NA_integer_
  
  courses <- sort(unique(sol$course))
  avail <- expand.grid(course = courses, group = 1:num_grupos, stringsAsFactors = FALSE) %>%
    mutate(disponible = max_grupo_size)
  
  for (i in seq_len(nrow(families))) {
    fam_id <- families$family_id[i]
    target <- families$grupo_asignado[i]
    
    cursos_fam <- unique(sol$course[sol$family_id == fam_id])
    for (c in cursos_fam) {
      idx <- which(sol$family_id == fam_id & sol$course == c)
      for (j in idx) {
        row_target <- which(avail$course == c & avail$group == target)
        if (avail$disponible[row_target] > 0) {
          sol$group[j] <- target
          avail$disponible[row_target] <- avail$disponible[row_target] - 1
        } else {
          candidatos <- avail %>% filter(course == c, disponible > 0)
          if(nrow(candidatos) == 0) {
            stop("No available capacity in course ", c)
          }
          elegido <- candidatos$group[which.max(candidatos$disponible)]
          sol$group[j] <- elegido
          row_elegido <- which(avail$course == c & avail$group == elegido)
          avail$disponible[row_elegido] <- avail$disponible[row_elegido] - 1
        }
      }
    }
  }
  
  sol <- sol %>% mutate(node = paste(course, group, sep = "-"))
  return(sol)
}

# Alternative initialization heuristic: prioritizes keeping families together
generar_solucion_alternativa <- function(datos, num_grupos, max_grupo_size, semilla = NULL, debug = FALSE) {
  if (!is.null(semilla)) set.seed(semilla)
  
  sol <- datos
  sol$group <- NA_integer_
  
  cursos <- sort(unique(sol$course))
  avail <- expand.grid(course = cursos, group = 1:num_grupos, stringsAsFactors = FALSE) %>%
    mutate(disponible = max_grupo_size)
  
  grupo_actual <- sample(1:num_grupos, 1)
  
  familias <- datos %>%
    group_by(family_id) %>%
    summarise(n_family = n(), .groups = "drop") %>%
    filter(n_family > 1) %>%
    mutate(rand = runif(n())) %>%
    arrange(rand)
  
  for (fam in familias$family_id) {
    cursos_fam <- unique(sol$course[sol$family_id == fam])
    for (curso in cursos_fam) {
      idx_alumnos <- which(sol$family_id == fam & sol$course == curso)
      
      if (avail$disponible[avail$course == curso & avail$group == grupo_actual] >= length(idx_alumnos)) {
        sol$group[idx_alumnos] <- grupo_actual
        avail$disponible[avail$course == curso & avail$group == grupo_actual] <-
          avail$disponible[avail$course == curso & avail$group == grupo_actual] - length(idx_alumnos)
      } else {
        grupos_alternativos <- avail %>% filter(course == curso & disponible >= length(idx_alumnos))
        if (nrow(grupos_alternativos) > 0) {
          nuevo_grupo <- sample(grupos_alternativos$group, 1)
          sol$group[idx_alumnos] <- nuevo_grupo
          avail$disponible[avail$course == curso & avail$group == nuevo_grupo] <-
            avail$disponible[avail$course == curso & avail$group == nuevo_grupo] - length(idx_alumnos)
          grupo_actual <- nuevo_grupo
        } else {
          for (idx in idx_alumnos) {
            candidatos <- avail %>% filter(course == curso, disponible > 0)
            if (nrow(candidatos) == 0) stop("No available space")
            
            elegido <- sample(candidatos$group, 1)
            sol$group[idx] <- elegido
            avail$disponible[avail$course == curso & avail$group == elegido] <-
              avail$disponible[avail$course == curso & avail$group == elegido] - 1
          }
        }
      }
      
      if (any(avail$disponible <= 0)) {
        posibles_grupos <- avail %>% filter(disponible > 0) %>% pull(group)
        if (length(posibles_grupos) > 0) {
          grupo_actual <- sample(posibles_grupos, 1)
        }
      }
    }
  }
  
  alumnos_individuales <- which(is.na(sol$group))
  for (idx in alumnos_individuales) {
    curso_actual <- sol$course[idx]
    candidatos <- avail %>% filter(course == curso_actual, disponible > 0)
    if (nrow(candidatos) == 0) stop("No space for individual students")
    
    elegido <- sample(candidatos$group, 1)
    sol$group[idx] <- elegido
    avail$disponible[avail$course == curso_actual & avail$group == elegido] <-
      avail$disponible[avail$course == curso_actual & avail$group == elegido] - 1
  }
  
  sol <- sol %>% mutate(node = paste(course, group, sep = "-"))
  return(sol)
}


# Swap mutation: randomly swaps students between groups within the same course
mutar_solucion_swap <- function(solucion, prob_swap = 0.1) {
  sol_mutada <- solucion
  cursos <- unique(sol_mutada$course)
  
  for (c in cursos) {
    indices <- which(sol_mutada$course == c)
    if (length(indices) > 1 && length(unique(sol_mutada$group[indices])) > 1) {
      num_swaps <- round(prob_swap * length(indices))
      for (k in seq_len(num_swaps)) {
        swap_indices <- sample(indices, 2, replace = FALSE)
        if (sol_mutada$group[swap_indices[1]] != sol_mutada$group[swap_indices[2]]) {
          temp <- sol_mutada$group[swap_indices[1]]
          sol_mutada$group[swap_indices[1]] <- sol_mutada$group[swap_indices[2]]
          sol_mutada$group[swap_indices[2]] <- temp
          
          sol_mutada$node[swap_indices[1]] <- paste(sol_mutada$course[swap_indices[1]], sol_mutada$group[swap_indices[1]], sep = "-")
          sol_mutada$node[swap_indices[2]] <- paste(sol_mutada$course[swap_indices[2]], sol_mutada$group[swap_indices[2]], sep = "-")
        }
      }
    }
  }
  
  return(sol_mutada)
}

# Crossover function: combines two parent solutions at family level
cruzar_soluciones <- function(sol_padre1, sol_padre2, max_grupo_size, num_grupos, prob_familia = 0.5) {
  sol_hijo <- sol_padre1
  
  familias <- unique(sol_padre1$family_id)
  for (fam in familias) {
    if (runif(1) < prob_familia) {
      idx <- which(sol_hijo$family_id == fam)
      sol_hijo[idx, "group"] <- sol_padre2[idx, "group"]
      sol_hijo[idx, "node"] <- paste(sol_hijo$course[idx], sol_padre2[idx, "group"], sep = "-")
    }
  }
  
  # Repair phase: resolve any overfilled group
  cursos <- unique(sol_hijo$course)
  for (c in cursos) {
    hay_sobrecupo <- TRUE
    while (hay_sobrecupo) {
      hay_sobrecupo <- FALSE
      for (g in 1:num_grupos) {
        alumnos_grupo <- which(sol_hijo$course == c & sol_hijo$group == g)
        if (length(alumnos_grupo) > max_grupo_size) {
          hay_sobrecupo <- TRUE
          indice_sobrecupo <- sample(alumnos_grupo, 1)
          grupos_alternativos <- setdiff(1:num_grupos, g)
          disponibles <- grupos_alternativos[sapply(grupos_alternativos, function(g2) {
            sum(sol_hijo$course == c & sol_hijo$group == g2) < max_grupo_size
          })]
          
          if (length(disponibles) == 0) {
            stop(paste("No available capacity to reassign student in course", c))
          }
          
          nuevo_grupo <- sample(disponibles, 1)
          sol_hijo$group[indice_sobrecupo] <- nuevo_grupo
          sol_hijo$node[indice_sobrecupo] <- paste(c, nuevo_grupo, sep = "-")
          break
        }
      }
    }
  }
  
  sol_hijo$node <- paste(sol_hijo$course, sol_hijo$group, sep = "-")
  return(sol_hijo)
}

# Fast non-dominated sorting for multi-objective optimization
fast_non_dominated_sort <- function(objs) {
  n <- nrow(objs)
  dominated_count <- integer(n)
  dominates_list <- vector("list", n)
  
  for (i in seq_len(n)) {
    for (j in seq_len(n)) {
      if (i != j) {
        cond1 <- objs$n_componentes[i] >= objs$n_componentes[j]
        cond2 <- objs$varianza_tamanio[i] <= objs$varianza_tamanio[j]
        strictly_better <- (objs$n_componentes[i] > objs$n_componentes[j]) ||
          (objs$varianza_tamanio[i] < objs$varianza_tamanio[j])
        
        if (cond1 && cond2 && strictly_better) {
          dominates_list[[i]] <- c(dominates_list[[i]], j)
        }
      }
    }
  }
  
  for (j in seq_len(n)) {
    dominated_count[j] <- sum(sapply(seq_len(n), function(i) j %in% dominates_list[[i]]))
  }
  
  fronts <- list()
  current_front <- which(dominated_count == 0)
  rank <- 1
  
  while (length(current_front) > 0) {
    fronts[[rank]] <- current_front
    next_front <- c()
    for (p in current_front) {
      for (q in dominates_list[[p]]) {
        dominated_count[q] <- dominated_count[q] - 1
        if (dominated_count[q] == 0) {
          next_front <- c(next_front, q)
        }
      }
    }
    rank <- rank + 1
    current_front <- next_front
  }
  
  return(fronts)
}

# Crowding distance for diversity preservation in NSGA-II
crowding_distance <- function(front_df) {
  n <- nrow(front_df)
  if (n <= 2) return(rep(Inf, n))
  
  f1 <- front_df$n_componentes
  f2 <- front_df$varianza_tamanio
  
  f1_min <- min(f1); f1_max <- max(f1)
  f2_min <- min(f2); f2_max <- max(f2)
  
  idx_f1 <- order(f1)
  dist <- rep(0, n)
  dist[idx_f1[1]] <- Inf
  dist[idx_f1[n]] <- Inf
  for (k in 2:(n-1)) {
    dist[idx_f1[k]] <- dist[idx_f1[k]] + 
      (f1[idx_f1[k+1]] - f1[idx_f1[k-1]]) / (f1_max - f1_min + 1e-9)
  }
  
  idx_f2 <- order(f2)
  dist[idx_f2[1]] <- Inf
  dist[idx_f2[n]] <- Inf
  for (k in 2:(n-1)) {
    dist[idx_f2[k]] <- dist[idx_f2[k]] + 
      (f2[idx_f2[k+1]] - f2[idx_f2[k-1]]) / (f2_max - f2_min + 1e-9)
  }
  
  return(dist)
}


# Main NSGA-II function for multi-objective optimization (maximize n_componentes, minimize varianza_tamanio)
nsga2 <- function(datos, 
                  pop_size = 20, 
                  num_generations = 10,
                  num_grupos, 
                  max_grupo_size,
                  p_mut = 0.1,       # mutation probability
                  p_familia = 0.5,   # probability of inheriting a family block during crossover
                  p_cross = 0.3      # crossover probability
) {
  # 1) Generate initial mixed population: half random, half heuristic-based
  poblacion <- c(
    replicate(pop_size / 2, 
              generar_solucion_aleatoria(datos, num_grupos, max_grupo_size), 
              simplify = FALSE),
    replicate(pop_size / 2, 
              generar_solucion_alternativa(datos, num_grupos, max_grupo_size), 
              simplify = FALSE)
  )
  
  # 2) Evaluate population
  evals <- do.call(rbind, lapply(poblacion, evaluar_solucion))
  
  for (gen in seq_len(num_generations)) {
    cat("\n=== Generation", gen, "===\n")
    
    # 3) Offspring creation
    # 3.1) Non-dominated sorting
    fronts <- fast_non_dominated_sort(evals)
    
    # 3.2) Compute rank and crowding distance
    rank_vector <- rep(NA, pop_size)
    crowd_vector <- rep(NA, pop_size)
    
    for (i in seq_along(fronts)) {
      f <- fronts[[i]]
      rank_vector[f] <- i
      cd <- crowding_distance(evals[f,])
      crowd_vector[f] <- cd
    }
    
    # 3.3) Binary tournament selection
    seleccionados <- vector("list", pop_size)
    for (i in seq_len(pop_size)) {
      idx1 <- sample(seq_len(pop_size), 1)
      idx2 <- sample(seq_len(pop_size), 1)
      
      ganador <- idx1
      if ((rank_vector[idx2] < rank_vector[idx1]) ||
          (rank_vector[idx2] == rank_vector[idx1] && crowd_vector[idx2] > crowd_vector[idx1])) {
        ganador <- idx2
      }
      seleccionados[[i]] <- poblacion[[ganador]]
    }
    
    # 3.4) Generate offspring via crossover and mutation
    hijos <- vector("list", pop_size)
    for (i in seq(1, pop_size, by = 2)) {
      p1 <- seleccionados[[i]]
      p2 <- if (i < pop_size) seleccionados[[i+1]] else seleccionados[[1]]
      
      if (runif(1) < p_cross) {
        hijo1 <- cruzar_soluciones(p1, p2, max_grupo_size, num_grupos, prob_familia = p_familia)
        hijo2 <- cruzar_soluciones(p2, p1, max_grupo_size, num_grupos, prob_familia = p_familia)
      } else {
        hijo1 <- p1
        hijo2 <- p2
      }
      
      hijo1 <- mutar_solucion_swap(hijo1, prob_swap = p_mut)
      hijo2 <- mutar_solucion_swap(hijo2, prob_swap = p_mut)
      
      hijos[[i]] <- hijo1
      if (i < pop_size) {
        hijos[[i+1]] <- hijo2
      }
    }
    
    # 3.5) Evaluate offspring
    evals_hijos <- do.call(rbind, lapply(hijos, evaluar_solucion))
    
    # 4) Combine parents and offspring
    poblacion_combinada <- c(poblacion, hijos)
    evals_combinada <- rbind(evals, evals_hijos)
    
    # 4.1) Non-dominated sorting of combined population
    fronts_comb <- fast_non_dominated_sort(evals_combinada)
    
    # 4.2) Build new population of fixed size (pop_size)
    nueva_poblacion <- list()
    nueva_evals <- data.frame(n_componentes = numeric(0), varianza_tamanio = numeric(0))
    cont <- 1
    
    for (f_idx in seq_along(fronts_comb)) {
      f <- fronts_comb[[f_idx]]
      if (length(f) + (cont - 1) <= pop_size) {
        for (idx in f) {
          nueva_poblacion[[cont]] <- poblacion_combinada[[idx]]
          nueva_evals <- rbind(nueva_evals, evals_combinada[idx, ])
          cont <- cont + 1
        }
      } else {
        cd <- crowding_distance(evals_combinada[f, ])
        f_order <- f[order(cd, decreasing = TRUE)]
        slots <- pop_size - (cont - 1)
        f_sel <- f_order[seq_len(slots)]
        
        for (idx in f_sel) {
          nueva_poblacion[[cont]] <- poblacion_combinada[[idx]]
          nueva_evals <- rbind(nueva_evals, evals_combinada[idx, ])
          cont <- cont + 1
        }
        break
      }
    }
    
    # Update population and evaluations for next generation
    poblacion <- nueva_poblacion
    evals <- nueva_evals
    
    # Optional: print best front info
    cat("Best front (indices in combined population):", fronts_comb[[1]], "\n")
    cat("Size of best front:", length(fronts_comb[[1]]), "\n")
  }
  
  # Final output: return final population and their evaluations
  return(list(poblacion = poblacion, evaluaciones = evals))
}

