library(dplyr)
library(tidyr)
library(readr)
library(purrr)
library(stringr)
library(igraph)
library(tidygraph)




evaluar_solucion <- function(asignaciones_df) {
  # Construimos el grafo
  res <- construir_grafo_y_componentes(asignaciones_df)
  
  # Objetivos
  n_comp <- res$n_components
  var_comp_size <- var(res$sizes)
  
  # Evitar NA en la varianza
  if (is.na(var_comp_size)) var_comp_size <- 0
  
  # Retornamos ambos valores (sin agregarlos en una sola métrica)
  tibble(n_componentes = n_comp, varianza_tamanio = var_comp_size)
}

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



generar_solucion_aleatoria <- function(datos, num_grupos, max_grupo_size, semilla = NULL, debug = FALSE) {
  if (!is.null(semilla)) set.seed(semilla)
  
  # 1. Ordenar familias por tamaño (rompiendo empates) y asignar grupo objetivo de forma cíclica
  families <- datos %>% 
    group_by(family_id) %>% 
    summarise(n_family = n(), .groups = "drop") %>% 
    mutate(rand = runif(n())) %>% 
    arrange(desc(n_family), rand) %>% 
    mutate(grupo_asignado = rep(1:num_grupos, length.out = n()))
  
  # Incorporar el grupo objetivo a cada alumno
  sol <- datos %>% 
    left_join(families %>% select(family_id, grupo_asignado), by = "family_id")
  # Inicializamos la columna group (que asignaremos individualmente)
  sol$group <- NA_integer_
  
  # 2. Crear tabla de disponibilidad: para cada par (curso, grupo), el cupo inicial es max_grupo_size.
  courses <- sort(unique(sol$course))
  avail <- expand.grid(course = courses, group = 1:num_grupos, stringsAsFactors = FALSE) %>%
    mutate(disponible = max_grupo_size)
  
  if (debug) {
    cat("Estado inicial de disponibilidad:\n")
    print(avail)
  }
  
  # 3. Procesar cada familia (en el orden determinado)
  for (i in seq_len(nrow(families))) {
    fam_id <- families$family_id[i]
    target <- families$grupo_asignado[i]
    
    if (debug) {
      cat("\nProcesando familia:", fam_id, "con grupo objetivo:", target, "\n")
    }
    
    # Para cada curso en el que aparece la familia
    cursos_fam <- unique(sol$course[sol$family_id == fam_id])
    for (c in cursos_fam) {
      # Obtener índices de los alumnos de la familia en el curso c
      idx <- which(sol$family_id == fam_id & sol$course == c)
      
      if (debug) {
        cat("  Curso:", c, "- disponibilidad antes de asignar:\n")
        print(avail[avail$course == c, ])
      }
      
      # Asignar cada alumno individualmente
      for (j in idx) {
        # Verificar disponibilidad en el grupo objetivo
        row_target <- which(avail$course == c & avail$group == target)
        if (avail$disponible[row_target] > 0) {
          sol$group[j] <- target
          avail$disponible[row_target] <- avail$disponible[row_target] - 1
          if (debug) {
            cat("    Alumno", sol$student_id[j], "asignado al grupo objetivo", target,
                "- nueva disponibilidad:", avail$disponible[row_target], "\n")
          }
        } else {
          # Si el grupo objetivo está lleno, se elige el grupo del mismo curso que tenga mayor disponibilidad.
          candidatos <- avail %>% filter(course == c, disponible > 0)
          if(nrow(candidatos) == 0) {
            stop("No hay capacidad disponible en el curso ", c)
          }
          # Seleccionar el grupo con mayor disponibilidad (en caso de empate, se elige el primero)
          elegido <- candidatos$group[which.max(candidatos$disponible)]
          sol$group[j] <- elegido
          row_elegido <- which(avail$course == c & avail$group == elegido)
          avail$disponible[row_elegido] <- avail$disponible[row_elegido] - 1
          if (debug) {
            cat("    Alumno", sol$student_id[j], "asignado al grupo alternativo", elegido,
                "- nueva disponibilidad:", avail$disponible[row_elegido], "\n")
          }
        }
      }
      
      if (debug) {
        cat("  Curso:", c, "- disponibilidad luego de asignar:\n")
        print(avail[avail$course == c, ])
      }
    }
    
    if (debug) {
      cat("Estado global de disponibilidad tras procesar familia", fam_id, ":\n")
      print(avail)
    }
  }
  
  # 4. Actualizar la columna 'node' con el par course-group
  sol <- sol %>% mutate(node = paste(course, group, sep = "-"))
  
  return(sol)
}


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
            if (nrow(candidatos) == 0) stop("Sin espacio disponible")
            
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
    if (nrow(candidatos) == 0) stop("Sin espacio disponible para alumnos individuales")
    
    elegido <- sample(candidatos$group, 1)
    sol$group[idx] <- elegido
    avail$disponible[avail$course == curso_actual & avail$group == elegido] <-
      avail$disponible[avail$course == curso_actual & avail$group == elegido] - 1
  }
  
  sol <- sol %>% mutate(node = paste(course, group, sep = "-"))
  
  return(sol)
}





mutar_solucion_swap <- function(solucion, prob_swap = 0.1) {
  sol_mutada <- solucion
  
  # Iteramos por cada curso
  cursos <- unique(sol_mutada$course)
  for (c in cursos) {
    # Índices de alumnos pertenecientes al curso c
    indices <- which(sol_mutada$course == c)
    
    # Solo se puede swapear si existen al menos dos alumnos y hay más de un grupo presente
    if (length(indices) > 1 && length(unique(sol_mutada$group[indices])) > 1) {
      # Determinamos el número de swaps a realizar en este curso
      num_swaps <- round(prob_swap * length(indices))
      
      for (k in seq_len(num_swaps)) {
        # Seleccionamos dos alumnos aleatoriamente
        swap_indices <- sample(indices, 2, replace = FALSE)
        # Solo intercambiamos si pertenecen a grupos distintos
        if (sol_mutada$group[swap_indices[1]] != sol_mutada$group[swap_indices[2]]) {
          # Intercambio de asignación de grupos
          temp <- sol_mutada$group[swap_indices[1]]
          sol_mutada$group[swap_indices[1]] <- sol_mutada$group[swap_indices[2]]
          sol_mutada$group[swap_indices[2]] <- temp
          
          # Actualizamos la columna 'node' con el par course-group actualizado
          sol_mutada$node[swap_indices[1]] <- paste(sol_mutada$course[swap_indices[1]], sol_mutada$group[swap_indices[1]], sep = "-")
          sol_mutada$node[swap_indices[2]] <- paste(sol_mutada$course[swap_indices[2]], sol_mutada$group[swap_indices[2]], sep = "-")
        }
      }
    }
  }
  
  return(sol_mutada)
}

cruzar_soluciones <- function(sol_padre1, sol_padre2, max_grupo_size, num_grupos, prob_familia = 0.5) {
  # Suponemos que ambas soluciones tienen la misma estructura y orden
  sol_hijo <- sol_padre1
  
  # Cruce a nivel de familia: para cada familia, con probabilidad prob_familia se toma
  # la asignación del sol_padre2, y con la complementaria se mantiene la del sol_padre1.
  familias <- unique(sol_padre1$family_id)
  for (fam in familias) {
    if (runif(1) < prob_familia) {
      idx <- which(sol_hijo$family_id == fam)
      sol_hijo[idx, "group"] <- sol_padre2[idx, "group"]
      sol_hijo[idx, "node"] <- paste(sol_hijo$course[idx], sol_padre2[idx, "group"], sep = "-")
    }
  }
  
  # Paso de reparación mejorado: iteramos hasta que ningún grupo exceda la capacidad
  cursos <- unique(sol_hijo$course)
  for (c in cursos) {
    # Mientras exista algún grupo con sobrecupo en el curso c
    hay_sobrecupo <- TRUE
    while (hay_sobrecupo) {
      hay_sobrecupo <- FALSE
      # Revisamos cada grupo del curso c
      for (g in 1:num_grupos) {
        alumnos_grupo <- which(sol_hijo$course == c & sol_hijo$group == g)
        n_alumnos <- length(alumnos_grupo)
        if (n_alumnos > max_grupo_size) {
          hay_sobrecupo <- TRUE
          # Seleccionar un alumno al azar de los excedentes para reasignar
          # (podrías elegir también al final o de forma determinista)
          indice_sobrecupo <- sample(alumnos_grupo, 1)
          
          # Buscar grupos alternativos en el mismo curso (excluyendo el grupo g)
          grupos_alternativos <- setdiff(1:num_grupos, g)
          disponibles <- grupos_alternativos[sapply(grupos_alternativos, function(g2) {
            sum(sol_hijo$course == c & sol_hijo$group == g2) < max_grupo_size
          })]
          
          if (length(disponibles) == 0) {
            stop(paste("No hay capacidad disponible para reasignar al alumno en curso", c))
          }
          
          nuevo_grupo <- sample(disponibles, 1)
          sol_hijo$group[indice_sobrecupo] <- nuevo_grupo
          sol_hijo$node[indice_sobrecupo] <- paste(c, nuevo_grupo, sep = "-")
          # Al haber reasignado, salimos del ciclo for para re-evaluar la condición
          break
        }
      }
    }
  }
  
  sol_hijo$node <- paste(sol_hijo$course, sol_hijo$group, sep = "-")
  
  return(sol_hijo)
}


# Ordenamiento no dominado (versión rápida, pero no la más óptima posible).
# Recibe un data frame con las columnas "n_componentes" y "varianza_tamanio"
# Devuelve una lista con los índices de cada frente.
fast_non_dominated_sort <- function(objs) {
  n <- nrow(objs)
  dominated_count <- integer(n)
  dominates_list <- vector("list", n)
  
  # 1) Para cada individuo, identificar a quién domina y cuántos lo dominan
  for (i in seq_len(n)) {
    for (j in seq_len(n)) {
      if (i != j) {
        # Ver si i domina a j
        # Objetivo 1 (n_componentes): se quiere MAXIMIZAR
        # Objetivo 2 (varianza_tamanio): se quiere MINIMIZAR
        # i domina a j si:
        #   n_comp(i) >= n_comp(j)  (para el objetivo a maximizar)
        #   y var(i) <= var(j)      (para el objetivo a minimizar)
        #   y al menos una de las dos condiciones es estricta
        cond1 <- objs$n_componentes[i] >= objs$n_componentes[j]
        cond2 <- objs$varianza_tamanio[i] <= objs$varianza_tamanio[j]
        strictly_better <- (objs$n_componentes[i] > objs$n_componentes[j]) ||
          (objs$varianza_tamanio[i] < objs$varianza_tamanio[j])
        
        if (cond1 && cond2 && strictly_better) {
          # i domina a j
          dominates_list[[i]] <- c(dominates_list[[i]], j)
        } 
      }
    }
  }
  
  for (j in seq_len(n)) {
    # dominated_count[j] = cuántos individuos dominan a j
    dominated_count[j] <- sum(sapply(seq_len(n), function(i) j %in% dominates_list[[i]]))
  }
  
  # 2) Encontrar frente 1 (dominated_count == 0)
  fronts <- list()
  current_front <- which(dominated_count == 0)
  rank <- 1
  
  while (length(current_front) > 0) {
    fronts[[rank]] <- current_front
    next_front <- c()
    # Para cada p en el frente actual
    for (p in current_front) {
      # Para cada q a quien p domina
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

# Cálculo de la distancia de crowding en un frente
# Objetivos: 1) n_componentes (max), 2) varianza_tamanio (min)
# Recibe: data frame con las filas de un frente y sus índices
crowding_distance <- function(front_df) {
  # Si el frente tiene 1 o 2 individuos, se les asigna distancia infinita
  n <- nrow(front_df)
  if (n <= 2) {
    return(rep(Inf, n))
  }
  
  # Normalizar cada objetivo (para calcular la distancia)
  # Para n_componentes: mayor = mejor, invertimos sign en varianza_tamanio
  # o normalizamos cada uno por separado.
  f1 <- front_df$n_componentes
  f2 <- front_df$varianza_tamanio
  
  # Rango
  f1_min <- min(f1); f1_max <- max(f1)
  f2_min <- min(f2); f2_max <- max(f2)
  
  # Ordenar por f1
  idx_f1 <- order(f1)
  dist <- rep(0, n)
  dist[idx_f1[1]] <- Inf
  dist[idx_f1[n]] <- Inf
  for (k in 2:(n-1)) {
    dist[idx_f1[k]] <- dist[idx_f1[k]] + 
      (f1[idx_f1[k+1]] - f1[idx_f1[k-1]]) / (f1_max - f1_min + 1e-9)
  }
  
  # Ordenar por f2 (recuerda que este objetivo es “menor es mejor”)
  # Pero para crowding distance, nos da igual la dirección de optimización;
  # calculamos la dispersión.
  idx_f2 <- order(f2)
  dist[idx_f2[1]] <- Inf
  dist[idx_f2[n]] <- Inf
  for (k in 2:(n-1)) {
    dist[idx_f2[k]] <- dist[idx_f2[k]] + 
      (f2[idx_f2[k+1]] - f2[idx_f2[k-1]]) / (f2_max - f2_min + 1e-9)
  }
  
  return(dist)
}



nsga2 <- function(datos, 
                  pop_size = 20, 
                  num_generations = 10,
                  num_grupos, 
                  max_grupo_size,
                  p_mut = 0.1,       # probabilidad de mutación
                  p_familia = 0.5,   # probabilidad de heredar familia en el cruce
                  p_cross = 0.3      # probabilidad de cruce (nuevo)
) {
  
  # 1) Generar población inicial mixta (mitad aleatoria, mitad alternativa)
  poblacion <- c(
    replicate(pop_size / 2, 
              generar_solucion_aleatoria(datos, num_grupos, max_grupo_size), 
              simplify = FALSE),
    replicate(pop_size / 2, 
              generar_solucion_alternativa(datos, num_grupos, max_grupo_size), 
              simplify = FALSE)
  )
  
  
  # 2) Evaluar población
  evals <- do.call(rbind, lapply(poblacion, evaluar_solucion))
  
  for (gen in seq_len(num_generations)) {
    cat("\n=== Generación", gen, "===\n")
    
    # 3) Crear descendencia
    # 3.1) Ordenamiento no dominado de la población actual
    fronts <- fast_non_dominated_sort(evals)
    
    # 3.2) Asignar rank y crowding distance a cada individuo
    rank_vector <- rep(NA, pop_size)
    crowd_vector <- rep(NA, pop_size)
    
    for (i in seq_along(fronts)) {
      f <- fronts[[i]]
      rank_vector[f] <- i
      cd <- crowding_distance(evals[f,])
      crowd_vector[f] <- cd
    }
    
    # 3.3) Selección (torneo binario)
    seleccionados <- vector("list", pop_size)
    for (i in seq_len(pop_size)) {
      # Elegimos 2 índices aleatorios
      idx1 <- sample(seq_len(pop_size), 1)
      idx2 <- sample(seq_len(pop_size), 1)
      
      # Comparamos rank y crowding distance
      ganador <- idx1
      if ( (rank_vector[idx2] < rank_vector[idx1]) ||
           (rank_vector[idx2] == rank_vector[idx1] && crowd_vector[idx2] > crowd_vector[idx1]) ) {
        ganador <- idx2
      }
      seleccionados[[i]] <- poblacion[[ganador]]
    }
    
    # 3.4) Generar hijos aplicando cruce (con prob p_cross) y mutación
    hijos <- vector("list", pop_size)
    for (i in seq(1, pop_size, by=2)) {
      # Tomamos dos padres
      p1 <- seleccionados[[i]]
      p2 <- if (i < pop_size) seleccionados[[i+1]] else seleccionados[[1]]
      
      # Decidimos si cruzamos o simplemente copiamos
      if (runif(1) < p_cross) {
        # Cruce
        hijo1 <- cruzar_soluciones(p1, p2, max_grupo_size, num_grupos, prob_familia = p_familia)
        hijo2 <- cruzar_soluciones(p2, p1, max_grupo_size, num_grupos, prob_familia = p_familia)
      } else {
        # Sin cruce: copiamos directamente los padres
        hijo1 <- p1
        hijo2 <- p2
      }
      
      # Mutación
      hijo1 <- mutar_solucion_swap(hijo1, prob_swap = p_mut)
      hijo2 <- mutar_solucion_swap(hijo2, prob_swap = p_mut)
      
      hijos[[i]] <- hijo1
      if (i < pop_size) {
        hijos[[i+1]] <- hijo2
      }
    }
    
    # 3.5) Evaluar hijos
    evals_hijos <- do.call(rbind, lapply(hijos, evaluar_solucion))
    
    # 4) Unir padres + hijos
    poblacion_combinada <- c(poblacion, hijos)
    evals_combinada <- rbind(evals, evals_hijos)
    
    # 4.1) Ordenamiento no dominado de la población combinada
    fronts_comb <- fast_non_dominated_sort(evals_combinada)
    
    # 4.2) Construir la nueva población de tamaño pop_size
    nueva_poblacion <- list()
    nueva_evals <- data.frame(n_componentes = numeric(0), varianza_tamanio = numeric(0))
    cont <- 1
    
    for (f_idx in seq_along(fronts_comb)) {
      f <- fronts_comb[[f_idx]]
      # Si cabe entero en la nueva población
      if (length(f) + (cont - 1) <= pop_size) {
        for (idx in f) {
          nueva_poblacion[[cont]] <- poblacion_combinada[[idx]]
          nueva_evals <- rbind(nueva_evals, evals_combinada[idx, ])
          cont <- cont + 1
        }
      } else {
        # Se pasa del cupo; debemos usar la distancia de crowding
        cd <- crowding_distance(evals_combinada[f, ])
        # Ordenar por cd desc
        f_order <- f[order(cd, decreasing = TRUE)]
        # Tomar los que quepan
        slots <- pop_size - (cont - 1)
        f_sel <- f_order[seq_len(slots)]
        
        for (idx in f_sel) {
          nueva_poblacion[[cont]] <- poblacion_combinada[[idx]]
          nueva_evals <- rbind(nueva_evals, evals_combinada[idx, ])
          cont <- cont + 1
        }
        
        break # llenamos la población
      }
    }
    
    # Actualizar población y sus evaluaciones
    poblacion <- nueva_poblacion
    evals <- nueva_evals
    
    # (Opcional) Mostrar info de la mejor solución actual
    cat("Mejor frente (índices en la población combinada):", fronts_comb[[1]], "\n")
    cat("Tamaño del mejor frente:", length(fronts_comb[[1]]), "\n")
  }
  
  # Al finalizar, 'poblacion' y 'evals' contienen la población final.
  # Normalmente devolvemos la población final o el frente de Pareto final.
  return(list(poblacion = poblacion, evaluaciones = evals))
}


