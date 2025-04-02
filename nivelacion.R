############################
#      AUXILIARY FUNCTIONS
############################

# ---------------------------------------------------------
# RecalcularDelta:
#   - Computes the number of students n_{c,g} in each
#     (course c, group g).
#   - Returns a data frame with Delta_{c,g} = n_{c,g} - num_students
# ---------------------------------------------------------
RecalcularDelta <- function(Asignacion, num_students) {
  df_counts <- Asignacion %>%
    group_by(course, group) %>%
    summarise(n = n(), .groups = "drop") %>%
    mutate(Delta = n - num_students)
  df_counts
}

# ---------------------------------------------------------
# contar_componentes_grupo:
#   - Computes the number of connected components between (course, group) nodes
#     based on shared family presence across groups.
# ---------------------------------------------------------
contar_componentes_grupo <- function(Asignacion) {
  Asignacion_nodos <- Asignacion %>%
    mutate(node = paste(course, group, sep = "-")) %>%
    distinct(student_id, family_id, node)
  
  edges_familias <- Asignacion_nodos %>%
    group_by(family_id) %>%
    filter(n() > 1) %>%
    summarise(
      pares = list(as.data.frame(t(combn(node, 2)))),
      .groups = "drop"
    ) %>%
    unnest(pares)
  
  colnames(edges_familias) <- c("family_id", "from", "to")
  edges_familias <- distinct(edges_familias, from, to)
  g <- graph_from_data_frame(edges_familias, directed = FALSE)
  
  n_comp <- components(g)$no
  return(n_comp)
}

# ---------------------------------------------------------
# MoverFamilia:
#   - Moves all members of a family to a new group g'
#     in all their enrolled courses.
# ---------------------------------------------------------
MoverFamilia <- function(Asignacion, familia_id, nuevo_grupo) {
  Asignacion_mod <- Asignacion %>%
    mutate(group = if_else(family_id == familia_id, nuevo_grupo, group))
  return(Asignacion_mod)
}

# ---------------------------------------------------------
# MoverAlumno:
#   - Reassigns a student to a new group g' in their course.
# ---------------------------------------------------------
MoverAlumno <- function(Asignacion, alumno_id, nuevo_grupo) {
  Asignacion_mod <- Asignacion %>%
    mutate(group = if_else(student_id == alumno_id, nuevo_grupo, group))
  return(Asignacion_mod)
}

# ---------------------------------------------------------
# mejoraDistribucion:
#   - Returns TRUE if the proposed assignment reduces the
#     total absolute deviation from target group size.
# ---------------------------------------------------------
mejoraDistribucion <- function(Asignacion_actual, Asignacion_prov, num_students) {
  deltas_actual <- RecalcularDelta(Asignacion_actual, num_students)
  deltas_prov   <- RecalcularDelta(Asignacion_prov,   num_students)
  
  sum_dev_actual <- sum(abs(deltas_actual$Delta))
  sum_dev_prov   <- sum(abs(deltas_prov$Delta))
  
  return(sum_dev_prov < sum_dev_actual)
}


# ------------------------------------------------------------------
# MAIN FUNCTION: Ajustar_Exacto_Nivelacion
#   - Adjusts an initial group assignment to achieve exact leveling
#     (e.g., 20 students per (course, group)), while trying to preserve
#     or increase connectivity between groups.
# ------------------------------------------------------------------
Ajustar_Exacto_Nivelacion <- function(asignacion_heuristica,
                                      num_students = 20) {
  # 1) Initialize assignment
  Asignacion <- asignacion_heuristica
  
  # 2) Compute initial number of components
  comp_inicial <- contar_componentes_grupo(Asignacion)
  
  # 3) Calculate Delta_{c,g} = n_{c,g} - num_students
  df_deltas <- RecalcularDelta(Asignacion, num_students)
  
  # ===============================================================
  # PHASE 1: FAMILY REALLOCATION (without reducing connectivity)
  # ===============================================================
  repeat {
    cambio_fase1 <- FALSE
    
    sobrepoblados <- df_deltas %>%
      filter(Delta > 0)
    
    if (nrow(sobrepoblados) == 0) break  # No overloaded groups
    
    for (idx_s in seq_len(nrow(sobrepoblados))) {
      c_g_sobre <- sobrepoblados[idx_s, ]
      curso_sobre <- c_g_sobre$course
      grupo_sobre <- c_g_sobre$group
      
      familias_sobre <- Asignacion %>%
        filter(course == curso_sobre, group == grupo_sobre) %>%
        distinct(family_id)
      
      subpoblados <- df_deltas %>%
        filter(Delta < 0)
      
      for (f_id in familias_sobre$family_id) {
        for (idx_sub in seq_len(nrow(subpoblados))) {
          c_g_sub <- subpoblados[idx_sub, ]
          curso_sub <- c_g_sub$course
          grupo_sub <- c_g_sub$group
          
          Asignacion_prov <- MoverFamilia(Asignacion, f_id, grupo_sub)
          comp_prov <- contar_componentes_grupo(Asignacion_prov)
          
          if (comp_prov >= comp_inicial &&
              mejoraDistribucion(Asignacion, Asignacion_prov, num_students)) {
            Asignacion <- Asignacion_prov
            comp_inicial <- comp_prov
            df_deltas <- RecalcularDelta(Asignacion, num_students)
            cambio_fase1 <- TRUE
            break
          }
        }
        if (cambio_fase1) break
      }
      if (cambio_fase1) break
    }
    
    if (!cambio_fase1) break
  }
  
  # ===============================================================
  # PHASE 2: INDIVIDUAL ADJUSTMENT
  # ===============================================================
  comp_fase2 <- comp_inicial
  
  # ------------------------------------------
  # Phase 2a: Try to improve without reducing connectivity
  # ------------------------------------------
  repeat {
    cambio_fase2a <- FALSE
    df_deltas <- RecalcularDelta(Asignacion, num_students)
    sobrepoblados <- df_deltas %>% filter(Delta > 0)
    subpoblados   <- df_deltas %>% filter(Delta < 0)
    
    if (nrow(sobrepoblados) == 0) break
    
    for (idx_s in seq_len(nrow(sobrepoblados))) {
      c_g_sobre <- sobrepoblados[idx_s, ]
      curso_sobre <- c_g_sobre$course
      grupo_sobre <- c_g_sobre$group
      
      alumnos_sobre <- Asignacion %>%
        filter(course == curso_sobre, group == grupo_sobre)
      
      for (a_id in alumnos_sobre$student_id) {
        for (idx_sub in seq_len(nrow(subpoblados))) {
          c_g_sub <- subpoblados[idx_sub, ]
          curso_sub <- c_g_sub$course
          grupo_sub <- c_g_sub$group
          
          Asignacion_prov <- MoverAlumno(Asignacion, a_id, grupo_sub)
          comp_prov <- contar_componentes_grupo(Asignacion_prov)
          
          if (comp_prov >= comp_inicial &&
              mejoraDistribucion(Asignacion, Asignacion_prov, num_students)) {
            Asignacion <- Asignacion_prov
            comp_inicial <- comp_prov
            cambio_fase2a <- TRUE
            break
          }
        }
        if (cambio_fase2a) break
      }
      if (cambio_fase2a) break
    }
    
    if (!cambio_fase2a) break
  }
  
  # 5.2) Check if leveling is already exact
  df_deltas <- RecalcularDelta(Asignacion, num_students)
  if (all(df_deltas$Delta == 0)) return(Asignacion)
  
  # ------------------------------------------
  # Phase 2b: Allow decrease in connectivity if needed
  # ------------------------------------------
  comp_minimo <- contar_componentes_grupo(Asignacion)
  
  repeat {
    cambio_fase2b <- FALSE
    df_deltas <- RecalcularDelta(Asignacion, num_students)
    sobrepoblados <- df_deltas %>% filter(Delta > 0)
    subpoblados   <- df_deltas %>% filter(Delta < 0)
    
    if (nrow(sobrepoblados) == 0) break
    
    for (idx_s in seq_len(nrow(sobrepoblados))) {
      c_g_sobre <- sobrepoblados[idx_s, ]
      curso_sobre <- c_g_sobre$course
      grupo_sobre <- c_g_sobre$group
      
      alumnos_sobre <- Asignacion %>%
        filter(course == curso_sobre, group == grupo_sobre)
      
      for (a_id in alumnos_sobre$student_id) {
        for (idx_sub in seq_len(nrow(subpoblados))) {
          c_g_sub <- subpoblados[idx_sub, ]
          curso_sub <- c_g_sub$course
          grupo_sub <- c_g_sub$group
          
          Asignacion_prov <- MoverAlumno(Asignacion, a_id, grupo_sub)
          comp_prov <- contar_componentes_grupo(Asignacion_prov)
          
          if (mejoraDistribucion(Asignacion, Asignacion_prov, num_students)) {
            Asignacion <- Asignacion_prov
            comp_minimo <- comp_prov
            cambio_fase2b <- TRUE
            break
          }
        }
        if (cambio_fase2b) break
      }
      if (cambio_fase2b) break
    }
    
    if (!cambio_fase2b) break
  }
  
  # Final check for exact leveling
  df_deltas <- RecalcularDelta(Asignacion, num_students)
  return(Asignacion)
}
