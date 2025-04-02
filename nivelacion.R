############################
#      FUNCIONES AUX       #
############################

# ---------------------------------------------------------
# RecalcularDelta:
#   - Calcula la cantidad de estudiantes n_{c,g} en cada
#     (curso c, grupo g).
#   - Devuelve un data frame/estructura con la columna Delta_{c,g} = n_{c,g} - num_students
# ---------------------------------------------------------
RecalcularDelta <- function(Asignacion, num_students) {
  # Suponiendo que Asignacion tiene columnas: student_id, course, group, ...
  # n_{c,g} es el conteo de alumnos para cada (c, g)
  df_counts <- Asignacion %>%
    group_by(course, group) %>%
    summarise(n = n(), .groups = "drop") %>%
    mutate(Delta = n - num_students)
  
  # Retornar un data frame con (course, group, n, Delta)
  df_counts
}

library(dplyr)
library(tidyr)
library(igraph)

#' Contar componentes conexos a nivel de grupos
#'
#' @param Asignacion Data frame con al menos las columnas:
#'        - student_id
#'        - family_id
#'        - course
#'        - group
#' @return Un entero con la cantidad de componentes conexos
#'         en el grafo donde cada nodo es un (course, group).
#'
contar_componentes_grupo <- function(Asignacion) {
  # 1) Creamos un identificador de nodo para cada (curso, grupo)
  #    (Por ejemplo, "1-A", "2-B", etc.)
  Asignacion_nodos <- Asignacion %>%
    mutate(node = paste(course, group, sep = "-")) %>%
    distinct(student_id, family_id, node)
  
  # 2) Para cada familia, buscamos todos los nodos (course-grupo) en los que está:
  #    si una familia tiene miembros en nodos A, B, C,
  #    se generan aristas A-B, A-C, B-C (todas las combinaciones de pares).
  edges_familias <- Asignacion_nodos %>%
    group_by(family_id) %>%
    # nos interesan solo familias con >=2 nodos distintos
    filter(n() > 1) %>%
    summarise(
      pares = list(as.data.frame(t(combn(node, 2)))),
      .groups = "drop"
    ) %>%
    unnest(pares)
  
  # Renombramos para claridad
  colnames(edges_familias) <- c("family_id", "from", "to")
  
  # 3) Construimos el grafo con igraph, usando (from, to) como aristas.
  #    Notar que un mismo par puede repetirse para familias distintas, por lo que
  #    es conveniente hacer 'distinct()' si no queremos aristas duplicadas.
  edges_familias <- distinct(edges_familias, from, to)
  g <- graph_from_data_frame(edges_familias, directed = FALSE)
  
  # 4) Calculamos la cantidad de componentes conexos
  n_comp <- components(g)$no
  
  return(n_comp)
}


# ---------------------------------------------------------
# MoverFamilia:
#   - Dada una familia F, reasigna *todos* sus miembros
#     a un nuevo grupo g' en *todos* los cursos que cursan.
#   - Retorna la nueva Asignacion_prov resultante.
# ---------------------------------------------------------
MoverFamilia <- function(Asignacion, familia_id, nuevo_grupo) {
  # La familia entera pasa a un mismo grupo para cada curso
  # donde aparezca. 
  # OJO: dependen de la lógica de "grupos disponibles" y
  # si la familia cursa múltiples cursos.
  Asignacion_mod <- Asignacion %>%
    mutate(group = if_else(family_id == familia_id, nuevo_grupo, group))
  
  return(Asignacion_mod)
}



# ---------------------------------------------------------
# MoverAlumno:
#   - Dado un alumno A, reasignarlo a un nuevo grupo g'
#     en su curso c.
#   - Retorna la nueva Asignacion_prov resultante.
# ---------------------------------------------------------
MoverAlumno <- function(Asignacion, alumno_id, nuevo_grupo) {
  # Asumiendo un alumno está en un único curso dentro
  # de la asignación actual. Si el modelo permite un
  # alumno en varios cursos, habría que adaptarlo.
  Asignacion_mod <- Asignacion %>%
    mutate(group = if_else(student_id == alumno_id, nuevo_grupo, group))
  
  return(Asignacion_mod)
}

# ---------------------------------------------------------
# mejoraDistribucion:
#   - Define el criterio para "mejorar" la distribución,
#     típicamente que se acerque más a la meta (num_students)
#     en cada (c,g), o reduzca la suma de |Delta_{c,g}|.
#   - Retorna TRUE o FALSE.
# ---------------------------------------------------------
mejoraDistribucion <- function(Asignacion_actual, Asignacion_prov, num_students) {
  # Un posible criterio: comparar la suma de desvios absolutos
  # antes y después del movimiento
  deltas_actual <- RecalcularDelta(Asignacion_actual, num_students)
  deltas_prov   <- RecalcularDelta(Asignacion_prov,   num_students)
  
  sum_dev_actual <- sum(abs(deltas_actual$Delta))
  sum_dev_prov   <- sum(abs(deltas_prov$Delta))
  
  # "mejorar" = reduce la suma de las desviaciones
  return( sum_dev_prov < sum_dev_actual )
}
############################
#  FIN FUNCIONES AUX       #
############################


# ------------------------------------------------------------------
#        FUNCION PRINCIPAL: Ajustar_Exacto_Nivelacion
# ------------------------------------------------------------------
Ajustar_Exacto_Nivelacion <- function(asignacion_heuristica,
                                      num_students = 20) {
  # 1) Asignacion <- asignacion_heuristica
  Asignacion <- asignacion_heuristica
  
  # 2) comp_inicial = #comp(Asignacion)
  comp_inicial <- contar_componentes_grupo(Asignacion)
  
  # 3) Recalcular n_{c,g} y Delta_{c,g} = n_{c,g} - num_students
  df_deltas <- RecalcularDelta(Asignacion, num_students)
  
  # ===============================================================
  # FASE 1: REASIGNACION DE FAMILIAS (sin reducir conectividad)
  # ===============================================================
  repeat {
    cambio_fase1 <- FALSE
    
    # Tomamos (c,g) sobrepoblados: Delta_{c,g} > 0
    sobrepoblados <- df_deltas %>%
      filter(Delta > 0)
    
    if (nrow(sobrepoblados) == 0) {
      # No hay sobrepoblados, se acaba la Fase 1
      break
    }
    
    # Recorremos cada (c,g) que esté sobrepoblado
    for (idx_s in seq_len(nrow(sobrepoblados))) {
      c_g_sobre <- sobrepoblados[idx_s, ]
      curso_sobre <- c_g_sobre$course
      grupo_sobre <- c_g_sobre$group
      
      # Seleccionamos las familias en (curso_sobre, grupo_sobre)
      familias_sobre <- Asignacion %>%
        filter(course == curso_sobre, group == grupo_sobre) %>%
        distinct(family_id)
      
      # Buscamos (c',g') subpoblados: Delta_{c',g'} < 0
      subpoblados <- df_deltas %>%
        filter(Delta < 0)
      
      # Iteramos por cada familia F de ese (c,g)
      for (f_id in familias_sobre$family_id) {
        
        # probamos reubicar la familia a cada (c',g') subpoblado
        for (idx_sub in seq_len(nrow(subpoblados))) {
          c_g_sub <- subpoblados[idx_sub, ]
          curso_sub <- c_g_sub$course
          grupo_sub <- c_g_sub$group
          
          # Verificar si la familia F *puede* moverse a 'grupo_sub'
          # en TODOS los cursos que cursa. Para este ejemplo simple,
          # asumimos que la familia sólo cursa 1 curso (o ignoramos
          # la validación). En un caso real, habría que verificar.
          Asignacion_prov <- MoverFamilia(Asignacion, f_id, grupo_sub)
          
          comp_prov <- contar_componentes_grupo(Asignacion_prov)
          
          # Verificamos que la conectividad no baje
          if (comp_prov >= comp_inicial &&
              mejoraDistribucion(Asignacion, Asignacion_prov, num_students)) {
            
            # Aceptar el movimiento
            Asignacion <- Asignacion_prov
            comp_inicial <- comp_prov
            df_deltas <- RecalcularDelta(Asignacion, num_students)
            
            cambio_fase1 <- TRUE
            break  # romper subpoblados
          }
        }
        
        if (cambio_fase1) {
          break  # romper familias
        }
      }
      
      if (cambio_fase1) {
        break  # romper (c,g) sobrepoblados
      }
    }
    
    # Si no hubo ningún cambio, salimos de FASE 1
    if (!cambio_fase1) {
      break
    }
  }  # end repeat (FASE 1)
  
  
  # ===============================================================
  # FASE 2: AJUSTE INDIVIDUAL
  # ===============================================================
  # 5) comp_fase2 = comp_inicial
  comp_fase2 <- comp_inicial
  
  # ------------------------------------------
  # 5.1) Sub-fase (a): Intentar NO reducir conectividad
  # ------------------------------------------
  repeat {
    cambio_fase2a <- FALSE
    df_deltas <- RecalcularDelta(Asignacion, num_students)
    sobrepoblados <- df_deltas %>% filter(Delta > 0)
    subpoblados   <- df_deltas %>% filter(Delta < 0)
    
    if (nrow(sobrepoblados) == 0) {
      # No hay sobrepoblados => distribución OK o subpoblada
      break
    }
    
    # Para cada (c,g) sobrepoblado
    for (idx_s in seq_len(nrow(sobrepoblados))) {
      c_g_sobre <- sobrepoblados[idx_s, ]
      curso_sobre <- c_g_sobre$course
      grupo_sobre <- c_g_sobre$group
      
      # Seleccionamos alumnos en (curso_sobre, grupo_sobre)
      alumnos_sobre <- Asignacion %>%
        filter(course == curso_sobre, group == grupo_sobre)
      
      # Probar reasignación a subpoblados
      for (a_id in alumnos_sobre$student_id) {
        for (idx_sub in seq_len(nrow(subpoblados))) {
          c_g_sub <- subpoblados[idx_sub, ]
          curso_sub <- c_g_sub$course
          grupo_sub <- c_g_sub$group
          
          # Verificar si el alumno A puede cursar en grupo_sub
          # (En un caso real, tal vez A está en curso_sobre != curso_sub,
          #   hay que decidir cómo manejarlo. Aquí lo simplificamos.)
          Asignacion_prov <- MoverAlumno(Asignacion, a_id, grupo_sub)
          comp_prov <- contar_componentes_grupo(Asignacion_prov)
          
          # no reducir conectividad
          if (comp_prov >= comp_inicial &&
              mejoraDistribucion(Asignacion, Asignacion_prov, num_students)) {
            # Aceptamos
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
    
    if (!cambio_fase2a) {
      # no se pudo hacer ningún cambio
      break
    }
  }
  
  # 5.2) Ver si logramos 20 exactos en todos
  df_deltas <- RecalcularDelta(Asignacion, num_students)
  if (all(df_deltas$Delta == 0)) {
    # Conseguiste la nivelación exacta sin bajar conectividad
    return(Asignacion)
  }
  
  # ------------------------------------------
  # 5.3) Sub-fase (b): Se permite bajar conectividad
  # ------------------------------------------
  comp_minimo <- contar_componentes_grupo(Asignacion)
  
  repeat {
    cambio_fase2b <- FALSE
    df_deltas <- RecalcularDelta(Asignacion, num_students)
    sobrepoblados <- df_deltas %>% filter(Delta > 0)
    subpoblados   <- df_deltas %>% filter(Delta < 0)
    
    if (nrow(sobrepoblados) == 0) {
      break
    }
    
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
          
          # Ahora permitimos comp_prov < comp_minimo, mientras mejore la distribución
          if (mejoraDistribucion(Asignacion, Asignacion_prov, num_students)) {
            # Aceptamos aunque baje la conectividad
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
    
    if (!cambio_fase2b) {
      # No se pudo hacer ningún cambio más
      break
    }
  }
  
  # Ver si ahora tenemos 20 exactos en todos
  df_deltas <- RecalcularDelta(Asignacion, num_students)
  if (all(df_deltas$Delta == 0)) {
    return(Asignacion)
  }
  
  # 6) // Si finaliza FASE 2 y no se pudo lograr 20 en todos
  return(Asignacion)
}
