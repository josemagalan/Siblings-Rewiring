library(tidyverse)

# Parámetros de entrada.
num_grupos <- 3          # Número de grupos por curso
alumnos_por_grupo <- 20  # Número de alumnos por grupo
num_cursos <- 9          # Número de cursos
lambda_hermanos <- 0.2   # Parámetro lambda para la distribución de Poisson (ajusta según necesidad)

#' Genera un dataset de alumnos con relaciones de hermanos bidireccionales y coherentes.
#'
#' @param num_grupos Número de grupos por curso.
#' @param alumnos_por_grupo Número de alumnos por grupo.
#' @param num_cursos Número de cursos.
#' @param lambda_hermanos Parámetro lambda para la distribución de Poisson (determina el número de hermanos).
#' @return Un dataframe 'tibble' con información de alumnos y sus hermanos.
#' @examples
#' dataset <- generar_dataset(3, 30, 4, 0.2)
generar_dataset <- function(num_grupos, alumnos_por_grupo, num_cursos, lambda_hermanos) {
  
  # Calcular el número total de alumnos.
  total_alumnos <- num_grupos * alumnos_por_grupo * num_cursos
  
  # Crear el dataframe inicial de alumnos.
  data <- tibble(
    alumno_id = 1:total_alumnos,
    curso = rep(1:num_cursos, each = num_grupos * alumnos_por_grupo),
    grupo = rep(rep(1:num_grupos, each = alumnos_por_grupo), times = num_cursos)
  )
  
  # Inicializar una lista para almacenar los grupos de hermanos.
  # Cada alumno comienza en su propio grupo de hermanos (solo él mismo).
  grupos_hermanos <- vector("list", total_alumnos)
  for (i in 1:total_alumnos) {
    grupos_hermanos[[i]] <- c(i)
  }
  
  # Asignar hermanos a cada alumno.
  for (i in 1:total_alumnos) {
    # Generar el número de hermanos para el alumno i usando una distribución de Poisson.
    num_hermanos <- rpois(1, lambda_hermanos)
    
    if (num_hermanos > 0) {
      # Obtener los posibles nuevos hermanos que no estén ya en su grupo de hermanos.
      posibles_nuevos_hermanos <- setdiff(1:total_alumnos, grupos_hermanos[[i]])
      
      if (length(posibles_nuevos_hermanos) > 0) {
        # Seleccionar hermanos al azar.
        num_seleccion <- min(num_hermanos, length(posibles_nuevos_hermanos))
        nuevos_hermanos <- sample(posibles_nuevos_hermanos, size = num_seleccion, replace = FALSE)
        
        # Unir los grupos de hermanos de todos los involucrados.
        grupo_completo <- unique(c(grupos_hermanos[[i]], unlist(grupos_hermanos[nuevos_hermanos])))
        
        # Actualizar el grupo de hermanos para cada miembro.
        for (miembro in grupo_completo) {
          grupos_hermanos[[miembro]] <- grupo_completo
        }
      }
    }
  }
  
  # Crear una columna en 'data' con los IDs de los hermanos.
  data$hermano_id <- sapply(1:total_alumnos, function(i) {
    # Excluir el propio alumno de la lista de hermanos.
    hermanos <- setdiff(grupos_hermanos[[i]], i)
    # Convertir la lista de hermanos en una cadena de texto separada por comas.
    paste(sort(hermanos), collapse = ",")
  })
  
  return(data)
}

# Generar un dataset de ejemplo.
set.seed(123)  # Para reproducibilidad.
dataset <- generar_dataset(num_grupos, alumnos_por_grupo, num_cursos, lambda_hermanos)

# Mostrar el resultado.
print(dataset)
