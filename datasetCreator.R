library(tidyverse)

# Parámetros de entrada.
num_grupos <- 3          # Número de grupos por curso
alumnos_por_grupo <- 20  # Número de alumnos por grupo
num_cursos <- 9          # Número de cursos
lambda_hermanos <- 0.5   # Parámetro lambda para la distribución de Poisson

generar_dataset <- function(num_grupos, alumnos_por_grupo, num_cursos, lambda_hermanos) {
  
  # Calcular el número total de alumnos.
  total_alumnos <- num_grupos * alumnos_por_grupo * num_cursos
  
  # Paso 1: Generar tamaños de familias
  tamanos_familias <- c()
  while (sum(tamanos_familias) < total_alumnos) {
    # Generamos un tamaño de familia
    tam_familia <- rpois(1, lambda_hermanos) + 1  # +1 incluye al alumno
    tamanos_familias <- c(tamanos_familias, tam_familia)
  }
  
  # Ajustar el tamaño de la última familia si es necesario
  diferencia <- sum(tamanos_familias) - total_alumnos
  if (diferencia > 0) {
    tamanos_familias[length(tamanos_familias)] <- tamanos_familias[length(tamanos_familias)] - diferencia
    # Si el tamaño resulta en 0, eliminamos esa familia
    if (tamanos_familias[length(tamanos_familias)] == 0) {
      tamanos_familias <- tamanos_familias[-length(tamanos_familias)]
    }
  }
  
  # Asignar IDs a los alumnos
  alumno_ids <- 1:total_alumnos
  
  # Asignar familias a los alumnos
  familias <- rep(1:length(tamanos_familias), times = tamanos_familias)
  alumnos_familia <- data.frame(alumno_id = alumno_ids, familia_id = familias)
  
  # Paso 2: Asignar alumnos aleatoriamente a cursos y grupos
  # Crear una lista de todos los cursos y grupos disponibles
  total_grupos <- num_cursos * num_grupos
  grupos <- expand.grid(curso = 1:num_cursos, grupo = 1:num_grupos)
  
  # Repetir el data frame de grupos para cubrir todos los alumnos
  grupos_repetidos <- grupos[rep(1:nrow(grupos), each = alumnos_por_grupo), ]
  
  # Verificar que tenemos suficientes grupos para todos los alumnos
  if (nrow(grupos_repetidos) < total_alumnos) {
    stop("No hay suficientes grupos para asignar a todos los alumnos.")
  }
  
  # Tomamos solo los grupos necesarios
  grupos_asignados <- grupos_repetidos[1:total_alumnos, ]
  
  # Mezclar aleatoriamente los alumnos
  set.seed(123)  # Puedes cambiar o eliminar la semilla para mayor aleatoriedad
  alumnos_permutados <- sample(alumno_ids)
  
  # Asignar curso y grupo a los alumnos permutados
  data <- data.frame(
    alumno_id = alumnos_permutados,
    curso = grupos_asignados$curso,
    grupo = grupos_asignados$grupo
  )
  
  # Añadir familia_id
  data <- merge(data, alumnos_familia, by = "alumno_id")
  
  # Ordenar por alumno_id
  data <- data[order(data$alumno_id), ]
  
  # Paso 3: Añadir columna de hermanos
  data <- data %>%
    group_by(familia_id) %>%
    mutate(hermano_id = map(alumno_id, ~ setdiff(alumno_id, .x))) %>%
    ungroup()
  
  # Convertir la lista de hermanos en cadena de texto
  data$hermano_id <- sapply(data$hermano_id, function(h) {
    if (length(h) == 0) {
      return(NA)
    } else {
      paste(sort(h), collapse = ",")
    }
  })
  
  # Añadir columna num_hermanos
  data$num_hermanos <- sapply(data$hermano_id, function(h) {
    if (is.na(h) || h == "") {
      return(0)
    } else {
      length(strsplit(h, ",")[[1]])
    }
  })
  
  # Reordenar columnas
  data <- data %>% select(alumno_id, curso, grupo, familia_id, hermano_id, num_hermanos)
  
  return(data)
}

# Parámetros variables
num_grupos_list <- 2:4  # Número de grupos variando entre 2 y 4
lambda_hermanos_list <- seq(0.1, 2.0, by = 0.1) # Valores de lambda para la distribución de Poisson
seed_list <- c(6666, 7777, 14141414)  # Semillas diferentes para cada dataset

# Crear la carpeta 'datasets' si no existe
if (!dir.exists("datasets")) {
  dir.create("datasets")
}

# Iterar sobre las combinaciones de parámetros
for (num_grupos in num_grupos_list) {
  for (lambda_hermanos in lambda_hermanos_list) {
    for (seed in seed_list) {
      # Establecer la semilla
      set.seed(seed)
      
      # Generar el dataset
      dataset <- generar_dataset(num_grupos, alumnos_por_grupo, num_cursos, lambda_hermanos)
      
      # Crear el nombre del archivo incluyendo todos los parámetros
      nombre_archivo <- paste0(
        "ds_",
        num_grupos, "_",
        alumnos_por_grupo, "_",
        num_cursos, "_",
        format(lambda_hermanos, nsmall = 1), "_",
        seed, ".csv"
      )
      
      # Guardar el dataset en la carpeta 'datasets'
      write.csv(dataset, file = file.path("datasets", nombre_archivo), row.names = FALSE)
    }
  }
}

