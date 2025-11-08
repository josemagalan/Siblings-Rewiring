# datasetCreator_Geometric_Robustness.R
library(tidyverse)
library(ggplot2)

# -------------------------------------------------
# Función para generar dataset con distribución geométrica desplazada
# -------------------------------------------------
generate_dataset_geometric <- function(num_groups,
                                       students_per_group,
                                       num_courses,
                                       mu_children) {
  # Total de alumnos
  total_students <- num_groups * students_per_group * num_courses
  
  # Parámetro p de la geométrica (media = 1/p)
  if (mu_children <= 1) stop("mu_children debe ser > 1")
  p_geom <- 1 / mu_children
  
  # --- Paso 1: generar tamaños de familia
  family_sizes <- c()
  while (sum(family_sizes) < total_students) {
    family_size <- rgeom(1, prob = p_geom) + 1  # K = 1 + Geom(p)
    family_sizes <- c(family_sizes, family_size)
  }
  
  # Ajustar si se sobrepasa el total
  difference <- sum(family_sizes) - total_students
  if (difference > 0) {
    family_sizes[length(family_sizes)] <- family_sizes[length(family_sizes)] - difference
    if (family_sizes[length(family_sizes)] == 0) {
      family_sizes <- family_sizes[-length(family_sizes)]
    }
  }
  
  # IDs y familias
  student_ids <- 1:total_students
  families <- rep(seq_along(family_sizes), times = family_sizes)
  students_family <- data.frame(student_id = student_ids, family_id = families)
  
  # --- Paso 2: asignar a cursos y grupos
  total_groups <- num_courses * num_groups
  groups <- expand.grid(course = 1:num_courses, group = 1:num_groups)
  repeated_groups <- groups[rep(1:nrow(groups), each = students_per_group), ]
  
  if (nrow(repeated_groups) < total_students) stop("No hay suficientes grupos.")
  
  assigned_groups <- repeated_groups[1:total_students, ]
  shuffled_students <- sample(student_ids)
  
  data <- data.frame(
    student_id = shuffled_students,
    course = assigned_groups$course,
    group  = assigned_groups$group
  )
  
  data <- merge(data, students_family, by = "student_id")
  data <- data[order(data$student_id), ]
  
  # --- Paso 3: hermanos
  data <- data %>%
    group_by(family_id) %>%
    mutate(sibling_ids = map(student_id, ~ setdiff(student_id, .x))) %>%
    ungroup()
  
  data$sibling_ids <- sapply(data$sibling_ids, function(s) {
    if (length(s) == 0) NA_character_ else paste(sort(s), collapse = ",")
  })
  
  data$num_siblings <- sapply(data$sibling_ids, function(s) {
    if (is.na(s) || s == "") 0 else length(strsplit(s, ",")[[1]])
  })
  
  data %>% select(student_id, course, group, family_id, sibling_ids, num_siblings)
}

# -------------------------------------------------
# Parámetros del experimento
# -------------------------------------------------
num_groups_list <- 2:4
students_per_group_list <- c(20, 30, 40)
num_courses <- 9
mu_children_list <- c(1.1, 1.3, 1.5, 1.7, 2.0, 2.3, 2.6, 3.0)  # 8 valores
seed_list <- c(1111, 2222, 3333)

# Crear carpeta destino
if (!dir.exists("datasetsRobustness")) dir.create("datasetsRobustness")
if (!dir.exists("results")) dir.create("results")
# Inicializar contenedor para el índice (usamos lista por eficiencia)
index_rows <- list()
row_id <- 0

# -------------------------------------------------
# Bucle principal para generar datasets
# -------------------------------------------------
for (num_groups in num_groups_list) {
  for (students_per_group in students_per_group_list) {
    for (mu_children in mu_children_list) {
      for (seed in seed_list) {
        set.seed(seed)
        dataset <- generate_dataset_geometric(
          num_groups = num_groups,
          students_per_group = students_per_group,
          num_courses = num_courses,
          mu_children = mu_children
        )
        
        p_geom <- 1 / mu_children
        
        file_name <- paste0(
          "ds_GEOM_",
          num_groups, "G_",
          students_per_group, "S_",
          num_courses, "C_",
          sprintf("mu%.1f", mu_children), "_",
          sprintf("p%.3f", p_geom), "_",
          seed, ".csv"
        )
        
        write.csv(dataset,
                  file = file.path("datasetsRobustness", file_name),
                  row.names = FALSE)
        
        
        row_id <- row_id + 1
        index_rows[[row_id]] <- tibble::tibble(
          file = file.path("datasetsRobustness", file_name),
          distribution = "GEOM",
          groups = num_groups,
          students_per_group = students_per_group,
          courses = num_courses,
          mu = mu_children,
          p = round(1 / mu_children, 6),
          seed = seed,
          total_students = num_groups * students_per_group * num_courses
        )
        
        
        
        
        
      }
    }
  }
}


index_GEOM <- dplyr::bind_rows(index_rows)

# Añade alguna metainfo útil ordenada por parámetros
index_GEOM <- index_GEOM %>%
  dplyr::arrange(groups, students_per_group, mu, seed)

if (!dir.exists("datasetsRobustness")) dir.create("datasetsRobustness")

# Guardar índice
readr::write_csv(index_GEOM, file = file.path("datasetsRobustness", "index_GEOM.csv"))




# -------------------------------------------------
# Visualización: PMF de la geométrica desplazada
# -------------------------------------------------
mu_values <- c(1.1, 1.3, 1.5, 1.7, 2.0, 2.3, 2.6, 3.0)
n_simulations <- 10000

distribution_data <- map_dfr(mu_values, function(mu) {
  p <- 1 / mu
  family_sizes <- rgeom(n_simulations, prob = p) + 1
  tibble(family_size = family_sizes, mu = factor(mu))
})

pmf_data <- distribution_data %>%
  group_by(mu, family_size) %>%
  summarise(frequency = n() / n_simulations, .groups = "drop")

plot_geom <- ggplot(pmf_data, aes(x = family_size, y = frequency, color = mu, group = mu)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  scale_x_continuous(breaks = seq(1, max(pmf_data$family_size), by = 1), expand = c(0, 0)) +
  labs(
    title = "Family Size Distribution (Shifted Geometric)",
    x = "K (children per family)",
    y = "Relative Frequency",
    color = "Mean (mu)"
  ) +
  theme_minimal()

ggsave("results/family_size_distribution_GEOM_Robustness.pdf", plot_geom, width = 8, height = 6)
ggsave("results/family_size_distribution_GEOM_Robustness.jpg", plot_geom, width = 8, height = 6)
print(plot_geom)
