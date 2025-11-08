# datasetCreator_NegBin_Robustness.R
library(tidyverse)
library(ggplot2)

# -------------------------------------------------
# Función para generar dataset con distribución NegBin desplazada
# -------------------------------------------------
generate_dataset_negbin <- function(num_groups,
                                    students_per_group,
                                    num_courses,
                                    mu_children,
                                    r_dispersion) {
  # Total de alumnos
  total_students <- num_groups * students_per_group * num_courses
  
  # Calcular p a partir de la media (E[K] = 1 + r*(1-p)/p)
  if (mu_children <= 1) stop("mu_children debe ser > 1")
  p_nb <- r_dispersion / (r_dispersion + (mu_children - 1))
  
  # --- Paso 1: generar tamaños de familia
  family_sizes <- c()
  while (sum(family_sizes) < total_students) {
    family_size <- rnbinom(1, size = r_dispersion, prob = p_nb) + 1  # K = 1 + NegBin(r,p)
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
mu_children_list <- c(1.1, 1.3, 1.5, 1.7, 2.0, 2.3, 2.6, 3.0)  # Medias
r_dispersion_list <- c(1, 2, 3)  # Controla overdispersion
seed_list <- c(8888, 9999, 4444)

# Crear carpetas
if (!dir.exists("datasetsRobustness")) dir.create("datasetsRobustness")
if (!dir.exists("results")) dir.create("results")

# Contenedor para índice
index_rows <- list()
row_id <- 0

# -------------------------------------------------
# Bucle principal
# -------------------------------------------------
for (num_groups in num_groups_list) {
  for (students_per_group in students_per_group_list) {
    for (r_dispersion in r_dispersion_list) {
      for (mu_children in mu_children_list) {
        for (seed in seed_list) {
          set.seed(seed)
          
          dataset <- generate_dataset_negbin(
            num_groups = num_groups,
            students_per_group = students_per_group,
            num_courses = num_courses,
            mu_children = mu_children,
            r_dispersion = r_dispersion
          )
          
          p_nb <- r_dispersion / (r_dispersion + (mu_children - 1))
          
          file_name <- paste0(
            "ds_NEGBIN_",
            num_groups, "G_",
            students_per_group, "S_",
            num_courses, "C_",
            sprintf("mu%.1f", mu_children), "_",
            "r", r_dispersion, "_",
            sprintf("p%.3f", p_nb), "_",
            seed, ".csv"
          )
          
          write.csv(dataset,
                    file = file.path("datasetsRobustness", file_name),
                    row.names = FALSE)
          
          row_id <- row_id + 1
          index_rows[[row_id]] <- tibble::tibble(
            file = file.path("datasetsRobustness", file_name),
            distribution = "NEGBIN",
            groups = num_groups,
            students_per_group = students_per_group,
            courses = num_courses,
            mu = mu_children,
            r = r_dispersion,
            p = round(p_nb, 6),
            seed = seed,
            total_students = num_groups * students_per_group * num_courses
          )
        }
      }
    }
  }
}

index_NB <- dplyr::bind_rows(index_rows) %>%
  dplyr::arrange(groups, students_per_group, r, mu, seed)

readr::write_csv(index_NB, file = file.path("datasetsRobustness", "index_NegBin.csv"))

# -------------------------------------------------
# Visualización de la PMF (NegBin desplazada)
# -------------------------------------------------
mu_values <- c(1.1, 1.5, 1.7, 2.0, 2.6, 3.0)
r_values <- c(1, 2, 3)
n_simulations <- 10000

distribution_data <- map_dfr(r_values, function(r) {
  map_dfr(mu_values, function(mu) {
    p <- r / (r + (mu - 1))
    family_sizes <- rnbinom(n_simulations, size = r, prob = p) + 1
    tibble(family_size = family_sizes,
           mu = factor(mu),
           r = factor(r))
  })
})

pmf_data <- distribution_data %>%
  group_by(r, mu, family_size) %>%
  summarise(frequency = n() / n_simulations, .groups = "drop")

plot_nb <- ggplot(pmf_data, aes(x = family_size, y = frequency, color = mu, group = mu)) +
  geom_line(linewidth = 1) +
  geom_point(size = 2) +
  facet_wrap(~r, labeller = label_bquote(r == .(r))) +
  scale_x_continuous(breaks = seq(1, max(pmf_data$family_size), by = 1), expand = c(0, 0)) +
  labs(
    title = "Family Size Distribution (Shifted Negative Binomial)",
    subtitle = "Different dispersion parameters (r)",
    x = "K (children per family)",
    y = "Relative Frequency",
    color = "Mean (mu)"
  ) +
  theme_minimal()

ggsave("results/family_size_distribution_NEGBIN_Robustness.pdf", plot_nb, width = 10, height = 6)
ggsave("results/family_size_distribution_NEGBIN_Robustness.jpg", plot_nb, width = 10, height = 6)
print(plot_nb)
