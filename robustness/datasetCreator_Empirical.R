# datasetCreator_Empirical_Robustness.R
library(tidyverse)
library(ggplot2)

# -------------------------------------------------
# Generador de tamaños de familia "empírico INE"
# Opción A (por defecto): soporte {1,2,3,4} con probs fijas.
# Opción B: cola geométrica a partir de K>=3 (activar use_geom_tail=TRUE).
# -------------------------------------------------
rK_empirical <- function(n,
                         p1 = 0.466, p2 = 0.444, p3 = 0.068, p4 = 0.023,
                         use_geom_tail = FALSE,
                         tail_p = 0.75,               # P(K=3 | K>=3) si cola discreta
                         geom_tail_prob = 0.6         # parámetro p para cola geométrica K=3+Geom(p)
) {
  if (!use_geom_tail) {
    # Discreta fija {1,2,3,4}
    ks <- sample(x = c(1,2,3,4), size = n, replace = TRUE,
                 prob = c(p1, p2, p3, p4))
    return(ks)
  } else {
    # Cola geométrica: primero decide si K=1,2 o K>=3
    p_ge3 <- 1 - (p1 + p2)  # masa para K>=3
    base_draw <- runif(n)
    ks <- integer(n)
    for (i in seq_len(n)) {
      u <- base_draw[i]
      if (u < p1) {
        ks[i] <- 1
      } else if (u < p1 + p2) {
        ks[i] <- 2
      } else {
        # K >= 3 con cola geométrica: K = 3 + Geom(geom_tail_prob) truncada a [3, ...)
        # E[K | cola] = 3 + (1-p)/p = 3 + (1-geom_tail_prob)/geom_tail_prob
        ks[i] <- 3 + rgeom(1, prob = geom_tail_prob)
      }
    }
    return(ks)
  }
}

# -------------------------------------------------
# Función para generar dataset con distribución empírica
# -------------------------------------------------
generate_dataset_empirical <- function(num_groups,
                                       students_per_group,
                                       num_courses,
                                       use_geom_tail = FALSE,
                                       probs = c(p1=0.466, p2=0.444, p3=0.068, p4=0.023),
                                       geom_tail_prob = 0.6) {
  # Total de alumnos objetivo
  total_students <- num_groups * students_per_group * num_courses
  
  # --- Paso 1: generar tamaños de familia K >= 1 (school-conditional)
  family_sizes <- c()
  while (sum(family_sizes) < total_students) {
    if (!use_geom_tail) {
      family_size <- rK_empirical(1, p1=probs["p1"], p2=probs["p2"], p3=probs["p3"], p4=probs["p4"])
    } else {
      family_size <- rK_empirical(1, p1=probs["p1"], p2=probs["p2"], p3=probs["p3"], p4=probs["p4"],
                                  use_geom_tail = TRUE, geom_tail_prob = geom_tail_prob)
    }
    family_sizes <- c(family_sizes, family_size)
  }
  
  # Ajustar si nos pasamos del total
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
  
  # --- Paso 2: asignación a cursos y grupos
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
# Parámetros del experimento (tus finales)
# -------------------------------------------------
num_groups_list <- 2:4
students_per_group_list <- c(20, 30, 40)
num_courses <- 9
seed_list <- c(212121, 232323, 252525)

# Empírico base (INE discretizado 1–4 hijos)
probs_emp <- c(p1=0.466, p2=0.444, p3=0.068, p4=0.023)

# Si quieres una variante con cola geométrica (K>=3), activa use_geom_tail y ajusta geom_tail_prob
use_geom_tail_flag <- FALSE
geom_tail_prob <- 0.6  # sólo si use_geom_tail_flag = TRUE

# Carpetas e índice
if (!dir.exists("datasetsRobustness")) dir.create("datasetsRobustness")
if (!dir.exists("results")) dir.create("results")
index_rows <- list(); row_id <- 0

# -------------------------------------------------
# Bucle principal
# -------------------------------------------------
for (num_groups in num_groups_list) {
  for (students_per_group in students_per_group_list) {
    for (seed in seed_list) {
      set.seed(seed)
      
      dataset <- generate_dataset_empirical(
        num_groups = num_groups,
        students_per_group = students_per_group,
        num_courses = num_courses,
        use_geom_tail = use_geom_tail_flag,
        probs = probs_emp,
        geom_tail_prob = geom_tail_prob
      )
      
      # Métricas útiles para el índice
      total_students <- num_groups * students_per_group * num_courses
      total_families <- max(dataset$family_id)
      
      # Media empírica realizada (por si quieres reportarla)
      mu_realized <- dataset %>%
        count(family_id, name = "K") %>%
        summarise(mu = mean(K)) %>% pull(mu)
      
      # Nombre de archivo
      file_name <- paste0(
        "ds_EMP_",
        num_groups, "G_",
        students_per_group, "S_",
        num_courses, "C_",
        ifelse(use_geom_tail_flag, "geomTail_", "fixed_"),
        seed, ".csv"
      )
      
      write.csv(dataset,
                file = file.path("datasetsRobustness", file_name),
                row.names = FALSE)
      
      # Fila del índice
      row_id <- row_id + 1
      index_rows[[row_id]] <- tibble::tibble(
        file = file.path("datasetsRobustness", file_name),
        distribution = ifelse(use_geom_tail_flag, "EMP_geomTail", "EMP_fixed"),
        groups = num_groups,
        students_per_group = students_per_group,
        courses = num_courses,
        seed = seed,
        total_students = total_students,
        total_families = total_families,
        p1 = probs_emp["p1"], p2 = probs_emp["p2"], p3 = probs_emp["p3"], p4 = probs_emp["p4"],
        use_geom_tail = use_geom_tail_flag,
        geom_tail_p = ifelse(use_geom_tail_flag, geom_tail_prob, NA_real_),
        mu_realized = mu_realized
      )
    }
  }
}

index_EMP <- dplyr::bind_rows(index_rows) %>%
  dplyr::arrange(groups, students_per_group, seed)

readr::write_csv(index_EMP, file = file.path("datasetsRobustness", "index_Empirical.csv"))

# -------------------------------------------------
# Visualización: PMF empírica
# -------------------------------------------------
set.seed(123)
n_simulations <- 20000

if (!use_geom_tail_flag) {
  ks <- rK_empirical(n_simulations,
                     p1=probs_emp["p1"], p2=probs_emp["p2"], p3=probs_emp["p3"], p4=probs_emp["p4"])
  df <- tibble(K = ks)
  pmf_emp <- df %>% count(K, name="n") %>% mutate(freq = n/sum(n))
  title_sub <- "Discrete support {1,2,3,4} (INE 2020)"
} else {
  ks <- rK_empirical(n_simulations,
                     p1=probs_emp["p1"], p2=probs_emp["p2"], p3=probs_emp["p3"], p4=probs_emp["p4"],
                     use_geom_tail = TRUE, geom_tail_prob = geom_tail_prob)
  df <- tibble(K = ks)
  pmf_emp <- df %>% count(K, name="n") %>% mutate(freq = n/sum(n))
  title_sub <- paste0("Geometric tail from K>=3 (p=", geom_tail_prob, ")")
}

plot_emp <- ggplot(pmf_emp, aes(x = K, y = freq)) +
  geom_line() +
  geom_point(size = 2) +
  scale_x_continuous(breaks = seq(min(pmf_emp$K), max(pmf_emp$K), by = 1)) +
  labs(
    title = "Empirical Family Size Distribution (School-conditional)",
    subtitle = title_sub,
    x = "K (children per family)",
    y = "Relative Frequency"
  ) +
  theme_minimal()

ggsave("results/family_size_distribution_EMP_Robustness.pdf", plot_emp, width = 8, height = 6)
ggsave("results/family_size_distribution_EMP_Robustness.jpg", plot_emp, width = 8, height = 6)
print(plot_emp)
