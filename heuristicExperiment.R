library(tidyverse)
library(igraph)
library(tidygraph)
library(ggraph)
library(mco)
library(RColorBrewer)
library(writexl)
source("graphFigures.R")

# Crear la carpeta results si no existe
dir.create("results", showWarnings = FALSE)

# Listar todos los archivos en datasets con formato ds_*.csv
files <- list.files("datasets", pattern = "^ds_.*\\.csv$", full.names = TRUE)

# Dataframe para almacenar resultados
results <- tibble(
  filename = character(),
  groups = double(),
  students_per_group = double(),
  courses = double(),
  poisson_param = double(),
  seed = double(),
  n_components_initial = double(),
  n_components_bubble = double(),
  n_components_heuristic = double(),
  max_students_heuristic = double() # Uso max_students_heuristic
)

for (file in files) {
  file_name <- basename(file)
  
  # Extraer los parámetros desde el nombre del archivo
  params_str <- str_remove(file_name, "\\.csv$")
  parts <- str_split(params_str, "_", simplify = TRUE)
  # parts[1] = "ds"
  # parts[2] = numero de grupos
  # parts[3] = alumnos por grupo
  # parts[4] = numero de cursos
  # parts[5] = parametro Poisson
  # parts[6] = semilla
  groups <- parts[2] %>% as.numeric()
  students_per_group <- parts[3] %>% as.numeric()
  courses <- parts[4] %>% as.numeric()
  poisson_param <- parts[5] %>% as.numeric()
  seed <- parts[6] %>% as.numeric()
  
  data <- read_csv(file)
  
  ######################
  # Asignación Inicial #
  ######################
  data_initial <- data %>% 
    mutate(node = paste(course, group, sep = "-"))
  
  edges_initial <- data_initial %>%
    group_by(family_id) %>%
    filter(n() > 1) %>%
    summarise(combinations = list(as.data.frame(t(combn(student_id, 2)))), .groups = "drop") %>%
    unnest(combinations) %>%
    rename(student_id1 = V1, student_id2 = V2) %>%
    inner_join(data_initial, by = c("student_id1" = "student_id")) %>%
    inner_join(data_initial, by = c("student_id2" = "student_id"), suffix = c("_src", "_dst")) %>%
    transmute(from = node_src, to = node_dst)
  
  nodes_initial <- data_initial %>%
    distinct(node, course, group) %>%
    mutate(
      x = as.numeric(as.factor(group)),
      y = as.numeric(as.factor(course)),
      name = node
    )
  
  graph_initial <- tbl_graph(edges = edges_initial, nodes = nodes_initial, directed = FALSE)
  V(graph_initial)$x <- nodes_initial$x
  V(graph_initial)$y <- nodes_initial$y
  V(graph_initial)$name <- nodes_initial$node
  
  n_components_initial <- components(as.igraph(graph_initial))$no
  
  plot_initial <- crear_y_graficar_grafo(graph_initial, "Initial assignment")
  # Guardar en JPG y PDF
  ggsave(filename = file.path("results", paste0(params_str, "_initial.jpg")), plot = plot_initial, width = 8, height = 6)
  ggsave(filename = file.path("results", paste0(params_str, "_initial.pdf")), plot = plot_initial, width = 8, height = 6)
  
  ##########################
  # Burbuja Estándar       #
  ##########################
  data_bubble <- data %>% 
    mutate(node = paste(course, group, sep = "-"))
  
  edges_bubble <- data_bubble %>%
    group_by(family_id) %>%
    filter(n() > 1) %>%
    summarise(combinations = list(as.data.frame(t(combn(student_id, 2)))), .groups = "drop") %>%
    unnest(combinations) %>%
    rename(student_id1 = V1, student_id2 = V2) %>%
    inner_join(data_bubble, by = c("student_id1" = "student_id")) %>%
    inner_join(data_bubble, by = c("student_id2" = "student_id"), suffix = c("_src", "_dst")) %>%
    mutate(
      same_course = course_src == course_dst,
      diff_group_same_course = same_course & group_src != group_dst
    ) %>%
    transmute(
      from = if_else(diff_group_same_course, node_src, node_src),
      to = if_else(diff_group_same_course, node_src, node_dst)
    )
  
  nodes_bubble <- data_bubble %>%
    distinct(node, course, group) %>%
    mutate(
      x = as.numeric(as.factor(group)),
      y = as.numeric(as.factor(course)),
      name = node
    )
  
  graph_bubble <- tbl_graph(edges = edges_bubble, nodes = nodes_bubble, directed = FALSE)
  V(graph_bubble)$x <- nodes_bubble$x
  V(graph_bubble)$y <- nodes_bubble$y
  V(graph_bubble)$name <- nodes_bubble$node
  
  n_components_bubble <- components(as.igraph(graph_bubble))$no
  
  plot_bubble <- crear_y_graficar_grafo(graph_bubble, "Standard bubble")
  ggsave(filename = file.path("results", paste0(params_str, "_bubble.jpg")), plot = plot_bubble, width = 8, height = 6)
  ggsave(filename = file.path("results", paste0(params_str, "_bubble.pdf")), plot = plot_bubble, width = 8, height = 6)
  
  ##########################
  # Asignación Heurística #
  ##########################
  
  # Determinamos grupos por curso a partir de los datos
  groups_per_course <- data_bubble %>%
    group_by(course) %>%
    summarise(groups_avail = list(unique(group)), .groups = "drop") %>%
    deframe()
  
  families_courses <- data_bubble %>%
    group_by(family_id) %>%
    summarise(courses = list(unique(course)), .groups = "drop")
  
  families_groups <- families_courses %>%
    mutate(
      common_groups = map(courses, ~ {
        course_vector <- .x
        group_lists <- lapply(course_vector, function(cc) groups_per_course[[as.character(cc)]])
        Reduce(intersect, group_lists)
      })
    ) %>%
    rowwise() %>%
    mutate(
      assigned_group = if (length(common_groups) > 0) {
        sample(common_groups, 1)
      } else {
        first_course <- courses[[1]]
        sample(groups_per_course[[as.character(first_course)]], 1)
      }
    ) %>%
    ungroup()
  
  data_assigned <- data_bubble %>%
    left_join(families_groups %>% select(family_id, assigned_group), by = "family_id") %>%
    mutate(
      group = assigned_group,
      node = paste(course, group, sep = "-")
    ) %>%
    select(-assigned_group)
  
  edges_heuristic <- data_assigned %>%
    group_by(family_id) %>%
    filter(n() > 1) %>%
    summarise(combinations = list(as.data.frame(t(combn(student_id, 2)))), .groups = "drop") %>%
    unnest(combinations) %>%
    rename(student_id1 = V1, student_id2 = V2) %>%
    inner_join(data_assigned, by = c("student_id1" = "student_id")) %>%
    inner_join(data_assigned, by = c("student_id2" = "student_id"), suffix = c("_src", "_dst")) %>%
    transmute(from = node_src, to = node_dst)
  
  nodes_heuristic <- data_assigned %>%
    distinct(node, course, group) %>%
    mutate(
      x = as.numeric(as.factor(group)),
      y = as.numeric(as.factor(course)),
      name = node
    )
  
  graph_heuristic <- graph_from_data_frame(edges_heuristic, vertices = nodes_heuristic, directed = FALSE)
  
  n_components_heuristic <- components(graph_heuristic)$no
  
  # Filtrar solo alumnos con hermanos (familias con más de un estudiante)
  data_assigned_siblings <- data_assigned %>%
    group_by(family_id) %>%
    filter(n() > 1) %>%
    ungroup()
  
  # Calcular el máximo número de alumnos asignados a un grupo y curso
  # considerando solo alumnos con hermanos.
  max_students <- data_assigned_siblings %>%
    group_by(course, group) %>%
    summarise(n_students = n(), .groups = "drop") %>%
    pull(n_students) %>%
    max(., na.rm = TRUE) # Se usa na.rm=TRUE por si no hay nodos con hermanos
  
  plot_heuristic <- crear_y_graficar_grafo(graph_heuristic, "Allocation obtained using the heuristic algorithm")
  ggsave(filename = file.path("results", paste0(params_str, "_heuristic.jpg")), plot = plot_heuristic, width = 8, height = 6)
  ggsave(filename = file.path("results", paste0(params_str, "_heuristic.pdf")), plot = plot_heuristic, width = 8, height = 6)
  
  # Agregar resultados a la tabla
  results <- results %>%
    add_row(
      filename = file_name,
      groups = groups,
      students_per_group = students_per_group,
      courses = courses,
      poisson_param = poisson_param,
      seed = seed,
      n_components_initial = n_components_initial,
      n_components_bubble = n_components_bubble,
      n_components_heuristic = n_components_heuristic,
      max_students_heuristic = max_students
    )
}

# Guardar el resumen de resultados
write_csv(results, "results/results_summary.csv")


#####################################################################


# Leer el archivo results_summary.csv
results <- read_csv("results/results_summary.csv")

# Calcular la media y varianza de los componentes por estrategia y agrupados por poisson_param y groups
grouped <- results %>%
  group_by(poisson_param, groups) %>%
  summarise(
    mean_initial = mean(n_components_initial, na.rm = TRUE),
    var_initial = var(n_components_initial, na.rm = TRUE),
    mean_bubble = mean(n_components_bubble, na.rm = TRUE),
    var_bubble = var(n_components_bubble, na.rm = TRUE),
    mean_heuristic = mean(n_components_heuristic, na.rm = TRUE),
    var_heuristic = var(n_components_heuristic, na.rm = TRUE),
    mean_max_students_heuristic = mean(max_students_heuristic, na.rm = TRUE),
    .groups = "drop"
  )

# Preparar datos en formato largo para las estrategias
data_long <- grouped %>%
  pivot_longer(
    cols = c(mean_initial, mean_bubble, mean_heuristic),
    names_to = "strategy",
    values_to = "mean_components"
  ) %>%
  mutate(
    strategy_var = case_when(
      strategy == "mean_initial" ~ var_initial,
      strategy == "mean_bubble" ~ var_bubble,
      strategy == "mean_heuristic" ~ var_heuristic
    )
  )

# Etiquetas personalizadas para las estrategias
strategy_labels <- c(
  mean_initial = "Mean initial strategy",
  mean_bubble = "Mean bubble strategy",
  mean_heuristic = "Mean heuristic strategy"
)


# Crear el gráfico
my_plot <- ggplot() +
  # Gráfico de componentes medios con ajuste loess para intervalos de confianza
  geom_smooth(
    data = data_long,
    aes(x = poisson_param, y = mean_components, color = strategy, fill = strategy),
    method = "loess",
    se = TRUE,
    alpha = 0.2
  ) +
  
  # Gráfico de max_students_heuristic (eje derecho)
  geom_point(data = grouped, aes(x = poisson_param, y = mean_max_students_heuristic / 5), color = "purple", size = 2) +
  geom_smooth(
    data = grouped,
    aes(x = poisson_param, y = mean_max_students_heuristic / 5),
    method = "loess",
    color = "purple",
    se = TRUE
  ) +
  
  # Escala secundaria en el eje derecho
  scale_y_continuous(
    name = "Average Components",
    limits = c(0, 7),  # Límites del eje izquierdo
    sec.axis = sec_axis(~ . * 5, name = "Max Students (Heuristic)", breaks = seq(0, 30, 5)) # Límites del eje derecho
  ) +
  
  # Línea horizontal en el eje derecho (convertida a escala secundaria)
  geom_hline(yintercept = 20 / 5, linetype = "dashed", color = "red") +
  
  # Etiquetas personalizadas en la leyenda
  scale_color_manual(
    values = c("mean_initial" = "blue", "mean_bubble" = "green", "mean_heuristic" = "orange"),
    labels = strategy_labels
  ) +
  scale_fill_manual(
    values = c("mean_initial" = "blue", "mean_bubble" = "green", "mean_heuristic" = "orange"),
    labels = strategy_labels
  ) +
  
  # Segmentación por groups
  facet_wrap(~groups, scales = "fixed", ncol = 1) +
  
  # Etiquetas
  labs(
    title = "Average Components and Maximum Assigned Students",
    x = "Lambda (Poisson distribution)",
    color = "Strategy",
    fill = "Strategy",
  ) +
  
  # Tema
  theme_minimal() +
  theme(
    legend.position = "bottom",
    strip.text = element_text(size = 12, face = "bold")
  )

# Exportar el gráfico en PDF
ggsave("results/Average_Components_and_Max_Students.pdf", plot = my_plot, device = "pdf", width = 8, height = 10)

# Exportar el gráfico en JPG
ggsave("results/Average_Components_and_Max_Students.jpg", plot = my_plot, device = "jpeg", width = 8, height = 10, dpi = 300)
