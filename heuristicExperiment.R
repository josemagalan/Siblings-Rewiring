# Load required libraries
library(tidyverse)
library(igraph)
library(tidygraph)
library(ggraph)
library(mco)
library(RColorBrewer)
library(writexl)

# Load external function definitions
source("graphFigures.R")
source("nivelacion.R")

# Create the output folder if it doesn't exist
dir.create("results", showWarnings = FALSE)

# List all dataset files starting with 'ds_'
files <- list.files("datasets", pattern = "^ds_.*\\.csv$", full.names = TRUE)

# Initialize results data frame
results <- tibble(
  filename = character(),
  groups = double(),
  students_per_group = double(),
  courses = double(),
  poisson_param = double(),
  seed = double(),
  n_components_initial = double(),
  var_components_initial = double(),
  n_components_bubble = double(),
  var_components_bubble = double(),
  n_components_heuristic = double(),
  n_components_heuristic_balanced = double(),
  var_components_heuristic_balanced = double(),
  max_students_heuristic = double(),
  max_students_balanced = double()
)

# Iterate over all dataset files
for (file in files) {
  file_name <- basename(file)
  
  # Extract parameters from file name
  params_str <- str_remove(file_name, "\\.csv$")
  parts <- str_split(params_str, "_", simplify = TRUE)
  groups <- parts[2] %>% as.numeric()
  students_per_group <- parts[3] %>% as.numeric()
  courses <- parts[4] %>% as.numeric()
  poisson_param <- parts[5] %>% as.numeric()
  seed <- parts[6] %>% as.numeric()
  
  # Read CSV file
  data <- read_csv(file, col_types = cols(sibling_ids = col_character()))
  
  ##############################
  # Initial Assignment Analysis
  ##############################
  data_initial <- data %>% 
    mutate(node = paste(course, group, sep = "-"))
  
  # Create edges between students in the same family
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
  
  # Build graph
  graph_initial <- tbl_graph(edges = edges_initial, nodes = nodes_initial, directed = FALSE)
  V(graph_initial)$x <- nodes_initial$x
  V(graph_initial)$y <- nodes_initial$y
  V(graph_initial)$name <- nodes_initial$node
  
  # Compute number of components and their variance
  n_components_initial <- components(as.igraph(graph_initial))$no
  comp_initial <- components(as.igraph(graph_initial))
  component_sizes_initial <- comp_initial$csize
  var_components_initial <- if (n_components_initial > 1) var(component_sizes_initial) else 0
  
  # Plot and save initial assignment
  plot_initial <- crear_y_graficar_grafo(graph_initial, "Initial assignment")
  ggsave(filename = file.path("results", paste0(params_str, "_initial.jpg")), plot = plot_initial, width = 8, height = 6)
  ggsave(filename = file.path("results", paste0(params_str, "_initial.pdf")), plot = plot_initial, width = 8, height = 6)
  
  ##############################
  # Standard Bubble Assignment
  ##############################
  data_bubble <- data %>%
    mutate(node = paste(course, group, sep = "-"))
  
  # Create edges: stronger links for siblings in same course but different groups
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
  
  # Build bubble graph
  graph_bubble <- tbl_graph(edges = edges_bubble, nodes = nodes_bubble, directed = FALSE)
  V(graph_bubble)$x <- nodes_bubble$x
  V(graph_bubble)$y <- nodes_bubble$y
  V(graph_bubble)$name <- nodes_bubble$node
  
  n_components_bubble <- components(as.igraph(graph_bubble))$no
  comp_bubble <- components(as.igraph(graph_bubble))
  component_sizes_bubble <- comp_bubble$csize
  var_components_bubble <- if (n_components_bubble > 1) var(component_sizes_bubble) else 0
  
  # Plot and save
  plot_bubble <- crear_y_graficar_grafo(graph_bubble, "Standard bubble")
  ggsave(filename = file.path("results", paste0(params_str, "_bubble.jpg")), plot = plot_bubble, width = 8, height = 6)
  ggsave(filename = file.path("results", paste0(params_str, "_bubble.pdf")), plot = plot_bubble, width = 8, height = 6)
  
  ##############################
  # Heuristic Assignment
  ##############################
  # Determine available groups per course
  groups_per_course <- data_bubble %>%
    group_by(course) %>%
    summarise(groups_avail = list(unique(group)), .groups = "drop") %>%
    deframe()
  
  families_courses <- data_bubble %>%
    group_by(family_id) %>%
    summarise(courses = list(unique(course)), .groups = "drop")
  
  # Assign group to families considering shared availability
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
  
  # Apply heuristic group assignment to data
  data_assigned <- data_bubble %>%
    left_join(families_groups %>% select(family_id, assigned_group), by = "family_id") %>%
    mutate(
      group = assigned_group,
      node = paste(course, group, sep = "-")
    ) %>%
    select(-assigned_group)
  
  # Build graph from heuristic assignment
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
  
  # Compute maximum group size for students with siblings
  data_assigned_siblings <- data_assigned %>%
    group_by(family_id) %>%
    filter(n() > 1) %>%
    ungroup()
  
  max_students <- data_assigned_siblings %>%
    group_by(course, group) %>%
    summarise(n_students = n(), .groups = "drop") %>%
    pull(n_students) %>%
    max(., na.rm = TRUE)
  
  # Plot and save heuristic assignment
  plot_heuristic <- crear_y_graficar_grafo(graph_heuristic, "Allocation obtained using the heuristic algorithm")
  ggsave(filename = file.path("results", paste0(params_str, "_heuristic.jpg")), plot = plot_heuristic, width = 8, height = 6)
  ggsave(filename = file.path("results", paste0(params_str, "_heuristic.pdf")), plot = plot_heuristic, width = 8, height = 6)
  
  ##############################
  # Balanced Heuristic Assignment
  ##############################
  datosNivelados <- Ajustar_Exacto_Nivelacion(data_assigned)
  datosNivelados <- datosNivelados %>%
    mutate(node = paste(course, group, sep = "-"))
  
  # Save leveled assignment as RDS and CSV
  saveRDS(datosNivelados, file.path("results", paste0(params_str, "_datosNivelados.rds")))
  write_csv(datosNivelados, file.path("results", paste0(params_str, "_datosNivelados.csv")))
  
  # Build graph from leveled assignment
  edges_heuristic_balanced <- datosNivelados %>%
    group_by(family_id) %>%
    filter(n() > 1) %>%
    summarise(combinations = list(as.data.frame(t(combn(student_id, 2)))), .groups = "drop") %>%
    unnest(combinations) %>%
    rename(student_id1 = V1, student_id2 = V2) %>%
    inner_join(data_assigned, by = c("student_id1" = "student_id")) %>%
    inner_join(data_assigned, by = c("student_id2" = "student_id"), suffix = c("_src", "_dst")) %>%
    transmute(from = node_src, to = node_dst)
  
  nodes_heuristic_balanced <- datosNivelados %>%
    distinct(node, course, group) %>%
    mutate(
      x = as.numeric(as.factor(group)),
      y = as.numeric(as.factor(course)),
      name = node
    )
  
  graph_heuristic_balanced <- graph_from_data_frame(edges_heuristic_balanced, vertices = nodes_heuristic_balanced, directed = FALSE)
  components_heuristic_balanced <- components(graph_heuristic_balanced)
  n_components_heuristic_balanced <- components_heuristic_balanced$no
  
  component_sizes_heuristic_balanced <- components_heuristic_balanced$csize
  var_components_heuristic_balanced <- if (n_components_heuristic_balanced > 1) var(component_sizes_heuristic_balanced) else 0
  
  # Compute max students per group after balancing
  data_assigned_siblings <- datosNivelados %>%
    group_by(family_id) %>%
    filter(n() > 1) %>%
    ungroup()
  
  max_students_balanced <- data_assigned_siblings %>%
    group_by(course, group) %>%
    summarise(n_students = n(), .groups = "drop") %>%
    pull(n_students) %>%
    max(., na.rm = TRUE)
  
  # Plot and save leveled assignment
  plot_heuristic <- crear_y_graficar_grafo(graph_heuristic_balanced, "Allocation obtained using the heuristic algorithm and greedy balancing")
  ggsave(filename = file.path("results", paste0(params_str, "_heuristic_balanced.jpg")), plot = plot_heuristic, width = 8, height = 6)
  ggsave(filename = file.path("results", paste0(params_str, "_heuristic_balanced.pdf")), plot = plot_heuristic, width = 8, height = 6)
  
  # Append row to results table
  results <- results %>%
    add_row(
      filename = file_name,
      groups = groups,
      students_per_group = students_per_group,
      courses = courses,
      poisson_param = poisson_param,
      seed = seed,
      n_components_initial = n_components_initial,
      var_components_initial = var_components_initial,
      n_components_bubble = n_components_bubble,
      var_components_bubble = var_components_bubble,
      n_components_heuristic = n_components_heuristic,
      n_components_heuristic_balanced = n_components_heuristic_balanced,
      var_components_heuristic_balanced = var_components_heuristic_balanced,
      max_students_heuristic = max_students,
      max_students_balanced = max_students_balanced
    )
}

# Save summary of results
write_csv(results, "results/results_summary.csv")


#####################################################################
# Aggregate and visualize the summary of experimental results
#####################################################################

# Read the summary CSV with all experiment results
results <- read_csv("results/results_summary.csv")

# Compute mean and variance of components per strategy,
# grouped by Poisson parameter and number of groups
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

# Convert the data to long format for easier plotting
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

# Define custom labels for plotting
strategy_labels <- c(
  mean_initial = "Mean initial strategy",
  mean_bubble = "Mean bubble strategy",
  mean_heuristic = "Mean heuristic strategy"
)

# Create the combined plot
my_plot <- ggplot() +
  # Smoothed trend of average components with confidence bands
  geom_smooth(
    data = data_long,
    aes(x = poisson_param, y = mean_components, color = strategy, fill = strategy),
    method = "loess",
    se = TRUE,
    alpha = 0.2
  ) +
  
  # Add points for max students in heuristic solution (rescaled)
  geom_point(data = grouped, aes(x = poisson_param, y = mean_max_students_heuristic / 5), color = "purple", size = 2) +
  geom_smooth(
    data = grouped,
    aes(x = poisson_param, y = mean_max_students_heuristic / 5),
    method = "loess",
    color = "purple",
    se = TRUE
  ) +
  
  # Set primary and secondary y-axes
  scale_y_continuous(
    name = "Average Components",
    limits = c(0, 7),
    sec.axis = sec_axis(~ . * 5, name = "Max Students (Heuristic)", breaks = seq(0, 30, 5))
  ) +
  
  # Add dashed reference line for the target max group size (20 students)
  geom_hline(yintercept = 20 / 5, linetype = "dashed", color = "red") +
  
  # Customize legend labels
  scale_color_manual(
    values = c("mean_initial" = "blue", "mean_bubble" = "green", "mean_heuristic" = "orange"),
    labels = strategy_labels
  ) +
  scale_fill_manual(
    values = c("mean_initial" = "blue", "mean_bubble" = "green", "mean_heuristic" = "orange"),
    labels = strategy_labels
  ) +
  
  # Create facets by number of groups
  facet_wrap(~groups, scales = "fixed", ncol = 1) +
  
  # Add titles and axis labels
  labs(
    title = "Average Components and Maximum Assigned Students",
    x = "Lambda (Poisson distribution)",
    color = "Strategy",
    fill = "Strategy"
  ) +
  
  # Apply a minimal theme
  theme_minimal() +
  theme(
    legend.position = "bottom",
    strip.text = element_text(size = 12, face = "bold")
  )

# Export plot to PDF and JPG
ggsave("results/Average_Components_and_Max_Students.pdf", plot = my_plot, device = "pdf", width = 8, height = 10)
ggsave("results/Average_Components_and_Max_Students.jpg", plot = my_plot, device = "jpeg", width = 8, height = 10, dpi = 300)
