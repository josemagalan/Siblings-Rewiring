# Adapted for GEOM / NEGBIN / EMP datasets located in datasetsRobustness/

library(tidyverse)
library(igraph)
library(tidygraph)
library(ggraph)
library(mco)
library(RColorBrewer)
library(writexl)
library(stringr)
library(purrr)

# External helper functions (graph styling and leveling/assignment utilities)
source("graphFigures.R")
source("nivelacion.R")

# Output folders
dir.create("resultsRobustness", showWarnings = FALSE)

# -------------------------------------------------
# Locate robustness dataset files
# -------------------------------------------------
files <- list.files("datasetsRobustness", pattern = "^ds_(GEOM|NEGBIN|EMP)_.*\\.csv$", full.names = TRUE)

# -------------------------------------------------
# Helper: parse parameters from the file name
# Expected patterns:
# GEOM:   ds_GEOM_{G}G_{S}S_{C}C_mu{mu}_p{p}_{seed}.csv
# NEGBIN: ds_NEGBIN_{G}G_{S}S_{C}C_mu{mu}_r{r}_p{p}_{seed}.csv
# EMP:    ds_EMP_{G}G_{S}S_{C}C_{fixed|geomTail}_{seed}.csv
# -------------------------------------------------
parse_params <- function(file_path) {
  fn <- basename(file_path)
  base <- str_remove(fn, "\\.csv$")
  parts <- str_split(base, "_", simplify = TRUE)
  
  dist <- parts[2]
  
  # Common prefix: {G}G_{S}S_{C}C
  groups <- as.numeric(str_remove(parts[3], "G$"))
  students_per_group <- as.numeric(str_remove(parts[4], "S$"))
  courses <- as.numeric(str_remove(parts[5], "C$"))
  
  # Initialize optional parameters (distribution-specific)
  mu_target <- NA_real_
  r_disp <- NA_real_
  p_param <- NA_real_
  emp_type <- NA_character_
  seed <- NA_real_
  
  if (dist == "GEOM") {
    mu_target <- as.numeric(str_remove(parts[6], "^mu"))
    p_param   <- as.numeric(str_remove(parts[7], "^p"))
    seed      <- as.numeric(parts[8])
  } else if (dist == "NEGBIN") {
    mu_target <- as.numeric(str_remove(parts[6], "^mu"))
    r_disp    <- as.numeric(str_remove(parts[7], "^r"))
    p_param   <- as.numeric(str_remove(parts[8], "^p"))
    seed      <- as.numeric(parts[9])
  } else if (dist == "EMP") {
    emp_type  <- parts[6]                # fixed | geomTail
    seed      <- as.numeric(parts[7])
  }
  
  tibble(
    filename = fn,
    fullpath = file_path,
    distribution = dist,
    emp_type = emp_type,
    groups = groups,
    students_per_group = students_per_group,
    courses = courses,
    mu_target = mu_target,
    r = r_disp,
    p = p_param,
    seed = seed
  )
}

params_df <- map_dfr(files, parse_params)

# -------------------------------------------------
# Results data frame (will be filled within the loop)
# -------------------------------------------------
results <- tibble(
  filename = character(),
  distribution = character(),
  emp_type = character(),
  groups = double(),
  students_per_group = double(),
  courses = double(),
  mu_target = double(),
  mu_realized = double(),
  r = double(),
  p = double(),
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

# -------------------------------------------------
# Main analysis loop (same pipeline; only the filename parsing differs)
# -------------------------------------------------
for (i in seq_len(nrow(params_df))) {
  file <- params_df$fullpath[i]
  meta <- params_df[i, ]
  
  data <- readr::read_csv(file, col_types = cols(sibling_ids = col_character()))
  file_name <- meta$filename
  params_str <- str_remove(file_name, "\\.csv$")
  
  # Realized μ (sanity check; also used for EMP datasets)
  mu_realized <- data %>%
    count(family_id, name = "K") %>%
    summarise(mu = mean(K)) %>% pull(mu)
  
  ##############################
  # Initial Assignment
  ##############################
  data_initial <- data %>% mutate(node = paste(course, group, sep = "-"))
  
  # Build edges by connecting siblings (pairs within a family)
  edges_initial <- data_initial %>%
    group_by(family_id) %>%
    filter(n() > 1) %>%
    summarise(comb = list(as.data.frame(t(combn(student_id, 2)))), .groups = "drop") %>%
    unnest(comb) %>%
    rename(student_id1 = V1, student_id2 = V2) %>%
    inner_join(data_initial, by = c("student_id1" = "student_id")) %>%
    inner_join(data_initial, by = c("student_id2" = "student_id"), suffix = c("_src", "_dst")) %>%
    transmute(from = node_src, to = node_dst)
  
  # Nodes are (course, group) cells with fixed layout coordinates
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
  
  comp_init <- components(as.igraph(graph_initial))
  n_components_initial <- comp_init$no
  component_sizes_initial <- comp_init$csize
  var_components_initial <- if (n_components_initial > 1) var(component_sizes_initial) else 0
  
  plot_initial <- crear_y_graficar_grafo(graph_initial, "Initial assignment")
  ggsave(filename = file.path("results", paste0(params_str, "_initial.jpg")), plot = plot_initial, width = 8, height = 6)
  ggsave(filename = file.path("results", paste0(params_str, "_initial.pdf")), plot = plot_initial, width = 8, height = 6)
  
  ##############################
  # Standard Bubble
  ##############################
  data_bubble <- data %>% mutate(node = paste(course, group, sep = "-"))
  
  # Edges: if siblings are in the same course but different groups,
  # collapse the link onto the same node (bubble logic); otherwise, keep original link
  edges_bubble <- data_bubble %>%
    group_by(family_id) %>%
    filter(n() > 1) %>%
    summarise(comb = list(as.data.frame(t(combn(student_id, 2)))), .groups = "drop") %>%
    unnest(comb) %>%
    rename(student_id1 = V1, student_id2 = V2) %>%
    inner_join(data_bubble, by = c("student_id1" = "student_id")) %>%
    inner_join(data_bubble, by = c("student_id2" = "student_id"), suffix = c("_src", "_dst")) %>%
    mutate(
      same_course = course_src == course_dst,
      diff_group_same_course = same_course & group_src != group_dst
    ) %>%
    transmute(
      from = if_else(diff_group_same_course, node_src, node_src),
      to   = if_else(diff_group_same_course, node_src, node_dst)
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
  
  comp_bub <- components(as.igraph(graph_bubble))
  n_components_bubble <- comp_bub$no
  component_sizes_bubble <- comp_bub$csize
  var_components_bubble <- if (n_components_bubble > 1) var(component_sizes_bubble) else 0
  
  plot_bubble <- crear_y_graficar_grafo(graph_bubble, "Standard bubble")
  ggsave(filename = file.path("results", paste0(params_str, "_bubble.jpg")), plot = plot_bubble, width = 8, height = 6)
  ggsave(filename = file.path("results", paste0(params_str, "_bubble.pdf")), plot = plot_bubble, width = 8, height = 6)
  
  ##############################
  # Heuristic
  ##############################
  # For each course, list available groups (used to assign a single common group to each family)
  groups_per_course <- data_bubble %>%
    group_by(course) %>%
    summarise(groups_avail = list(unique(group)), .groups = "drop") %>%
    deframe()
  
  # Courses attended by each family
  families_courses <- data_bubble %>%
    group_by(family_id) %>%
    summarise(courses = list(unique(course)), .groups = "drop")
  
  # Assign an "agreed" group to the whole family:
  # if there is an intersection of groups across their courses, pick one at random;
  # otherwise, pick a random group from the first course.
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
  
  # Apply the family-wise group assignment
  data_assigned <- data_bubble %>%
    left_join(families_groups %>% select(family_id, assigned_group), by = "family_id") %>%
    mutate(group = assigned_group,
           node  = paste(course, group, sep = "-")) %>%
    select(-assigned_group)
  
  # Build edges after heuristic allocation (siblings paired within families)
  edges_heuristic <- data_assigned %>%
    group_by(family_id) %>%
    filter(n() > 1) %>%
    summarise(comb = list(as.data.frame(t(combn(student_id, 2)))), .groups = "drop") %>%
    unnest(comb) %>%
    rename(student_id1 = V1, student_id2 = V2) %>%
    inner_join(data_assigned, by = c("student_id1" = "student_id")) %>%
    inner_join(data_assigned, by = c("student_id2" = "student_id"), suffix = c("_src", "_dst")) %>%
    transmute(from = node_src, to = node_dst)
  
  # Nodes (with fixed layout) for the heuristic graph
  nodes_heuristic <- data_assigned %>%
    distinct(node, course, group) %>%
    mutate(x = as.numeric(as.factor(group)),
           y = as.numeric(as.factor(course)),
           name = node)
  
  graph_heuristic <- graph_from_data_frame(edges_heuristic, vertices = nodes_heuristic, directed = FALSE)
  n_components_heuristic <- components(graph_heuristic)$no
  
  # Only families with >1 sibling to compute group loads
  data_assigned_siblings <- data_assigned %>%
    group_by(family_id) %>% filter(n() > 1) %>% ungroup()
  
  # Max number of assigned siblings in a single (course, group) cell
  max_students <- data_assigned_siblings %>%
    group_by(course, group) %>%
    summarise(n_students = n(), .groups = "drop") %>%
    pull(n_students) %>%
    max(., na.rm = TRUE)
  
  plot_heuristic <- crear_y_graficar_grafo(graph_heuristic, "Allocation obtained using the heuristic algorithm")
  ggsave(filename = file.path("results", paste0(params_str, "_heuristic.jpg")), plot = plot_heuristic, width = 8, height = 6)
  ggsave(filename = file.path("results", paste0(params_str, "_heuristic.pdf")), plot = plot_heuristic, width = 8, height = 6)
  
  ##############################
  # Balanced Heuristic
  ##############################
  # Apply exact balancing procedure and re-compute nodes
  datosNivelados <- Ajustar_Exacto_Nivelacion(data_assigned) %>%
    mutate(node = paste(course, group, sep = "-"))
  
  saveRDS(datosNivelados, file.path("results", paste0(params_str, "_datosNivelados.rds")))
  readr::write_csv(datosNivelados, file.path("results", paste0(params_str, "_datosNivelados.csv")))
  
  # Edges for the balanced allocation (siblings paired after balancing)
  edges_heuristic_balanced <- datosNivelados %>%
    group_by(family_id) %>%
    filter(n() > 1) %>%
    summarise(comb = list(as.data.frame(t(combn(student_id, 2)))), .groups = "drop") %>%
    unnest(comb) %>%
    rename(student_id1 = V1, student_id2 = V2) %>%
    inner_join(data_assigned, by = c("student_id1" = "student_id")) %>%
    inner_join(data_assigned, by = c("student_id2" = "student_id"), suffix = c("_src", "_dst")) %>%
    transmute(from = node_src, to = node_dst)
  
  # Nodes (with fixed layout) for the balanced heuristic graph
  nodes_heuristic_balanced <- datosNivelados %>%
    distinct(node, course, group) %>%
    mutate(x = as.numeric(as.factor(group)),
           y = as.numeric(as.factor(course)),
           name = node)
  
  graph_heuristic_balanced <- graph_from_data_frame(edges_heuristic_balanced, vertices = nodes_heuristic_balanced, directed = FALSE)
  comp_hb <- components(graph_heuristic_balanced)
  n_components_heuristic_balanced <- comp_hb$no
  var_components_heuristic_balanced <- if (n_components_heuristic_balanced > 1) var(comp_hb$csize) else 0
  
  # Only families with >1 sibling to compute group loads (balanced)
  data_assigned_siblings2 <- datosNivelados %>%
    group_by(family_id) %>% filter(n() > 1) %>% ungroup()
  
  max_students_balanced <- data_assigned_siblings2 %>%
    group_by(course, group) %>%
    summarise(n_students = n(), .groups = "drop") %>%
    pull(n_students) %>%
    max(., na.rm = TRUE)
  
  # Append a results row for this dataset instance
  results <- results %>% add_row(
    filename = file_name,
    distribution = meta$distribution,
    emp_type = meta$emp_type %||% NA_character_,
    groups = meta$groups,
    students_per_group = meta$students_per_group,
    courses = meta$courses,
    mu_target = meta$mu_target %||% NA_real_,
    mu_realized = mu_realized,
    r = meta$r %||% NA_real_,
    p = meta$p %||% NA_real_,
    seed = meta$seed,
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

# -------------------------------------------------
# Save per-instance summary table
# -------------------------------------------------
readr::write_csv(results, "resultsRobustness/results_summary_robustness.csv")

#####################################################################
# Aggregation and visualization (adapted for robustness datasets)
#####################################################################
results <- readr::read_csv("resultsRobustness/results_summary_robustness.csv", show_col_types = FALSE)

# Use an effective μ on the x-axis: prefer mu_target, fallback to realized μ
results <- results %>%
  mutate(mu_effective = ifelse(!is.na(mu_target), mu_target, mu_realized))

# Aggregate per distribution, μ, and configuration
grouped <- results %>%
  group_by(distribution, mu_effective, groups, students_per_group) %>%
  summarise(
    mean_initial = mean(n_components_initial, na.rm = TRUE),
    var_initial  = var(n_components_initial, na.rm = TRUE),
    mean_bubble  = mean(n_components_bubble, na.rm = TRUE),
    var_bubble   = var(n_components_bubble, na.rm = TRUE),
    mean_heuristic = mean(n_components_heuristic, na.rm = TRUE),
    var_heuristic  = var(n_components_heuristic, na.rm = TRUE),
    mean_max_students_heuristic = mean(max_students_heuristic, na.rm = TRUE),
    .groups = "drop"
  )

# Long-format for plotting the strategy trends
data_long <- grouped %>%
  pivot_longer(
    cols = c(mean_initial, mean_bubble, mean_heuristic),
    names_to = "strategy",
    values_to = "mean_components"
  ) %>%
  mutate(
    strategy_var = case_when(
      strategy == "mean_initial"  ~ var_initial,
      strategy == "mean_bubble"   ~ var_bubble,
      strategy == "mean_heuristic"~ var_heuristic
    ),
    strategy = factor(strategy,
                      levels = c("mean_initial","mean_bubble","mean_heuristic"),
                      labels = c("Initial","Bubble","Heuristic"))
  )

# Color palette for strategies (optional)
cols <- c("Initial"="#1f77b4","Bubble"="#2ca02c","Heuristic"="#ff7f0e")

# Figure:
# - x = mu_effective
# - facets by groups × students_per_group
# - purple series (secondary axis) shows mean_max_students_heuristic / 5
my_plot <- ggplot() +
  geom_smooth(
    data = data_long,
    aes(x = mu_effective, y = mean_components, color = strategy, fill = strategy),
    method = "loess", se = TRUE, alpha = 0.2
  ) +
  geom_point(
    data = grouped,
    aes(x = mu_effective, y = mean_max_students_heuristic / 5),
    color = "purple", size = 2
  ) +
  geom_smooth(
    data = grouped,
    aes(x = mu_effective, y = mean_max_students_heuristic / 5),
    method = "loess", color = "purple", se = TRUE
  ) +
  scale_y_continuous(
    name = "Average Components",
    sec.axis = sec_axis(~ . * 5, name = "Max Students (Heuristic)")
  ) +
  scale_color_manual(values = cols) +
  scale_fill_manual(values = cols) +
  facet_grid(groups ~ students_per_group, labeller = labeller(
    groups = function(g) paste("Groups:", g),
    students_per_group = function(s) paste("Group size:", s)
  )) +
  labs(
    title = "Average Components and Maximum Assigned Students (Robustness datasets)",
    subtitle = "By distribution, μ and school capacity",
    x = "μ (shifted family-size mean)",
    color = "Strategy",
    fill = "Strategy"
  ) +
  theme_minimal(base_size = 11) +
  theme(
    legend.position = "bottom",
    strip.text = element_text(size = 10, face = "bold")
  )

ggsave("resultsRobustness/Average_Components_and_Max_Students_ROBUSTNESS.pdf", plot = my_plot, device = "pdf", width = 11, height = 10)
ggsave("resultsRobustness/Average_Components_and_Max_Students_ROBUSTNESS.jpg", plot = my_plot, device = "jpeg", width = 11, height = 10, dpi = 300)

# Note: the dashed red line per facet (students_per_group) can be overlaid using a facet-aware data frame
# with y = students_per_group/5 and geom_hline(data = ...). Below, 'ref_lines' is prepared for that.
ref_lines <- grouped %>%
  distinct(groups, students_per_group) %>%
  mutate(y_line = students_per_group / 5)

my_plot2 <- ggplot() +
  geom_smooth(
    data = data_long,
    aes(x = mu_effective, y = mean_components, color = strategy, fill = strategy),
    method = "loess", se = TRUE, alpha = 0.2
  ) +
  geom_point(
    data = grouped,
    aes(x = mu_effective, y = mean_max_students_heuristic / 5),
    color = "purple", size = 2
  ) +
  geom_smooth(
    data = grouped,
    aes(x = mu_effective, y = mean_max_students_heuristic / 5),
    method = "loess", color = "purple", se = TRUE
  ) +
  geom_hline(
    data = ref_lines,
    aes(yintercept = y_line),
    linetype = "dashed", color = "red"
  ) +
  scale_y_continuous(
    name = "Average Components",
    sec.axis = sec_axis(~ . * 5, name = "Max Students (Heuristic)")
  ) +
  scale_color_manual(values = cols) +
  scale_fill_manual(values = cols) +
  facet_grid(groups ~ students_per_group, labeller = labeller(
    groups = function(g) paste("Groups:", g),
    students_per_group = function(s) paste("Group size:", s)
  )) +
  labs(
    title = "Average Components and Maximum Assigned Students (Robustness datasets)",
    subtitle = "Dashed line = target group size (secondary axis)",
    x = "μ (shifted family-size mean)",
    color = "Strategy",
    fill = "Strategy"
  ) +
  theme_minimal(base_size = 11) +
  theme(
    legend.position = "bottom",
    strip.text = element_text(size = 10, face = "bold")
  )

ggsave("resultsRobustness/Average_Components_and_Max_Students_ROBUSTNESS_ref.pdf", plot = my_plot2, device = "pdf", width = 11, height = 10)
ggsave("resultsRobustness/Average_Components_and_Max_Students_ROBUSTNESS_ref.jpg", plot = my_plot2, device = "jpeg", width = 11, height = 10, dpi = 300)
