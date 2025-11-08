# =======================
# Figure 1 Improved (1 dataset)
# =======================

# Paquetes
library(tidyverse)
library(igraph)
library(tidygraph)
library(ggraph)
library(ggrepel)
library(ggforce)
library(patchwork)
library(RColorBrewer)

# Funciones propias (balanceo)
source("nivelacion.R")  # Ajustar_Exacto_Nivelacion()


crear_y_graficar_grafo_v2 <- function(graph, title = "") {
  # 1) Componentes
  comps <- igraph::components(graph)
  k <- comps$no
  V(graph)$component <- comps$membership
  
  # 2) Componente de origen para cada arista (para colorear arcos)
  ee <- igraph::ends(graph, igraph::E(graph), names = FALSE)
  igraph::E(graph)$component_from <- V(graph)$component[ee[, 1]]
  
  # 3) Paleta estable y consistente entre nodos y aristas
  comp_levels <- sort(unique(V(graph)$component))
  if (length(comp_levels) <= 9) {
    base_n <- max(3, length(comp_levels))
    pal <- RColorBrewer::brewer.pal(base_n, "Set1")[seq_along(comp_levels)]
  } else {
    base_colors <- RColorBrewer::brewer.pal(9, "Set1")
    pal <- grDevices::colorRampPalette(base_colors)(length(comp_levels))
  }
  # vector con nombre -> color (misma asignación para nodos y aristas)
  pal_named <- setNames(pal, as.character(comp_levels))
  
  # 4) Plot
  p <- ggraph(graph, layout = "manual", x = V(graph)$x, y = V(graph)$y) +
    # Loops SIEMPRE gris oscuro
    geom_edge_loop(colour = "grey20", alpha = 0.7, show.legend = FALSE) +
    # Arcos coloreados por el componente del nodo origen (mismo mapeo que nodos)
    geom_edge_arc(aes(colour = as.factor(component_from)), alpha = 0.55, show.legend = FALSE) +
    # Nodos más grandes
    geom_node_point(aes(colour = as.factor(component)), size = 4.6) +
    # Etiquetas
    geom_node_text(aes(label = name), repel = TRUE, size = 3) +
    # Misma escala (y mismos niveles) para nodos y aristas
    scale_colour_manual(values = pal_named, limits = names(pal_named), drop = FALSE) +
    # Ejes
    scale_x_continuous(breaks = 1:4, labels = 1:4, expand = expansion(mult = c(0.05, 0.05))) +
    scale_y_continuous(
      breaks = seq(floor(min(V(graph)$y)), ceiling(max(V(graph)$y)), by = 1),
      expand = expansion(mult = c(0.05, 0.05))
    ) +
    labs(title = title, x = "Group", y = "Course", colour = "Component") +
    theme_minimal(base_size = 11) +
    theme(
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      axis.ticks = element_blank(),
      axis.line = element_blank(),
      plot.title = element_text(face = "bold"),
      legend.position = if (length(comp_levels) > 1) "right" else "none"
    ) +
    coord_fixed()
  
  return(p)
}

# ---------------------------------------------------------------

# Paths y carpeta
file <- "datasets/ds_4_20_9_0.2_7777.csv"
params_str <- basename(file) |> str_remove("\\.csv$")
dir.create("Fig.1", showWarnings = FALSE)

# Lectura
data <- readr::read_csv(file, col_types = readr::cols(sibling_ids = readr::col_character()))

# Helpers (nodos / aristas)
make_nodes <- function(df){
  df %>%
    mutate(node = paste(course, group, sep = "-")) %>%
    distinct(node, course, group) %>%
    mutate(
      x = as.numeric(as.factor(group)),
      y = as.numeric(as.factor(course)),
      name = node
    )
}
make_edges <- function(df_with_node){
  df_with_node %>%
    group_by(family_id) %>% filter(n() > 1) %>% ungroup() %>%
    summarise(comb = list(as.data.frame(t(combn(student_id, 2)))), .groups = "drop") %>%
    unnest(comb) %>%
    rename(student_id1 = V1, student_id2 = V2) %>%
    inner_join(df_with_node, by = c("student_id1" = "student_id")) %>%
    inner_join(df_with_node, by = c("student_id2" = "student_id"),
               suffix = c("_src", "_dst")) %>%
    transmute(from = paste(course_src, group_src, sep = "-"),
              to   = paste(course_dst, group_dst, sep = "-"))
}

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

plot_initial <- crear_y_graficar_grafo_v2(as.igraph(graph_initial), "Initial assignment")
ggsave(file.path("Fig.1", "Fig1A_initial.jpg"), plot_initial, width = 8, height = 6, dpi = 450)
ggsave(file.path("Fig.1", "Fig1A_initial.pdf"), plot_initial, width = 8, height = 6, device = cairo_pdf)

# ========================
# 2) Standard bubble
# ========================
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

plot_bubble <- crear_y_graficar_grafo_v2(as.igraph(graph_bubble), "Standard bubble")
ggsave(file.path("Fig.1", "Fig1B_bubble.jpg"), plot_bubble, width = 8, height = 6, dpi = 450)
ggsave(file.path("Fig.1", "Fig1B_bubble.pdf"), plot_bubble, width = 8, height = 6, device = cairo_pdf)

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
plot_heuristic <- crear_y_graficar_grafo_v2(graph_heuristic, "Allocation obtained using the heuristic algorithm")
ggsave(filename = file.path("results", paste0(params_str, "_heuristic.jpg")), plot = plot_heuristic, width = 8, height = 6)
ggsave(filename = file.path("results", paste0(params_str, "_heuristic.pdf")), plot = plot_heuristic, width = 8, height = 6)
ggsave(file.path("Fig.1", "Fig1C_heuristic.jpg"), plot_bal, width = 8, height = 6, dpi = 450)
ggsave(file.path("Fig.1", "Fig1C_heuristic.pdf"), plot_bal, width = 8, height = 6, device = cairo_pdf)


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

plot_bal <- crear_y_graficar_grafo_v2(graph_heuristic_balanced, "Allocation obtained using the heuristic algorithm\nand greedy balancing")
ggsave(file.path("Fig.1", "Fig1C_heuristic_balanced.jpg"), plot_bal, width = 8, height = 6, dpi = 450)
ggsave(file.path("Fig.1", "Fig1C_heuristic_balanced.pdf"), plot_bal, width = 8, height = 6, device = cairo_pdf)

# ========================
# 4) Composición de los 3 paneles
# ========================
combo <- plot_initial | plot_bubble | plot_bal
ggsave(file.path("Fig.1", "Fig1_combined.jpg"), combo, width = 14, height = 4.5, dpi = 450)
ggsave(file.path("Fig.1", "Fig1_combined.pdf"), combo, width = 14, height = 4.5, device = cairo_pdf)
