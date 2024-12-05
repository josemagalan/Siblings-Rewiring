library(tidyverse)
library(igraph)
library(tidygraph)
library(ggraph)
library(mco)
library(RColorBrewer)

# Leer el archivo CSV
data <- read_csv("datasets/ds_4_20_9_0.1_7777.csv")

# Crear un nodo combinando curso y grupo
data <- data %>%
  mutate(node = paste(course, group, sep = "-"))

# Crear los enlaces recorriendo las familias
edges <- data %>%
  group_by(family_id) %>%
  filter(n() > 1) %>%  # Considerar solo familias con más de un miembro
  summarise(combinations = list(as.data.frame(t(combn(student_id, 2)))), .groups = "drop") %>%
  unnest(combinations) %>%
  rename(student_id1 = V1, student_id2 = V2) %>%
  inner_join(data, by = c("student_id1" = "student_id")) %>%
  inner_join(data, by = c("student_id2" = "student_id"), suffix = c("_src", "_dst")) %>%
  transmute(
    from = node_src,
    to = node_dst,
    student_id1 = student_id1,
    student_id2 = student_id2
  )

# Crear un dataframe de nodos con posiciones
nodes <- data %>%
  distinct(node, course, group) %>%
  mutate(
    x = as.numeric(as.factor(group)),  # Posición horizontal (grupo)
    y = as.numeric(as.factor(course))  # Posición vertical (curso)
  )

# Crear la red como grafo con tidygraph y añadir posiciones como atributo
graph <- tbl_graph(edges = edges, nodes = nodes, directed = FALSE)

# Asignar posiciones manuales y nombres como atributos
V(graph)$x <- nodes$x
V(graph)$y <- nodes$y
V(graph)$name <- nodes$node  # Asegurar que los nombres estén asignados

# Visualizar la red con ggraph
ggraph(graph, layout = "manual", x = V(graph)$x, y = V(graph)$y) +
  # Dibujar autoenlaces (loops)
  geom_edge_loop(aes(alpha = 0.7), show.legend = FALSE, color = "red", strength = 0.2) +
  # Dibujar enlaces normales
  geom_edge_link(aes(alpha = 0.5), show.legend = FALSE) +  
  # Dibujar nodos
  geom_node_point(size = 4, color = "blue") +
  # Etiquetas de nodos
  geom_node_text(aes(label = name), repel = TRUE) +
  theme_minimal() +
  theme(
    panel.grid.major = element_blank(),  # Eliminar cuadrícula principal
    panel.grid.minor = element_blank(),  # Eliminar cuadrícula secundaria
    axis.ticks = element_blank(),        # Eliminar marcas de los ejes
    axis.line = element_blank()          # Eliminar líneas de los ejes
  )+
  labs(title = "Red de Conexiones de Hermanos (Incluyendo Autoenlaces)", x = "Group", y = "Course") +
  coord_fixed()



############################Burbuja estándar

# Crear un nodo combinando curso y grupo
data <- data %>%
  mutate(node = paste(course, group, sep = "-"))

# Crear los enlaces recorriendo las familias y ajustando para evitar enlaces entre grupos del mismo curso
edges <- data %>%
  group_by(family_id) %>%
  filter(n() > 1) %>%  # Considerar solo familias con más de un miembro
  summarise(combinations = list(as.data.frame(t(combn(student_id, 2)))), .groups = "drop") %>%
  unnest(combinations) %>%
  rename(student_id1 = V1, student_id2 = V2) %>%
  inner_join(data, by = c("student_id1" = "student_id")) %>%
  inner_join(data, by = c("student_id2" = "student_id"), suffix = c("_src", "_dst")) %>%
  mutate(
    # Detectar si los hermanos están en el mismo curso
    same_course = course_src == course_dst,
    # Detectar si están en diferentes grupos dentro del mismo curso
    diff_group_same_course = same_course & group_src != group_dst
  ) %>%
  transmute(
    from = if_else(diff_group_same_course, node_src, node_src),  # Usar el nodo de origen para autoenlace
    to = if_else(diff_group_same_course, node_src, node_dst),    # Usar el mismo nodo para autoenlace
    student_id1 = student_id1,
    student_id2 = student_id2
  )

# Crear un dataframe de nodos con posiciones
nodes <- data %>%
  distinct(node, course, group) %>%
  mutate(
    x = as.numeric(as.factor(group)),  # Posición horizontal (grupo)
    y = as.numeric(as.factor(course))  # Posición vertical (curso)
  )

# Crear la red como grafo con tidygraph
graph <- tbl_graph(edges = edges, nodes = nodes, directed = FALSE)

# Asignar posiciones manuales y nombres como atributos
V(graph)$x <- nodes$x
V(graph)$y <- nodes$y
V(graph)$name <- nodes$node  # Asegurar que los nombres estén asignados

# Visualizar la red con ggraph
ggraph(graph, layout = "manual", x = V(graph)$x, y = V(graph)$y) +
  geom_edge_loop(aes(alpha = 0.7), show.legend = FALSE, color = "red", strength = 0.2) +
  geom_edge_link(aes(alpha = 0.5), show.legend = FALSE) +  
  geom_node_point(size = 4, color = "blue") +
  geom_node_text(aes(label = name), repel = TRUE) +
  theme_minimal() +
  theme(
    panel.grid.major = element_blank(),  # Eliminar cuadrícula principal
    panel.grid.minor = element_blank(),  # Eliminar cuadrícula secundaria
    axis.ticks = element_blank(),        # Eliminar marcas de los ejes
    axis.line = element_blank()          # Eliminar líneas de los ejes
  ) +
  labs(title = "Adjusted Network with Self-Loops for Same-Course Siblings", x = "Group", y = "Course") +
  coord_fixed()


# Calcular componentes conexos
components <- components(graph)

# Número de componentes conexos
num_components <- components$no
cat("Número de componentes conexos:", num_components, "\n")

# Tamaño de cada componente conexo
component_sizes <- components$csize
cat("Tamaños de los componentes conexos:\n")
print(component_sizes)

# Asignar el componente a cada nodo
node_components <- tibble(
  node = V(graph)$name,
  component = components$membership
)

cat("Asignación de nodos a componentes conexos:\n")
print(node_components)




##########################


# Crear una lista de grupos disponibles por curso
groups_per_course <- data %>%
  group_by(course) %>%
  summarise(groups = list(unique(group))) %>%
  deframe()

fitness_function <- function(individual) {
  # Asegurar que 'individual' contenga solo valores enteros
  individual <- round(individual)
  
  # 'individual' es un vector con las asignaciones de grupos para cada estudiante
  data_individual <- data
  data_individual$group <- individual
  data_individual$node <- paste(data_individual$course, data_individual$group, sep = "-")
  
  # Reconstruir los enlaces basados en las nuevas asignaciones
  edges <- data_individual %>%
    group_by(family_id) %>%
    filter(n() > 1) %>%
    summarise(combinations = list(as.data.frame(t(combn(student_id, 2)))), .groups = "drop") %>%
    unnest(combinations) %>%
    rename(student_id1 = V1, student_id2 = V2) %>%
    inner_join(data_individual, by = c("student_id1" = "student_id")) %>%
    inner_join(data_individual, by = c("student_id2" = "student_id"), suffix = c("_src", "_dst")) %>%
    transmute(
      from = node_src,
      to = node_dst
    )
  
  # Crear el grafo
  nodes <- data_individual %>%
    distinct(node)
  
  graph <- graph_from_data_frame(d = edges, vertices = nodes, directed = FALSE)
  
  if (ecount(graph) == 0 || vcount(graph) == 0) {
    return(c(NA, NA))  # Grafo vacío, retornar NA
  }
  
  # Calcular los componentes conexos
  comps <- components(graph)
  num_components <- comps$no
  component_sizes <- comps$csize
  
  # Manejar casos de varianza
  if (length(component_sizes) < 2) {
    variance_sizes <- 0  # Si hay un solo componente, la varianza es 0
  } else {
    variance_sizes <- var(component_sizes, na.rm = TRUE)
  }
  
  # Retornar las funciones objetivo
  return(c(-num_components, variance_sizes))  # Negamos num_components porque NSGA-II minimiza
}


# Número de variables (número de estudiantes)
num_variables <- nrow(data)

# Definir las variables (límites de los grupos para cada estudiante)
lower_bounds <- sapply(1:num_variables, function(i) {
  student_course <- data$course[i]
  min(groups_per_course[[as.character(student_course)]])
})
upper_bounds <- sapply(1:num_variables, function(i) {
  student_course <- data$course[i]
  max(groups_per_course[[as.character(student_course)]])
})

# Ejecutar el algoritmo NSGA-II
result <- nsga2(
  fn  = fitness_function,
  idim = num_variables,    # Número de variables de decisión
  odim = 2,                # Número de funciones objetivo
  lower.bounds = lower_bounds,
  upper.bounds = upper_bounds,
  popsize = 100,
  generations = 100,
  cprob = 0.7,             # Probabilidad de cruzamiento
  mprob = 0.2,             # Probabilidad de mutación
  cdist = 20,
  mdist = 20
)

# Extraer las soluciones no dominadas
pareto_front <- result$value




# Paso 1: Seleccionar una solución específica
solution_index <- 2  # Cambiar al índice deseado
selected_solution <- result$par[solution_index, ]
selected_solution <- round(selected_solution)  # Asegurarse de que las variables sean enteras

# Paso 2: Crear el dataframe con asignaciones de grupos según la solución seleccionada
data_selected <- data
data_selected$group <- selected_solution
data_selected$node <- paste(data_selected$course, data_selected$group, sep = "-")

# Reconstruir los enlaces (edges)
edges <- data_selected %>%
  group_by(family_id) %>%
  filter(n() > 1) %>%
  summarise(combinations = list(as.data.frame(t(combn(student_id, 2)))), .groups = "drop") %>%
  unnest(combinations) %>%
  rename(student_id1 = V1, student_id2 = V2) %>%
  inner_join(data_selected, by = c("student_id1" = "student_id")) %>%
  inner_join(data_selected, by = c("student_id2" = "student_id"), suffix = c("_src", "_dst")) %>%
  transmute(
    from = node_src,
    to = node_dst
  )

# Paso 3: Crear el dataframe de nodos con posiciones
nodes <- data_selected %>%
  distinct(node, course, group) %>%
  mutate(
    x = as.numeric(as.factor(group)),  # Posición horizontal (grupo)
    y = as.numeric(as.factor(course))  # Posición vertical (curso)
  )

nodes <- nodes %>%
  mutate(name = node)  # Asegurarte de que 'name' existe

# Paso 1: Identificar los componentes conexos
graph_igraph <- graph_from_data_frame(edges, vertices = nodes, directed = FALSE)
components <- components(graph_igraph)

# Asignar el componente a cada nodo
nodes <- nodes %>%
  mutate(component = components$membership)

palette <- colorRampPalette(brewer.pal(9, "Set1"))(num_components)

# Asignar colores a los componentes
num_components <- max(nodes$component)
palette <- brewer.pal(n = min(num_components, 9), name = "Set1")
node_colors <- palette[nodes$component]


# Crear el grafo con tidygraph
graph <- tbl_graph(edges = edges, nodes = nodes, directed = FALSE)

# Asignar posiciones manuales y nombres como atributos
V(graph)$x <- nodes$x
V(graph)$y <- nodes$y
V(graph)$name <- nodes$node  # Asegurar que los nombres estén asignados
ggraph(graph, layout = "manual", x = V(graph)$x, y = V(graph)$y) +
  geom_edge_fan(aes(alpha = 0.5), show.legend = FALSE, color = "grey") +
  geom_node_point(aes(color = as.factor(component)), size = 4) +
  geom_node_text(aes(label = name), repel = TRUE, size = 3) +
  scale_color_manual(values = palette) +
  scale_y_continuous(
    breaks = seq(floor(min(V(graph)$y)), ceiling(max(V(graph)$y)), by = 1)  # Escala entera
  ) +
  theme_minimal() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.ticks = element_blank(),
    axis.line = element_blank()
  ) +
  labs(
    title = "Red con Nodos Coloreados por Componentes",
    x = "Grupo",
    y = "Curso",
    color = "Componente"
  ) +
  coord_fixed()





# Crear el gráfico con ajustes
ggraph(graph, layout = "manual", x = V(graph)$x, y = V(graph)$y) +
  geom_edge_arc(aes(alpha = 0.5), show.legend = FALSE, color = "grey") +  # Enlaces curvos
  geom_node_point(aes(color = as.factor(component)), size = 4) +
  geom_node_text(aes(label = name), repel = TRUE, size = 3) +
  scale_color_manual(values = palette) +  # Colores por componente
  scale_y_continuous(
    breaks = seq(floor(min(V(graph)$y)), ceiling(max(V(graph)$y)), by = 1)  # Escala entera
  ) +
  theme_minimal() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.ticks = element_blank(),
    axis.line = element_blank()
  ) +
  labs(
    title = "Red con Nodos Coloreados por Componentes y Líneas Curvas",
    x = "Grupo",
    y = "Curso",
    color = "Componente"
  ) +
  coord_fixed()
