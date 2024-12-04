library(tidyverse)
library(igraph)
library(tidygraph)
library(ggraph)

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

library(igraph)
library(GA)
library(dplyr)

# Crear la función para inicializar una red válida con rewiring
initialize_network <- function(graph) {
  edges <- as_data_frame(graph, what = "edges") %>%
    distinct()  # Eliminar duplicados en las aristas
  nodes <- as_data_frame(graph, what = "vertices") %>%
    mutate(name = ifelse(is.null(name), as.character(row_number()), name)) %>%
    distinct()  # Asegurar que los nodos sean únicos
  
  # Mantener enlaces dentro del mismo curso
  rewired_edges <- edges %>%
    left_join(nodes, by = c("from" = "name")) %>%
    filter(!is.na(course)) %>%  # Asegurar que haya datos de curso
    group_by(course) %>%
    summarise(
      rewired = list(tibble(
        from = sample(from, replace = FALSE),
        to = sample(from, replace = FALSE)
      )),
      .groups = "drop"
    ) %>%
    unnest(cols = rewired) %>%
    distinct(from, to)  # Eliminar duplicados después del "rewiring"
  
  # Crear nueva red con los enlaces reorganizados
  graph_from_data_frame(rewired_edges, directed = FALSE, vertices = nodes)
}

# Evaluación de un individuo
evaluate_network <- function(graph) {
  components <- components(graph)
  num_components <- components$no
  variance_components <- var(components$csize, na.rm = TRUE)
  return(c(-num_components, variance_components))  # Maximizar componentes, minimizar varianza
}

# Definir la función de evaluación para el GA
fitness_function <- function(individual, graph) {
  rewired_graph <- initialize_network(graph)  # Generar red a partir del individuo
  evaluation <- evaluate_network(rewired_graph)
  return(evaluation)
}


# Configuración del GA
ga <- ga(
  type = "binary", 
  fitness = function(individual) fitness_function(individual, graph),
  nBits = ecount(graph),  # Número de enlaces
  maxiter = 100,          # Máximo número de iteraciones
  popSize = 50,           # Tamaño de la población
  elitism = 5,            # Elitismo
  pmutation = 0.2         # Probabilidad de mutación
)

# Resultados
summary(ga)
plot(ga)
