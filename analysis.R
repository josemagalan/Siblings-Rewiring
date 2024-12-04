library(tidyverse)
library(igraph)
library(tidygraph)
library(ggraph)

# Leer el archivo CSV
data <- read_csv("datasets/ds_4_20_9_2.0_7777.csv")

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
  labs(title = "Red de Conexiones de Hermanos (Incluyendo Autoenlaces)", x = "Grupo", y = "Curso") +
  coord_fixed()

