library(tidyverse)
library(igraph)
library(tidygraph)
library(ggraph)
library(mco)
library(RColorBrewer)
library(writexl)
source("graphFigures.R")


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

myPlot<-crear_y_graficar_grafo(graph,"Initial asignment")
myPlot

########################################
############################Burbuja estándar
##########################################

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


myPlot<-crear_y_graficar_grafo(graph,"Standard bubble")
myPlot



################################Heurístic#################


#########################
#####Tener en cuenta que podría haber diferentes grupos por curso en versiones posteriores
######################################


# Se asume que 'data' y 'groups_per_course' están ya preparados.
# 'data' debe contener: student_id, family_id, course, group (aunque group se va a sobrescribir).
# 'groups_per_course' es una lista con nombres de curso como claves (ej: "1","2","3",...) 
# y como valor un vector de grupos disponibles en ese curso.


# 1. Para cada familia, determinamos en qué cursos está
families_courses <- data %>%
  group_by(family_id) %>%
  summarise(courses = list(unique(course)), .groups = "drop")

# 2. Para cada familia, buscar la intersección de grupos disponibles en todos los cursos en los que está
families_groups <- families_courses %>%
  mutate(
    common_groups = map(courses, ~ {
      # Intersectar todos los vectores de grupos de estos cursos
      course_vector <- .x
      # Obtener la lista de vectores de grupos para cada curso
      group_lists <- lapply(course_vector, function(cc) groups_per_course[[as.character(cc)]])
      # Intersectar
      Reduce(intersect, group_lists)
    })
  )

# 3. Asignar un grupo aleatorio para cada familia a partir de los common_groups
# Si no hay intersección, se puede optar por asignar un grupo del primer curso
families_groups <- families_groups %>%
  rowwise() %>%
  mutate(
    assigned_group = if (length(common_groups) > 0) {
      sample(common_groups, 1)
    } else {
      # Si no hay intersección, tomamos un grupo del primer curso.
      first_course <- courses[[1]]
      sample(groups_per_course[[as.character(first_course)]], 1)
    }
  ) %>%
  ungroup()

# 4. Unir esta asignación a los datos originales
data_assigned <- data %>%
  left_join(families_groups %>% select(family_id, assigned_group), by = "family_id") %>%
  mutate(
    group = assigned_group,
    node = paste(course, group, sep = "-")
  ) %>%
  select(-assigned_group)

initial_solution<-data_assigned$group

# 5. Reconstruir la red
edges <- data_assigned %>%
  group_by(family_id) %>%
  filter(n() > 1) %>%
  summarise(combinations = list(as.data.frame(t(combn(student_id, 2)))), .groups = "drop") %>%
  unnest(combinations) %>%
  rename(student_id1 = V1, student_id2 = V2) %>%
  inner_join(data_assigned, by = c("student_id1" = "student_id")) %>%
  inner_join(data_assigned, by = c("student_id2" = "student_id"), suffix = c("_src", "_dst")) %>%
  transmute(
    from = node_src,
    to = node_dst
  )

# 6. Dataframe de nodos
nodes <- data_assigned %>%
  distinct(node, course, group) %>%
  mutate(
    x = as.numeric(as.factor(group)),  # posición horizontal (grupo)
    y = as.numeric(as.factor(course)), # posición vertical (curso)
    name = node
  )

# 7. Crear el grafo
graph_igraph <- graph_from_data_frame(edges, vertices = nodes, directed = FALSE)

# Calcular los grados de todos los nodos
node_degrees <- degree(graph_igraph)

# Obtener el grado máximo
max_degree <- max(node_degrees)

cat("El grado máximo del grafo es:", max_degree, "\n")



myPlot<-crear_y_graficar_grafo(graph_igraph,"Allocation obtained using the heuristic algorithm")
myPlot