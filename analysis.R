library(tidyverse)
library(igraph)
library(tidygraph)
library(ggraph)
library(mco)
library(RColorBrewer)
library(writexl)

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
  geom_edge_loop(aes(alpha = 0.7), show.legend = FALSE, color = "red") +
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
  geom_edge_loop(aes(alpha = 0.7), show.legend = FALSE, color = "red") +
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
#### Genético
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





################################Heurístic#################


#########################
#####Tener en cuenta que podría haber diferentes grupos por curso en versiones posteriores
######################################


# Se asume que 'data' y 'groups_per_course' están ya preparados.
# 'data' debe contener: student_id, family_id, course, group (aunque group se va a sobrescribir).
# 'groups_per_course' es una lista con nombres de curso como claves (ej: "1","2","3",...) 
# y como valor un vector de grupos disponibles en ese curso.

set.seed(123) # Para reproducibilidad

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

# 8. Calcular componentes conexos
comps <- components(graph_igraph)
num_components <- comps$no
component_sizes <- comps$csize
variance_sizes <- if(length(component_sizes) > 1) var(component_sizes) else 0

cat("Número de componentes conexos:", num_components, "\n")
cat("Varianza de los tamaños de los componentes:", variance_sizes, "\n")

# 9. Asignar componente a cada nodo
nodes <- nodes %>%
  mutate(component = comps$membership)

num_components <- max(nodes$component)
palette <- brewer.pal(n = min(num_components, 9), name = "Set1")
node_colors <- palette[nodes$component]

# 10. Crear el grafo con tidygraph y graficar
graph <- tbl_graph(edges = edges, nodes = nodes, directed = FALSE)
V(graph)$x <- nodes$x
V(graph)$y <- nodes$y
V(graph)$name <- nodes$node

ggraph(graph, layout = "manual", x = V(graph)$x, y = V(graph)$y) +
  # Dibujar autoenlaces (loops)
  geom_edge_loop(aes(alpha = 0.7), show.legend = FALSE, color = "red") +
  # Dibujar enlaces curvos
  geom_edge_arc(aes(alpha = 0.5), show.legend = FALSE, color = "grey") +  # Enlaces curvos
  # Dibujar nodos
  geom_node_point(aes(color = as.factor(component)), size = 4) +
  geom_node_text(aes(label = name), repel = TRUE, size = 3) +
  scale_color_manual(values = palette) +
  scale_y_continuous(
    breaks = seq(floor(min(V(graph)$y)), ceiling(max(V(graph)$y)), by = 1)
  ) +
  theme_minimal() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.ticks = element_blank(),
    axis.line = element_blank()
  ) +
  labs(
    title = "Red con Familias Asignadas a un Único Grupo (con Autoenlaces)",
    x = "Grupo",
    y = "Curso",
    color = "Componente"
  ) +
  coord_fixed()




##################################################
#########Heuristic mas SA##########################
#El cambio es de la asignación de la familia a un grupo
################################################


set.seed(123)


alpha <- 0.005  # Ajustar este parámetro según importancia relativa
max_iter <- 1000
T <- 1.0      # Temperatura inicial
cooling_rate <- 0.99

evaluate_solution <- function(solution, data) {
  # solution es un vector con grupos asignados a cada estudiante
  data_eval <- data
  data_eval$group <- solution
  data_eval$node <- paste(data_eval$course, data_eval$group, sep = "-")
  
  edges <- data_eval %>%
    group_by(family_id) %>%
    filter(n() > 1) %>%
    summarise(combinations = list(as.data.frame(t(combn(student_id, 2)))), .groups = "drop") %>%
    unnest(combinations) %>%
    rename(student_id1 = V1, student_id2 = V2) %>%
    inner_join(data_eval, by = c("student_id1" = "student_id")) %>%
    inner_join(data_eval, by = c("student_id2" = "student_id"), suffix = c("_src", "_dst")) %>%
    transmute(from = node_src, to = node_dst)
  
  nodes <- data_eval %>%
    distinct(node)
  
  graph <- graph_from_data_frame(d = edges, vertices = nodes, directed = FALSE)
  
  if (ecount(graph) == 0 || vcount(graph) == 0) {
    return(-Inf)  # Si no hay grafo, puntaje muy malo
  }
  
  comps <- components(graph)
  num_components <- comps$no
  component_sizes <- comps$csize
  variance_sizes <- if (length(component_sizes) < 2) 0 else var(component_sizes, na.rm = TRUE)
  
  # Objetivo: maximizar num_components y minimizar varianza
  return(num_components - alpha * variance_sizes)
}

# Función para generar un vecino
generate_neighbor <- function(current_solution, data, groups_per_course) {
  # Seleccionar una familia al azar
  fam_ids <- unique(data$family_id)
  selected_fam <- sample(fam_ids, 1)
  
  # Subset de la familia seleccionada
  fam_rows <- data$family_id == selected_fam
  
  # Obtener cursos de la familia
  fam_courses <- unique(data$course[fam_rows])
  
  # Obtener la intersección de grupos disponibles para esos cursos
  group_lists <- lapply(fam_courses, function(cc) groups_per_course[[as.character(cc)]])
  common_groups <- Reduce(intersect, group_lists)
  
  # Si no hay intersección, tomar un grupo del primer curso de la familia
  if (length(common_groups) == 0) {
    common_groups <- groups_per_course[[as.character(fam_courses[1])]]
  }
  
  # Elegir un grupo distinto al actual (si posible)
  current_group <- current_solution[which(fam_rows)[1]] # grupo actual de la familia (asumiendo que todos son iguales)
  possible_groups <- setdiff(common_groups, current_group)
  if (length(possible_groups) == 0) {
    # Si no hay otro grupo, no hay cambio
    return(current_solution)
  }
  
  new_group <- sample(possible_groups, 1)
  
  # Crear nueva solución
  new_solution <- current_solution
  new_solution[fam_rows] <- new_group
  return(new_solution)
}

# Preparar solución inicial (obtenida con la heurística previa)
current_solution <- initial_solution # Vector con asignaciones de grupo por estudiante
current_score <- evaluate_solution(current_solution, data)
best_solution <- current_solution
best_score <- current_score

for (iter in 1:max_iter) {
  neighbor <- generate_neighbor(current_solution, data, groups_per_course)
  neighbor_score <- evaluate_solution(neighbor, data)
  
  if (neighbor_score > current_score) {
    # Si la solución vecina es mejor, aceptar
    current_solution <- neighbor
    current_score <- neighbor_score
  } else {
    # Si es peor, aceptar con cierta probabilidad
    delta <- neighbor_score - current_score
    p <- exp(delta / T)
    if (runif(1) < p) {
      current_solution <- neighbor
      current_score <- neighbor_score
    }
  }
  
  # Actualizar mejor solución
  if (current_score > best_score) {
    best_solution <- current_solution
    best_score <- current_score
  }
  
  # Enfriar la temperatura
  T <- T * cooling_rate
  print(iter)
}

cat("Mejor puntuación encontrada:", best_score, "\n")





# Suponiendo que 'data' es tu dataframe original con: student_id, family_id, course, etc.
# 'best_solution' es el vector con la asignación final de grupos obtenido del recocido simulado.
data_final <- data
data_final$group <- best_solution
data_final$node <- paste(data_final$course, data_final$group, sep = "-")

# 5. Reconstruir la red (edges)
edges <- data_final %>%
  group_by(family_id) %>%
  filter(n() > 1) %>%
  summarise(combinations = list(as.data.frame(t(combn(student_id, 2)))), .groups = "drop") %>%
  unnest(combinations) %>%
  rename(student_id1 = V1, student_id2 = V2) %>%
  inner_join(data_final, by = c("student_id1" = "student_id")) %>%
  inner_join(data_final, by = c("student_id2" = "student_id"), suffix = c("_src", "_dst")) %>%
  transmute(from = node_src, to = node_dst)

# 6. Dataframe de nodos
nodes <- data_final %>%
  distinct(node, course, group) %>%
  mutate(
    x = as.numeric(as.factor(group)),  # posición horizontal (grupo)
    y = as.numeric(as.factor(course)), # posición vertical (curso)
    name = node
  )

# 7. Crear el grafo
graph_igraph <- graph_from_data_frame(edges, vertices = nodes, directed = FALSE)

# 8. Calcular componentes conexos
comps <- components(graph_igraph)
num_components <- comps$no
component_sizes <- comps$csize
variance_sizes <- if(length(component_sizes) > 1) var(component_sizes) else 0

cat("Número de componentes conexos:", num_components, "\n")
cat("Varianza de los tamaños de los componentes:", variance_sizes, "\n")

# 9. Asignar componente a cada nodo
nodes <- nodes %>%
  mutate(component = comps$membership)

num_components <- max(nodes$component)
palette <- brewer.pal(n = min(num_components, 9), name = "Set1")
node_colors <- palette[nodes$component]

# 10. Crear el grafo con tidygraph y graficar
graph <- tbl_graph(edges = edges, nodes = nodes, directed = FALSE)
V(graph)$x <- nodes$x
V(graph)$y <- nodes$y
V(graph)$name <- nodes$node

ggraph(graph, layout = "manual", x = V(graph)$x, y = V(graph)$y) +
  # Dibujar autoenlaces (loops)
  geom_edge_loop(aes(alpha = 0.7), show.legend = FALSE, color = "red") +
  # Dibujar enlaces curvos
  geom_edge_arc(aes(alpha = 0.5), show.legend = FALSE, color = "grey") +  
  # Dibujar nodos
  geom_node_point(aes(color = as.factor(component)), size = 4) +
  geom_node_text(aes(label = name), repel = TRUE, size = 3) +
  scale_color_manual(values = palette) +
  scale_y_continuous(
    breaks = seq(floor(min(V(graph)$y)), ceiling(max(V(graph)$y)), by = 1)
  ) +
  theme_minimal() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.ticks = element_blank(),
    axis.line = element_blank()
  ) +
  labs(
    title = "Red con la Solución Final Encontrada utilizando Simulated Annealing",
    x = "Grupo",
    y = "Curso",
    color = "Componente"
  ) +
  coord_fixed()



##################################################
#########Heuristic mas SA##########################
#SA mejorado con búsqueda local
################################################


# Parámetros
L <- 10
Tf <- 0.01
T0 <- 0.4 * initial_score
T <- T0

cooling_rate <- 0.9 

alpha <- 0.005

evaluate_solution <- function(solution, data, alpha) {
  data_eval <- data
  data_eval$group <- solution
  data_eval$node <- paste(data_eval$course, data_eval$group, sep = "-")
  
  edges <- data_eval %>%
    group_by(family_id) %>%
    filter(n() > 1) %>%
    summarise(combinations = list(as.data.frame(t(combn(student_id, 2)))), .groups = "drop") %>%
    unnest(combinations) %>%
    rename(student_id1 = V1, student_id2 = V2) %>%
    inner_join(data_eval, by = c("student_id1" = "student_id")) %>%
    inner_join(data_eval, by = c("student_id2" = "student_id"), suffix = c("_src", "_dst")) %>%
    transmute(from = node_src, to = node_dst)
  
  nodes <- data_eval %>%
    distinct(node)
  
  graph <- graph_from_data_frame(d = edges, vertices = nodes, directed = FALSE)
  
  if (ecount(graph) == 0 || vcount(graph) == 0) {
    return(-Inf)  # Si no hay grafo, puntaje muy bajo
  }
  
  comps <- components(graph)
  num_components <- comps$no
  component_sizes <- comps$csize
  variance_sizes <- if (length(component_sizes) < 2) 0 else var(component_sizes, na.rm = TRUE)
  
  
  return(num_components - alpha * variance_sizes)
}

generate_neighbor <- function(current_solution, data, groups_per_course) {
  # Seleccionar una familia al azar
  fam_ids <- unique(data$family_id)
  selected_fam <- sample(fam_ids, 1)
  
  # Subset de la familia seleccionada
  fam_rows <- data$family_id == selected_fam
  
  # Obtener cursos de la familia
  fam_courses <- unique(data$course[fam_rows])
  
  # Obtener la intersección de grupos disponibles para esos cursos
  group_lists <- lapply(fam_courses, function(cc) groups_per_course[[as.character(cc)]])
  common_groups <- Reduce(intersect, group_lists)
  
  # Si no hay intersección, tomar un grupo del primer curso de la familia
  if (length(common_groups) == 0) {
    common_groups <- groups_per_course[[as.character(fam_courses[1])]]
  }
  
  # Elegir un grupo distinto al actual (si posible)
  current_group <- current_solution[which(fam_rows)[1]]
  possible_groups <- setdiff(common_groups, current_group)
  if (length(possible_groups) == 0) {
    # Si no hay otro grupo, no hay cambio
    return(current_solution)
  }
  
  new_group <- sample(possible_groups, 1)
  
  # Crear nueva solución
  new_solution <- current_solution
  new_solution[fam_rows] <- new_group
  return(new_solution)
}





local_search_first_best <- function(solution, data, groups_per_course, alpha) {
  current_solution <- solution
  current_score <- evaluate_solution(current_solution, data, alpha)
  
  families <- unique(data$family_id)
  
  improved <- TRUE
  while (improved) {
    improved <- FALSE
    # Recorremos exhaustivamente todas las familias
    for (fam in families) {
      fam_rows <- data$family_id == fam
      fam_courses <- unique(data$course[fam_rows])
      
      # Obtener intersección de grupos para esos cursos
      group_lists <- lapply(fam_courses, function(cc) groups_per_course[[as.character(cc)]])
      common_groups <- Reduce(intersect, group_lists)
      
      # Grupo actual de esta familia
      current_group <- current_solution[which(fam_rows)[1]]
      
      # Probar todos los grupos diferentes al actual
      candidate_groups <- setdiff(common_groups, current_group)
      
      found_improvement <- FALSE
      for (g in candidate_groups) {
        neighbor_solution <- current_solution
        neighbor_solution[fam_rows] <- g
        neighbor_score <- evaluate_solution(neighbor_solution, data, alpha)
        
        if (neighbor_score > current_score) {
          # Primera mejora encontrada, actualizamos y volvemos a comenzar
          current_solution <- neighbor_solution
          current_score <- neighbor_score
          improved <- TRUE
          found_improvement <- TRUE
          break
        }
      }
      
      # Si se encontró una mejora, salir del bucle para reiniciar el proceso desde la primera familia
      if (found_improvement) break
    }
  }
  
  return(current_solution)
}


# Asumimos que initial_solution, data y groups_per_course están disponibles
initial_score <- evaluate_solution(initial_solution, data, alpha)


current_solution <- initial_solution
current_score <- initial_score
best_solution <- current_solution
best_score <- current_score

# Recocido Simulado
while (T > Tf) {
  for (count in 1:L) {
    neighbor <- generate_neighbor(current_solution, data, groups_per_course)
    neighbor_score <- evaluate_solution(neighbor, data, alpha)
    
    delta <- neighbor_score - current_score
    # Aceptación
    if (delta < 0) {
      # Si es peor, se acepta con probabilidad e^(-delta/T)
      if (runif(1) < exp(delta / T)) {
        current_solution <- neighbor
        current_score <- neighbor_score
      }
    } else {
      # Si es mejor, se acepta siempre
      current_solution <- neighbor
      current_score <- neighbor_score
    }
    
    # Actualizar mejor solución
    if (current_score > best_score) {
      best_solution <- current_solution
      best_score <- current_score
    }
  }
  
  # Enfriamiento
  T <- T * cooling_rate
  print(T)
}

# Búsqueda Local post-recocido (First-Best Improvement)
best_solution <- local_search_first_best(best_solution, data, groups_per_course, alpha)
best_score <- evaluate_solution(best_solution, data, alpha)

cat("Mejor puntuación encontrada:", best_score, "\n")

# Reconstruir datos finales
data_final <- data
data_final$group <- best_solution
data_final$node <- paste(data_final$course, data_final$group, sep = "-")

edges <- data_final %>%
  group_by(family_id) %>%
  filter(n() > 1) %>%
  summarise(combinations = list(as.data.frame(t(combn(student_id, 2)))), .groups = "drop") %>%
  unnest(combinations) %>%
  rename(student_id1 = V1, student_id2 = V2) %>%
  inner_join(data_final, by = c("student_id1" = "student_id")) %>%
  inner_join(data_final, by = c("student_id2" = "student_id"), suffix = c("_src", "_dst")) %>%
  transmute(from = node_src, to = node_dst)

nodes <- data_final %>%
  distinct(node, course, group) %>%
  mutate(
    x = as.numeric(as.factor(group)),
    y = as.numeric(as.factor(course)),
    name = node
  )

graph_igraph <- graph_from_data_frame(edges, vertices = nodes, directed = FALSE)
comps <- components(graph_igraph)
num_components <- comps$no
component_sizes <- comps$csize
variance_sizes <- if(length(component_sizes) > 1) var(component_sizes) else 0

cat("Número de componentes conexos:", num_components, "\n")
cat("Varianza de los tamaños de los componentes:", variance_sizes, "\n")

nodes <- nodes %>%
  mutate(component = comps$membership)

num_components <- max(nodes$component)

if (num_components <= 9) {
  palette <- brewer.pal(n = num_components, name = "Set1")
} else {
  # Generar una paleta más grande usando, por ejemplo, colorRampPalette
  # Aquí tomamos Set1 como base y expandimos. Puedes usar cualquier otra paleta base.
  base_colors <- brewer.pal(n = 9, name = "Set1")
  # Generar una paleta de num_components colores interpolando la existente
  palette <- colorRampPalette(base_colors)(num_components)
}



graph <- tbl_graph(edges = edges, nodes = nodes, directed = FALSE)
V(graph)$x <- nodes$x
V(graph)$y <- nodes$y
V(graph)$name <- nodes$node

ggraph(graph, layout = "manual", x = V(graph)$x, y = V(graph)$y) +
  geom_edge_loop(aes(alpha = 0.7), show.legend = FALSE, color = "red") +
  geom_edge_arc(aes(alpha = 0.5), show.legend = FALSE, color = "grey") +  
  geom_node_point(aes(color = as.factor(component)), size = 4) +
  geom_node_text(aes(label = name), repel = TRUE, size = 3) +
  scale_color_manual(values = palette) +
  scale_y_continuous(
    breaks = seq(floor(min(V(graph)$y)), ceiling(max(V(graph)$y)), by = 1)
  ) +
  theme_minimal() +
  theme(
    panel.grid.major = element_blank(),
    panel.grid.minor = element_blank(),
    axis.ticks = element_blank(),
    axis.line = element_blank()
  ) +
  labs(
    title = "Red con la Solución Final (Recocido + Búsqueda Local)",
    x = "Grupo",
    y = "Curso",
    color = "Componente"
  ) +
  coord_fixed()

write_xlsx(data_final, "solucion_final.xlsx")


###########################Nivelación




