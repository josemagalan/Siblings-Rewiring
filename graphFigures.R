crear_y_graficar_grafo <- function(graph, title = "") {
  # 1. Calcular componentes conexos
  comps <- components(graph)
  num_components <- comps$no
  component_sizes <- comps$csize
  variance_sizes <- if(length(component_sizes) > 1) var(component_sizes) else 0
  
  cat("Número de componentes conexos:", num_components, "\n")
  cat("Varianza de los tamaños de los componentes:", variance_sizes, "\n")
  
  # 2. Asignar componentes como atributo del grafo
  V(graph)$component <- comps$membership
  
  # 3. Generar una paleta de colores adecuada
  num_components <- max(V(graph)$component)
  if (num_components <= 9) {
    palette <- brewer.pal(n = num_components, name = "Set1")
  } else {
    base_colors <- brewer.pal(n = 9, name = "Set1")
    palette <- colorRampPalette(base_colors)(num_components)
  }
  
  # 4. Graficar la red
  plot <- ggraph(graph, layout = "manual", x = V(graph)$x, y = V(graph)$y) +
    geom_edge_loop(aes(alpha = 0.7), show.legend = FALSE, color = "red") +
    geom_edge_arc(aes(alpha = 0.5), show.legend = FALSE, color = "grey") +
    geom_node_point(aes(color = as.factor(component)), linewidth = 4) +
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
      title = title,
      x = "Group",
      y = "Course",
      color = "Component"
    ) +
    coord_fixed()
  
  return(plot)
}

