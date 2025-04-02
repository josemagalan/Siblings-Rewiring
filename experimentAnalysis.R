library(tidyverse)
library(RColorBrewer)
library(viridis)


# Leer y combinar todos los CSV que contengan "experiment_results_" en la carpeta results
SAexpResults <- list.files(path = "results", 
                           pattern = "experiment_results_.*\\.csv$", 
                           full.names = TRUE) %>%
  map_df(read_csv)

# Ver los primeros resultados para verificar
print(SAexpResults)


# Leer y combinar todos los CSV que contengan "experiment_results_GA_" en la carpeta results/GA
GAexpResults <- list.files(path = "results/GA", 
                           pattern = "experiment_results_GA_.*\\.csv$", 
                           full.names = TRUE) %>%
  map_df(read_csv)

# Verificar los primeros resultados
print(GAexpResults)

BasicexpResults <- read_csv("results/results_summary.csv")



# Creando dataframe en formato largo con los tres algoritmos
BasicexpResults_long <- BasicexpResults %>%
  select(filename, groups, students_per_group, courses, poisson_param, seed,
         n_components_initial, var_components_initial,
         n_components_bubble, var_components_bubble) %>%
  pivot_longer(cols = c(n_components_initial, var_components_initial,
                        n_components_bubble, var_components_bubble),
               names_to = c("metric", "Algorithm"),
               names_pattern = "(n_components|var_components)_(initial|bubble)",
               values_to = "value") %>%
  pivot_wider(names_from = metric, values_from = value) %>%
  mutate(Algorithm = recode(Algorithm,
                            initial = "Initial",
                            bubble = "Bubble"))

# Verificación
print(BasicexpResults_long)



# Transformación del dataframe SAexpResults en formato largo con algoritmo "SA"
SAexpResults_long <- SAexpResults %>%
  transmute(
    filename = archivo,
    groups = grupos,
    students_per_group = alumnos,
    courses = cursos,
    poisson_param = poison,
    seed = semilla,
    Algorithm = "SA",
    n_components = n_componentes_final,
    var_components = varianza_final
  )


SAexpResults_long <- SAexpResults_long %>%
  mutate(filename = str_replace(filename, "_datosNivelados\\.rds$", ".csv"))

# Verificar el resultado
print(SAexpResults_long)


# Transformación del dataframe SAexpResults en formato largo con algoritmo "SA"
HeuristicexpResults_long <- SAexpResults %>%
  filter(w1==0)%>%
  transmute(
    filename = archivo,
    groups = grupos,
    students_per_group = alumnos,
    courses = cursos,
    poisson_param = poison,
    seed = semilla,
    Algorithm = "HeuristicBalanced",
    n_components = n_componentes_inicial,
    var_components = varianza_inicial
  )


HeuristicexpResults_long  <- HeuristicexpResults_long  %>%
  mutate(filename = str_replace(filename, "_datosNivelados\\.rds$", ".csv"))

# Transformación del dataframe GAexpResults en formato largo con algoritmo "GA"
GAexpResults_long <- GAexpResults %>%
  transmute(
    filename = archivo,
    groups = grupos,
    students_per_group = alumnos,
    courses = cursos,
    poisson_param = poison,
    seed = semilla,
    Algorithm = "GA",
    n_components = n_componentes_final,
    var_components = varianza_final
  )

# Unión de los tres dataframes en uno solo
AllExpResults <- bind_rows(BasicexpResults_long, SAexpResults_long, GAexpResults_long,HeuristicexpResults_long)

# Verificación rápida del resultado
print(AllExpResults)

AllExpResults <- AllExpResults %>%
  mutate(var_components = replace_na(var_components, 0))

summary(AllExpResults)

AllExpResults <- AllExpResults %>%
  group_by(filename, Algorithm, n_components) %>%
  mutate(
    MinVar = if_else(var_components == min(var_components), 1, 0),
    MinVar = if_else(MinVar == 1 & duplicated(var_components), 0, MinVar)
  ) %>%
  ungroup() %>%
  group_by(filename, Algorithm) %>%
  mutate(
    Pareto = if_else(MinVar == 1 & !map_lgl(row_number(), ~ any(
      (var_components < var_components[.x]) & (n_components > n_components[.x])
    )), 1, 0)
  ) %>%
  ungroup()


jitter<-0.05


AllExpResults %>%
  filter(Pareto == 1) %>%
  group_by(filename) %>%
  group_walk(~ {
    plot <- ggplot(.x, aes(x = n_components, y = var_components, color = Algorithm)) +
      geom_point(size = 3, position = position_jitter(width = jitter, height = jitter)) +
      geom_line(aes(group = Algorithm), position = position_jitter(width = jitter, height = jitter)) +
      labs(title = .y$filename,
           x = "Número de componentes",
           y = "Varianza",
           color = "Algoritmo") +
      theme_minimal()
    
    ggsave(filename = paste0("pareto/", .y$filename, ".jpg"), plot = plot, width = 8, height = 6)
  })



#########################################
####Análisis de las tres técnicas monosolución


# Filtrar sólo las estrategias iniciales con soluciones únicas
single_solutions <- AllExpResults %>%
  filter(Algorithm %in% c("Initial", "Bubble", "HeuristicBalanced")) %>%
  select(filename, groups, poisson_param, Algorithm, n_components, var_components)


ggplot(single_solutions, aes(x=Algorithm, y=n_components, fill=Algorithm)) +
  geom_boxplot(alpha=0.7) +
  theme_minimal() +
  labs(title="Comparación del Número de Componentes Conexos",
       x="Strategy",
       y="Number of components") +
  theme(legend.position="none")

ggplot(single_solutions, aes(x=Algorithm, y=var_components, fill=Algorithm)) +
  geom_boxplot(alpha=0.7) +
  theme_minimal() +
  labs(title="Comparación de la Varianza del tamaño de los Componentes",
       x="Strategy",
       y="Number of components") +
  theme(legend.position="none")


# Formato ancho
wide_data <- single_solutions %>%
  select(filename, Algorithm, n_components) %>%
  pivot_wider(names_from = Algorithm, values_from = n_components)

# Comprobar que no hay datos faltantes
wide_data <- drop_na(wide_data)

# Aplicar el test de Friedman
friedman.test(as.matrix(wide_data[, -1]))



# Varianza en formato ancho
wide_data_var <- single_solutions %>%
  select(filename, Algorithm, var_components) %>%
  pivot_wider(names_from = Algorithm, values_from = var_components)

# Comprobar que no hay datos faltantes
wide_data_var <- drop_na(wide_data_var)

# Aplicar el test de Friedman
friedman.test(as.matrix(wide_data_var[, -1]))


# Ejemplo de prueba post-hoc entre Initial y Bubble
wilcox.test(wide_data$Initial, wide_data$Bubble, paired=TRUE)
wilcox.test(wide_data$Initial, wide_data$HeuristicBalanced, paired=TRUE)
wilcox.test(wide_data$Bubble, wide_data$HeuristicBalanced, paired=TRUE)



# Ejemplo: número de componentes según poisson_param
ggplot(single_solutions, aes(x=poisson_param, y=n_components, color=Algorithm)) +
  geom_smooth(se=FALSE) +
  theme_minimal() +
  labs(title="Número de componentes vs. Parámetro de Poisson",
       x="Parámetro de Poisson",
       y="Número promedio de componentes")


# Ejemplo: Varianza según número de grupos
ggplot(single_solutions, aes(x=factor(groups), y=var_components, fill=Algorithm)) +
  geom_boxplot() +
  theme_minimal() +
  labs(title="Varianza vs. Número de grupos",
       x="Número de grupos",
       y="Varianza promedio")



###############################################


single_solutions <- AllExpResults %>%
  filter(Algorithm %in% c("Initial", "Bubble", "HeuristicBalanced"))



# Ajustar orden y etiquetas personalizadas
single_solutions <- single_solutions %>%
  mutate(Algorithm = factor(Algorithm, 
                            levels = c("Initial", "Bubble", "HeuristicBalanced"),
                            labels = c("Initial", "Bubble", "Heuristic")))

# Gráfico ajustado
p <- ggplot(single_solutions, aes(x = poisson_param, y = n_components, color = Algorithm)) +
  geom_smooth(se = FALSE, method = "loess", span = 0.25, linewidth=1.1) +
  theme_minimal(base_size = 14) +
  facet_wrap(~ groups, labeller = label_both) +
  scale_color_brewer(palette = "Dark2") +
  labs(title = "Number of components according to Poisson and number of groups",
       x = "Poisson parameter",
       y = "Number of components",
       color = "Strategy")

# Mostrar gráfico
print(p)



# Guardar en PDF 
ggsave("analysis/1_components_poisson_groups.pdf", plot = p, 
       width = 10, height = 6, units = "in")

# Guardar en JPG 
ggsave("analysis/1_components_poisson_groups.jpg", plot = p, 
       width = 10, height = 6, units = "in", dpi = 300)


#############################################################

single_solutions <- AllExpResults %>%
  filter(Algorithm %in% c("Initial", "Bubble", "HeuristicBalanced")) %>%
  mutate(Algorithm = factor(Algorithm, 
                            levels = c("Initial", "Bubble", "HeuristicBalanced"),
                            labels = c("Initial", "Bubble", "Heuristic")))

# Gráfico para la varianza
p_var <- ggplot(single_solutions, aes(x = poisson_param, y = var_components, color = Algorithm)) +
  geom_smooth(se = FALSE, method = "loess", span = 0.25, linewidth=1.1) +
  theme_minimal(base_size = 14) +
  facet_wrap(~ groups, labeller = label_both) +
  scale_color_brewer(palette = "Dark2") +
  labs(title = "Variance of component sizes according to Poisson and number of groups",
       x = "Poisson parameter",
       y = "Variance of component sizes",
       color = "Strategy")

# Mostrar gráfico
print(p_var)

# Guardar gráficos en la carpeta analysis
if(!dir.exists("analysis")) dir.create("analysis")

ggsave("analysis/2_variance_poisson_groups.pdf", plot = p_var, 
       width = 10, height = 6, units = "in")

ggsave("analysis/2_variance_poisson_groups.jpg", plot = p_var, 
       width = 10, height = 6, units = "in", dpi = 300)


########################################################


# Preparación de los datos (orden y etiquetas coherentes)
single_solutions <- AllExpResults %>%
  filter(Algorithm %in% c("Initial", "Bubble", "HeuristicBalanced")) %>%
  select(filename, groups, poisson_param, Algorithm, n_components, var_components) %>%
  mutate(Algorithm = factor(Algorithm,
                            levels = c("Initial", "Bubble", "HeuristicBalanced"),
                            labels = c("Initial", "Bubble", "Heuristic")))

# Colores consistentes (RColorBrewer)
palette_colors <- brewer.pal(3, "Dark2")

# Gráfico Boxplot Número de componentes
p1 <- ggplot(single_solutions, aes(x=Algorithm, y=n_components, fill=Algorithm)) +
  geom_boxplot(alpha=0.7) +
  scale_fill_manual(values = palette_colors) +
  theme_minimal(base_size = 14) +
  labs(title="Comparison of the number of connected components",
       x="Strategy",
       y="Number of components") +
  theme(legend.position="none")

print(p1)

# Guardar gráfico componentes
if(!dir.exists("analysis")) dir.create("analysis")

ggsave("analysis/3_boxplot_components.pdf", plot = p1,
       width = 8, height = 5, units = "in")

ggsave("analysis/3_boxplot_components.jpg", plot = p1,
       width = 8, height = 5, units = "in", dpi = 300)

# Gráfico Boxplot Varianza de componentes
p2 <- ggplot(single_solutions, aes(x=Algorithm, y=var_components, fill=Algorithm)) +
  geom_boxplot(alpha=0.7) +
  scale_fill_manual(values = palette_colors) +
  theme_minimal(base_size = 14) +
  labs(title="Comparison of variance of component sizes",
       x="Strategy",
       y="Variance of component sizes") +
  theme(legend.position="none")

print(p2)

# Guardar gráfico varianza
ggsave("analysis/4_boxplot_variance.pdf", plot = p2,
       width = 8, height = 5, units = "in")

ggsave("analysis/4_boxplot_variance.jpg", plot = p2,
       width = 8, height = 5, units = "in", dpi = 300)

# Análisis Estadístico (Test de Friedman)

# Número de componentes
wide_data <- single_solutions %>%
  select(filename, Algorithm, n_components) %>%
  pivot_wider(names_from = Algorithm, values_from = n_components) %>%
  drop_na()

test_components <- friedman.test(as.matrix(wide_data[, -1]))
print(test_components)

# Guardar resultado del test para número de componentes
sink("analysis/friedman_test_components.txt")
cat("Friedman Test for Number of Components:\n\n")
print(test_components)
sink()

# Varianza de componentes
wide_data_var <- single_solutions %>%
  select(filename, Algorithm, var_components) %>%
  pivot_wider(names_from = Algorithm, values_from = var_components) %>%
  drop_na()

test_variance <- friedman.test(as.matrix(wide_data_var[, -1]))
print(test_variance)


# Guardar resultado del test para varianza
sink("analysis/friedman_test_variance.txt")
cat("Friedman Test for Variance of Component Sizes:\n\n")
print(test_variance)
sink()


##########################################



# Función para obtener la frontera Pareto (maximizar n_components, minimizar var_components)
pareto_front <- function(df){
  dominated <- logical(nrow(df))
  for(i in seq_len(nrow(df))){
    dominated[i] <- any(
      (df$n_components >= df$n_components[i] & df$var_components <= df$var_components[i]) &
        (df$n_components > df$n_components[i] | df$var_components < df$var_components[i])
    )
  }
  return(df[!dominated, ])
}




# 1. Seleccionar columnas relevantes (incluye todo lo necesario)
pareto_data <- AllExpResults %>%
  select(filename, Algorithm, groups, poisson_param, seed,
         n_components, var_components) %>%
  distinct()

# 2. Obtener frontera de Pareto por problema (filename)
# Usamos todos los campos, no solo n_components y var_components
pareto_global_detailed <- pareto_data %>%
  group_by(filename) %>%
  nest() %>%
  mutate(
    pareto_points = map(data, pareto_front)
  ) %>%
  select(filename, pareto_points) %>%
  unnest(pareto_points)  # esto devuelve todos los campos originales de cada punto

pareto_with_algorithms <- pareto_global_detailed %>%
  group_by(filename, n_components, var_components, groups, poisson_param, seed) %>%
  summarise(
    Algorithms = paste(sort(unique(Algorithm)), collapse = ", "),
    .groups = "drop"
  )


# Puntos únicos de la frontera por problema
pareto_unique_points <- pareto_global_detailed %>%
  distinct(filename, n_components, var_components) %>%
  group_by(filename) %>%
  summarise(total_pareto_points = n(), .groups = "drop")

# Cuántos puntos de la frontera ha encontrado cada algoritmo por problema
pareto_points_by_algorithm <- pareto_global_detailed %>%
  distinct(filename, n_components, var_components, Algorithm) %>%
  group_by(filename, Algorithm) %>%
  summarise(points_found = n(), .groups = "drop")


pareto_coverage <- pareto_points_by_algorithm %>%
  left_join(pareto_unique_points, by = "filename") %>%
  mutate(percent_covered = round(100 * points_found / total_pareto_points, 2))


# Transformar a formato ancho
pareto_wide <- pareto_coverage %>%
  select(filename, Algorithm, points_found, total_pareto_points) %>%
  pivot_wider(
    names_from = Algorithm,
    values_from = points_found,
    values_fill = 0  # Si un algoritmo no está, se pone 0
  )


pareto_wide <- pareto_wide %>%
  mutate(filename_clean = str_remove(filename, "\\.csv$")) %>%
  separate(filename_clean, into = c("prefix", "groups", "students", "courses", "poisson_param", "seed"), sep = "_", remove = FALSE) %>%
  mutate(
    groups = as.integer(groups),
    poisson_param = as.numeric(poisson_param),
    seed = as.integer(seed)
  ) %>%
  select(-prefix, -students, -courses, -filename_clean)



# Reordenar columnas a tu gusto
pareto_final <- pareto_wide %>%
  relocate(filename, total_pareto_points, Initial, Bubble, HeuristicBalanced, SA, GA, groups, poisson_param, seed)


pareto_final <- pareto_final %>%
  rename(Heuristic = HeuristicBalanced)


write_csv(pareto_final, "analysis/pareto_coverage_wide.csv")


pareto_final_summary <- pareto_final %>%
  group_by(groups, poisson_param) %>%
  summarise(
    avg_total_pareto_points = mean(total_pareto_points),
    avg_initial = mean(Initial),
    avg_bubble = mean(Bubble),
    avg_heuristic = mean(Heuristic),
    avg_sa = mean(SA),
    avg_ga = mean(GA),
    .groups = "drop"
  )


pareto_final_summary <- pareto_final_summary %>%
  mutate(across(where(is.numeric), ~ round(.x, 2)))


write_csv(pareto_final_summary, "analysis/pareto_summary_by_group_poisson.csv")



pareto_long <- pareto_final_summary %>%
  pivot_longer(
    cols = starts_with("avg_"),
    names_to = "Metric",
    values_to = "Average"
  ) %>%
  mutate(
    Metric = recode(Metric,
                    "avg_total_pareto_points" = "Total Pareto Points",
                    "avg_initial" = "Initial",
                    "avg_bubble" = "Bubble",
                    "avg_heuristic" = "Heuristic",
                    "avg_sa" = "SA",
                    "avg_ga" = "GA"
    )
  )

# Asegurar orden y tipo de variable
pareto_long <- pareto_long %>%
  mutate(
    Metric = factor(Metric, levels = c("Total Pareto Points", "Initial", "Bubble", "Heuristic", "SA", "GA"))
  )

# Definir colores: uno especial para "Total", y Brewer para los demás
colors <- c(
  "Total Pareto Points" = "black",
  "Initial" = RColorBrewer::brewer.pal(6, "Dark2")[1],
  "Bubble" = RColorBrewer::brewer.pal(6, "Dark2")[2],
  "Heuristic" = RColorBrewer::brewer.pal(6, "Dark2")[3],
  "SA" = RColorBrewer::brewer.pal(6, "Dark2")[4],
  "GA" = RColorBrewer::brewer.pal(6, "Dark2")[5]
)

# Definir tipo de línea
linetypes <- c(
  "Total Pareto Points" = "dashed",
  "Initial" = "solid",
  "Bubble" = "solid",
  "Heuristic" = "solid",
  "SA" = "solid",
  "GA" = "solid"
)

p7<-ggplot(pareto_long, aes(x = poisson_param, y = Average, color = Metric, linetype = Metric, shape = Metric)) +
  geom_line(linewidth = 1.2) +
  geom_point(size = 2) +
  facet_grid(groups ~ ., labeller = label_both) +
  theme_minimal(base_size = 14) +
  scale_color_manual(values = colors) +
  scale_linetype_manual(values = linetypes) +
  scale_shape_manual(values = c(
    "Total Pareto Points" = 16,  # circle
    "Initial" = 15,              # square
    "Bubble" = 17,               # triangle
    "Heuristic" = 3,             # plus
    "SA" = 4,                    # x
    "GA" = 8                     # asterisk
  )) +
  labs(
    title = "Average number of Pareto points by strategy and Poisson parameter",
    x = "Poisson parameter",
    y = "Average number of points",
    color = "Metric",
    linetype = "Metric",
    shape = "Metric"
  )

print(p7)

# Guardar gráfico varianza
ggsave("analysis/7_Average number of Pareto points by strategy and Poisson parameter.pdf", plot = p7,
       width = 8, height = 5, units = "in")

ggsave("analysis/7_Average number of Pareto points by strategy and Poisson parameter.jpg", plot = p7,
       width = 8, height = 5, units = "in", dpi = 300)


##################################################################


pareto_percent_summary <- pareto_final_summary %>%
  mutate(
    pct_initial = 100 * avg_initial / avg_total_pareto_points,
    pct_bubble = 100 * avg_bubble / avg_total_pareto_points,
    pct_heuristic = 100 * avg_heuristic / avg_total_pareto_points,
    pct_sa = 100 * avg_sa / avg_total_pareto_points,
    pct_ga = 100 * avg_ga / avg_total_pareto_points
  )


pareto_percent_long <- pareto_percent_summary %>%
  select(groups, poisson_param,
         pct_initial, pct_bubble, pct_heuristic, pct_sa, pct_ga) %>%
  pivot_longer(
    cols = starts_with("pct_"),
    names_to = "Algorithm",
    values_to = "Percent"
  ) %>%
  mutate(
    Algorithm = recode(Algorithm,
                       "pct_initial" = "Initial",
                       "pct_bubble" = "Bubble",
                       "pct_heuristic" = "Heuristic",
                       "pct_sa" = "SA",
                       "pct_ga" = "GA"
    )
  )

pareto_percent_long <- pareto_percent_long %>%
  mutate(
    Algorithm = factor(Algorithm, levels = c("Initial", "Bubble", "Heuristic", "SA", "GA"))
  )


p9 <- ggplot(pareto_percent_long, aes(x = poisson_param, y = Algorithm, fill = Percent)) +
  geom_tile(color = "white") +
  facet_grid(groups ~ ., labeller = label_both) +
  scale_fill_viridis(
    name = "% of Pareto points",
    limits = c(0, 100),
    option = "D",
    direction = -1  # ← Inversión del color
  ) +
  theme_minimal(base_size = 14) +
  labs(
    title = "Percentage of Pareto points found by strategy",
    x = "Poisson parameter",
    y = "Strategy"
  )



print(p9)

ggsave("analysis/9_Pareto_points_percent_by_strategy_and_poisson_ordered.pdf", plot = p9,
       width = 8, height = 6, units = "in")

ggsave("analysis/9_Pareto_points_percent_by_strategy_and_poisson_ordered.jpg", plot = p9,
       width = 8, height = 6, units = "in", dpi = 300)
