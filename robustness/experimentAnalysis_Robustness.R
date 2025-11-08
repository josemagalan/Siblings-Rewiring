# Load necessary libraries
library(tidyverse)
library(RColorBrewer)
library(viridis)

# Load all CSV files in 'results' folder that match the pattern "experiment_results_*.csv"
SAexpResults_robustness <- list.files(path = "resultsRobustness", 
                           pattern = "experiment_results_.*\\.csv$", 
                           full.names = TRUE) %>%
  map_df(read_csv)

# Preview the first rows of the Simulated Annealing results
print(SAexpResults)

# Load all Genetic Algorithm results from the 'results/GA' folder
GAexpResults_robustness <- list.files(path = "resultsRobustness/GA", 
                           pattern = "experiment_results_GA_.*\\.csv$", 
                           full.names = TRUE) %>%
  map_df(read_csv)

# Preview the first rows of the GA results
print(GAexpResults)

# Load the basic results summary
BasicexpResults_robustness <- read_csv("resultsRobustness/results_summary_robustness.csv")

# Transform basic results into long format and label the algorithms
BasicexpResults_long_robustness <- BasicexpResults_robustness %>%
  select(filename, distribution,groups, students_per_group, courses, mu_target,r,p, seed,
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

# Preview the transformed basic results
print(BasicexpResults_long_robustness)

# Transform Simulated Annealing results into long format
SAexpResults_robustness_long <- SAexpResults_robustness %>%
  transmute(
    filename = archivo,
    distribution=distribution,
    groups = grupos,
    students_per_group = students_per_group,
    courses = cursos,
    mu_target=mu_target,
    r=r,
    p=p,
    seed = semilla,
    Algorithm = "SA",
    n_components = n_componentes_final,
    var_components = varianza_final
  ) %>%
  mutate(filename = str_replace(filename, "_datosNivelados\\.rds$", ".csv"))

# Preview SA results in long format
print(SAexpResults_robustness_long)

# Extract initial solution performance from SA results where w1 = 0 (only structure considered)
HeuristicexpResults_robustness_long <- SAexpResults_robustness %>%
  filter(w1 == 0) %>%
  transmute(
    filename = archivo,
    distribution=distribution,
    groups = grupos,
    students_per_group = students_per_group,
    courses = cursos,
    mu_target=mu_target,
    r=r,
    p=p,
    seed = semilla,
    Algorithm = "HeuristicBalanced",
    n_components = n_componentes_inicial,
    var_components = varianza_inicial
  ) %>%
  mutate(filename = str_replace(filename, "_datosNivelados\\.rds$", ".csv"))

GAexpResults_robustness <- GAexpResults_robustness %>%
  mutate(grupos = str_extract(archivo, "_\\dG_") |>        # extrae por ejemplo "_2G_"
           str_remove_all("[^0-9]") |>              # deja solo el número
           as.integer())                            # lo convierte a número


# Transform GA results into long format
GAexpResults_robustness_long <- GAexpResults_robustness %>%
  transmute(
    filename = archivo,
    distribution=distribution,
    groups = grupos,
    students_per_group = students_per_group,
    courses = cursos,
    mu_target=mu_target,
    r=r,
    p=p,
    seed = semilla,
    Algorithm = "GA",
    n_components = n_componentes_final,
    var_components = varianza_final
  )

# Merge all experimental results into one table
AllExpResults_robustness <- bind_rows(BasicexpResults_long_robustness, SAexpResults_robustness_long, GAexpResults_robustness_long, HeuristicexpResults_robustness_long)

# Ensure var_components has no missing values
AllExpResults_robustness <- AllExpResults_robustness %>%
  mutate(var_components = replace_na(var_components, 0))

# Compute Pareto indicators: MinVar (minimum variance) and Pareto dominance
AllExpResults_robustness <- AllExpResults_robustness %>%
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


# Save the complete data frame with all experimental results to a CSV file
write_csv(AllExpResults_robustness, "resultsRobustness/AllExpResults_robustness.csv")



#########################################
#### Analysis of the three single-solution techniques ####
#########################################

# Filter only initial strategies with single solutions
single_solutions_robustness <- AllExpResults_robustness %>%
  filter(Algorithm %in% c("Initial", "Bubble", "HeuristicBalanced")) %>%
  select(filename,distribution, groups,students_per_group, mu_target,r,p, Algorithm, n_components, var_components)




# Boxplot of the number of components by strategy
ggplot(single_solutions_robustness, aes(x = Algorithm, y = n_components, fill = Algorithm)) +
  geom_boxplot(alpha = 0.7) +
  theme_minimal() +
  labs(title = "Comparison of the Number of Connected Components",
       x = "Strategy",
       y = "Number of components") +
  theme(legend.position = "none")

# Boxplot of variance by strategy
ggplot(single_solutions_robustness, aes(x = Algorithm, y = var_components, fill = Algorithm)) +
  geom_boxplot(alpha = 0.7) +
  theme_minimal() +
  labs(title = "Comparison of Component Size Variance",
       x = "Strategy",
       y = "Variance") +
  theme(legend.position = "none")

# Convert to wide format to apply Friedman test on number of components
wide_data <- single_solutions_robustness %>%
  select(filename, Algorithm, n_components) %>%
  pivot_wider(names_from = Algorithm, values_from = n_components) %>%
  drop_na()

# Friedman test (non-parametric equivalent of repeated-measures ANOVA)
friedman.test(as.matrix(wide_data[, -1]))

# Same for variance
wide_data_var <- single_solutions_robustness %>%
  select(filename, Algorithm, var_components) %>%
  pivot_wider(names_from = Algorithm, values_from = var_components) %>%
  drop_na()

friedman.test(as.matrix(wide_data_var[, -1]))

# Pairwise post-hoc comparisons using Wilcoxon test (paired)
wilcox.test(wide_data$Initial, wide_data$Bubble, paired = TRUE)
wilcox.test(wide_data$Initial, wide_data$HeuristicBalanced, paired = TRUE)
wilcox.test(wide_data$Bubble, wide_data$HeuristicBalanced, paired = TRUE)


########################################################
#### Boxplots: Number of Components and Variance by Strategy
########################################################

# Use color palette from RColorBrewer
palette_colors <- brewer.pal(3, "Dark2")

# Boxplot of number of components
p1 <- ggplot(single_solutions_robustness, aes(x = Algorithm, y = n_components, fill = Algorithm)) +
  geom_boxplot(alpha = 0.7) +
  scale_fill_manual(values = palette_colors) +
  theme_minimal(base_size = 14) +
  labs(title = "Comparison of Number of Connected Components",
       x = "Strategy",
       y = "Number of Components") +
  theme(legend.position = "none")

ggsave("resultsRobustness/Figuras robustness/3_boxplot_components.pdf", plot = p1, width = 8, height = 5)
ggsave("resultsRobustness/Figuras robustness/3_boxplot_components.jpg", plot = p1, width = 8, height = 5, dpi = 300)

# Boxplot of component size variance
p2 <- ggplot(single_solutions_robustness, aes(x = Algorithm, y = var_components, fill = Algorithm)) +
  geom_boxplot(alpha = 0.7) +
  scale_fill_manual(values = palette_colors) +
  theme_minimal(base_size = 14) +
  labs(title = "Comparison of Component Size Variance",
       x = "Strategy",
       y = "Variance of Component Sizes") +
  theme(legend.position = "none")

ggsave("resultsRobustness/Figuras robustness/4_boxplot_variance.pdf", plot = p2, width = 8, height = 5)
ggsave("resultsRobustness/Figuras robustness/4_boxplot_variance.jpg", plot = p2, width = 8, height = 5, dpi = 300)

######################################################
#### Friedman Tests on Number of Components and Variance
######################################################

# Friedman test on number of components
wide_data <- single_solutions_robustness %>%
  select(filename, Algorithm, n_components) %>%
  pivot_wider(names_from = Algorithm, values_from = n_components) %>%
  drop_na()

test_components <- friedman.test(as.matrix(wide_data[, -1]))

# Save the test result
sink("resultsRobustness/Figuras robustness/friedman_test_components.txt")
cat("Friedman Test for Number of Components:\n\n")
print(test_components)
sink()

# Friedman test on variance
wide_data_var <- single_solutions_robustness %>%
  select(filename, Algorithm, var_components) %>%
  pivot_wider(names_from = Algorithm, values_from = var_components) %>%
  drop_na()

test_variance <- friedman.test(as.matrix(wide_data_var[, -1]))

sink("resultsRobustness/Figuras robustness/friedman_test_variance.txt")
cat("Friedman Test for Variance of Component Sizes:\n\n")
print(test_variance)
sink()


#########################################



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
pareto_data <- AllExpResults_robustness %>%
  select(filename, Algorithm, groups, students_per_group, mu_target,r,p,seed,
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
  group_by(filename, n_components, var_components, groups, students_per_group, mu_target,r,p,seed) %>%
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


##################
#empirical
##################


pareto_wide_EMP   <- pareto_wide %>% filter(str_detect(filename, regex("EMP",   ignore_case = TRUE)))
pareto_wide_GEOM  <- pareto_wide %>% filter(str_detect(filename, regex("GEOM",  ignore_case = TRUE)))
pareto_wide_NEGBIN<- pareto_wide %>% filter(str_detect(filename, regex("NEGBIN",ignore_case = TRUE)))



pareto_wide_EMP <- pareto_wide_EMP %>%
  mutate(filename_clean = str_remove(filename, "\\.csv$")) %>%
  separate(
    filename_clean,
    into = c("prefix", "dist", "groups", "students", "courses", "fixed", "seed"),
    sep = "_",
    remove = FALSE
  ) %>%
  mutate(
    groups   = as.integer(str_remove(groups, "G")),
    students = as.integer(str_remove(students, "S")),
    courses  = as.integer(str_remove(courses, "C")),
    fixed    = fixed == "fixed",   
    seed     = as.integer(seed)
  ) %>%
  select(-prefix, -dist, -filename_clean)


pareto_wide_GEOM <- pareto_wide_GEOM %>%
  mutate(filename_clean = str_remove(filename, "\\.csv$")) %>%
  separate(
    filename_clean,
    into = c("prefix", "dist", "groups", "students", "courses", "mu", "p", "seed"),
    sep = "_",
    remove = FALSE
  ) %>%
  mutate(
    groups   = as.integer(str_remove(groups, "G")),
    students = as.integer(str_remove(students, "S")),
    courses  = as.integer(str_remove(courses, "C")),
    mu       = as.numeric(str_remove(mu, "mu")),
    p        = as.numeric(str_remove(p, "p")),
    seed     = as.integer(seed)
  ) %>%
  select(-prefix, -dist, -filename_clean)


pareto_wide_NEGBIN <- pareto_wide_NEGBIN %>%
  mutate(filename_clean = str_remove(filename, "\\.csv$")) %>%
  separate(
    filename_clean,
    into = c("prefix", "dist", "groups", "students", "courses", "mu", "r", "p", "seed"),
    sep = "_",
    remove = FALSE
  ) %>%
  mutate(
    groups   = as.integer(str_remove(groups, "G")),
    students = as.integer(str_remove(students, "S")),
    courses  = as.integer(str_remove(courses, "C")),
    mu       = as.numeric(str_remove(mu, "^mu")),
    r        = as.numeric(str_remove(r, "^r")),
    p        = as.numeric(str_remove(p, "^p")),
    seed     = as.integer(seed)
  ) %>%
  select(-prefix, -dist, -filename_clean)





# Reordenar columnas a tu gusto
pareto_wide_EMP <- pareto_wide_EMP %>%
  rename(Heuristic = HeuristicBalanced) %>%
  relocate(
    filename,
    total_pareto_points,
    Initial, Bubble, Heuristic, SA, GA,
    groups, fixed, seed
  )
pareto_wide_GEOM <- pareto_wide_GEOM %>%
  rename(Heuristic = HeuristicBalanced) %>%
  relocate(
    filename,
    total_pareto_points,
    Initial, Bubble, Heuristic, SA, GA,
    groups, mu, p, seed
  )
pareto_wide_NEGBIN <- pareto_wide_NEGBIN %>%
  rename(Heuristic = HeuristicBalanced) %>%
  relocate(
    filename,
    total_pareto_points,
    Initial, Bubble, Heuristic, SA, GA,
    groups, mu, r, p, seed
  )





pareto_final_summary_EMP <- pareto_wide_EMP %>%
  group_by(groups, students, fixed) %>%
  summarise(
    avg_total_pareto_points = mean(total_pareto_points, na.rm = TRUE),
    avg_initial   = mean(Initial,  na.rm = TRUE),
    avg_bubble    = mean(Bubble,   na.rm = TRUE),
    avg_heuristic = mean(Heuristic,na.rm = TRUE),
    avg_sa        = mean(SA,       na.rm = TRUE),
    avg_ga        = mean(GA,       na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(across(where(is.numeric), ~ round(.x, 2)))

pareto_final_summary_GEOM <- pareto_wide_GEOM %>%
  group_by(groups, students, mu, p) %>%
  summarise(
    avg_total_pareto_points = mean(total_pareto_points, na.rm = TRUE),
    avg_initial   = mean(Initial,  na.rm = TRUE),
    avg_bubble    = mean(Bubble,   na.rm = TRUE),
    avg_heuristic = mean(Heuristic,na.rm = TRUE),
    avg_sa        = mean(SA,       na.rm = TRUE),
    avg_ga        = mean(GA,       na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(across(where(is.numeric), ~ round(.x, 2)))


pareto_final_summary_NEGBIN <- pareto_wide_NEGBIN %>%
  group_by(groups, students, mu, r, p) %>%
  summarise(
    avg_total_pareto_points = mean(total_pareto_points, na.rm = TRUE),
    avg_initial   = mean(Initial,  na.rm = TRUE),
    avg_bubble    = mean(Bubble,   na.rm = TRUE),
    avg_heuristic = mean(Heuristic,na.rm = TRUE),
    avg_sa        = mean(SA,       na.rm = TRUE),
    avg_ga        = mean(GA,       na.rm = TRUE),
    .groups = "drop"
  ) %>%
  mutate(across(where(is.numeric), ~ round(.x, 2)))


pareto_percent_summary_EMP <- pareto_final_summary_EMP %>%
  mutate(
    pct_initial   = 100 * avg_initial   / avg_total_pareto_points,
    pct_bubble    = 100 * avg_bubble    / avg_total_pareto_points,
    pct_heuristic = 100 * avg_heuristic / avg_total_pareto_points,
    pct_sa        = 100 * avg_sa        / avg_total_pareto_points,
    pct_ga        = 100 * avg_ga        / avg_total_pareto_points
  ) %>%
  mutate(across(starts_with("pct_"), ~ round(.x, 2)))


pareto_percent_summary_GEOM <- pareto_final_summary_GEOM %>%
  mutate(
    pct_initial   = 100 * avg_initial   / avg_total_pareto_points,
    pct_bubble    = 100 * avg_bubble    / avg_total_pareto_points,
    pct_heuristic = 100 * avg_heuristic / avg_total_pareto_points,
    pct_sa        = 100 * avg_sa        / avg_total_pareto_points,
    pct_ga        = 100 * avg_ga        / avg_total_pareto_points
  ) %>%
  mutate(across(starts_with("pct_"), ~ round(.x, 2)))


pareto_percent_summary_NEGBIN <- pareto_final_summary_NEGBIN %>%
  mutate(
    pct_initial   = 100 * avg_initial   / avg_total_pareto_points,
    pct_bubble    = 100 * avg_bubble    / avg_total_pareto_points,
    pct_heuristic = 100 * avg_heuristic / avg_total_pareto_points,
    pct_sa        = 100 * avg_sa        / avg_total_pareto_points,
    pct_ga        = 100 * avg_ga        / avg_total_pareto_points
  ) %>%
  mutate(across(starts_with("pct_"), ~ round(.x, 2)))


#####Graficos EMP


# Carpeta de salida
outdir <- "resultsRobustness/Figuras robustness"
dir.create(outdir, recursive = TRUE, showWarnings = FALSE)

# Long EMP
pareto_percent_long_EMP <- pareto_percent_summary_EMP %>%
  select(groups, students,
         pct_initial, pct_bubble, pct_heuristic, pct_sa, pct_ga) %>%
  pivot_longer(
    cols = starts_with("pct_"),
    names_to = "Algorithm",
    values_to = "Percent"
  ) %>%
  mutate(
    Algorithm = recode(Algorithm,
                       "pct_initial"   = "Initial",
                       "pct_bubble"    = "Bubble",
                       "pct_heuristic" = "Heuristic",
                       "pct_sa"        = "SA",
                       "pct_ga"        = "GA"),
    Algorithm = factor(Algorithm, levels = c("Initial", "Bubble", "Heuristic", "SA", "GA"))
  )


# Para que el eje X muestre exactamente los valores de students presentes
students_breaks <- sort(unique(pareto_percent_long_EMP$students))

p_EMP <- ggplot(pareto_percent_long_EMP,
                aes(x = students, y = Algorithm, fill = Percent)) +
  geom_tile(color = "white") +
  facet_wrap(~ groups, ncol = 1, labeller = label_both) +   # ← tres gráficos, uno por 'groups'
  scale_x_continuous(breaks = students_breaks) +
  scale_fill_viridis(
    name = "% of Pareto points",
    limits = c(0, 100),
    option = "D",
    direction = -1
  ) +
  theme_minimal(base_size = 14) +
  labs(
    title = "EMP — % Pareto points by strategy",
    x = "Students per group",
    y = "Strategy"
  )

print(p_EMP)

ggsave(file.path(outdir, "EMP_Pareto_points_percent_by_strategy_students_by_groups.pdf"),
       plot = p_EMP, width = 10, height = 5, units = "in")
ggsave(file.path(outdir, "EMP_Pareto_points_percent_by_strategy_students_by_groups.jpg"),
       plot = p_EMP, width = 10, height = 5, units = "in", dpi = 300)





# Carpeta de salida (por si no existe)
outdir <- "resultsRobustness/Figuras robustness"
dir.create(outdir, recursive = TRUE, showWarnings = FALSE)

# 1) Pasar a formato largo para GEOM (si no lo tienes ya)
pareto_percent_long_GEOM <- pareto_final_summary_GEOM %>%
  select(groups, students, p,
         avg_total_pareto_points,
         avg_initial, avg_bubble, avg_heuristic, avg_sa, avg_ga) %>%
  mutate(
    pct_initial   = 100 * avg_initial   / avg_total_pareto_points,
    pct_bubble    = 100 * avg_bubble    / avg_total_pareto_points,
    pct_heuristic = 100 * avg_heuristic / avg_total_pareto_points,
    pct_sa        = 100 * avg_sa        / avg_total_pareto_points,
    pct_ga        = 100 * avg_ga        / avg_total_pareto_points
  ) %>%
  pivot_longer(
    cols = starts_with("pct_"),
    names_to = "Algorithm",
    values_to = "Percent"
  ) %>%
  mutate(
    Algorithm = recode(Algorithm,
                       "pct_initial"   = "Initial",
                       "pct_bubble"    = "Bubble",
                       "pct_heuristic" = "Heuristic",
                       "pct_sa"        = "SA",
                       "pct_ga"        = "GA"),
    Algorithm = factor(Algorithm, levels = c("Initial", "Bubble", "Heuristic", "SA", "GA"))
  )

# 2) Generar un gráfico por cada valor de students
students_vals <- sort(unique(pareto_percent_long_GEOM$students))

for (s in students_vals) {
  df_s <- pareto_percent_long_GEOM %>% filter(students == s)
  p_breaks <- sort(unique(df_s$p))
  
  p_geom_s <- ggplot(df_s, aes(x = factor(p), y = Algorithm, fill = Percent)) +
    geom_tile(color = "white") +
    facet_wrap(~ groups, ncol = 1, labeller = label_both) +
    scale_fill_viridis(
      name = "% of Pareto points",
      limits = c(0, 100),
      option = "D",
      direction = -1
    ) +
    theme_minimal(base_size = 14) +
    theme(
      strip.text = element_text(size = 12, face = "bold"),
      panel.spacing.y = unit(0.6, "cm")
    ) +
    labs(
      title = paste0("GEOM — % Pareto points by strategy (students = ", s, ")"),
      x = "Geometric parameter p",
      y = "Strategy"
    )
  
  
  print(p_geom_s)
  
  base <- paste0("GEOM_Pareto_points_percent_by_strategy_p_by_groups_students", s)
  ggsave(file.path(outdir, paste0(base, ".pdf")), plot = p_geom_s, width = 7, height = 9, units = "in")
  ggsave(file.path(outdir, paste0(base, ".jpg")), plot = p_geom_s, width = 7, height = 9, units = "in", dpi = 300)
}

library(dplyr)
library(tidyr)
library(ggplot2)
library(viridis)

# Carpeta de salida
outdir <- "resultsRobustness/Figuras robustness"
dir.create(outdir, recursive = TRUE, showWarnings = FALSE)

# 1) Pasar a formato largo (si no lo tienes ya)
pareto_percent_long_NEGBIN <- pareto_final_summary_NEGBIN %>%
  select(groups, students, r, mu,
         avg_total_pareto_points,
         avg_initial, avg_bubble, avg_heuristic, avg_sa, avg_ga) %>%
  mutate(
    pct_initial   = 100 * avg_initial   / avg_total_pareto_points,
    pct_bubble    = 100 * avg_bubble    / avg_total_pareto_points,
    pct_heuristic = 100 * avg_heuristic / avg_total_pareto_points,
    pct_sa        = 100 * avg_sa        / avg_total_pareto_points,
    pct_ga        = 100 * avg_ga        / avg_total_pareto_points
  ) %>%
  pivot_longer(
    cols = starts_with("pct_"),
    names_to = "Algorithm",
    values_to = "Percent"
  ) %>%
  mutate(
    Algorithm = recode(Algorithm,
                       "pct_initial"   = "Initial",
                       "pct_bubble"    = "Bubble",
                       "pct_heuristic" = "Heuristic",
                       "pct_sa"        = "SA",
                       "pct_ga"        = "GA"),
    Algorithm = factor(Algorithm, levels = c("Initial", "Bubble", "Heuristic", "SA", "GA"))
  )

# 2) Un gráfico por combinación (students, r); paneles por groups
students_vals <- sort(unique(pareto_percent_long_NEGBIN$students))
r_vals <- sort(unique(pareto_percent_long_NEGBIN$r))

for (s in students_vals) {
  for (rv in r_vals) {
    df_sr <- pareto_percent_long_NEGBIN %>% filter(students == s, r == rv)
    if (nrow(df_sr) == 0) next
    
    mu_levels <- sort(unique(df_sr$mu))
    
    p_negbin_sr <- ggplot(df_sr, aes(x = factor(mu, levels = mu_levels),
                                     y = Algorithm, fill = Percent)) +
      geom_tile(color = "white") +
      facet_wrap(~ groups, ncol = 1, labeller = label_both) +  # apilados verticalmente
      scale_fill_viridis(
        name = "% of Pareto points",
        limits = c(0, 100),
        option = "D",
        direction = -1
      ) +
      theme_minimal(base_size = 14) +
      theme(
        strip.text = element_text(size = 12, face = "bold"),
        panel.spacing.y = unit(0.6, "cm")
      ) +
      labs(
        title = paste0("NEGBIN — % Pareto points by strategy (students = ", s, ", r = ", rv, ")"),
        x = "NegBin parameter μ",
        y = "Strategy"
      )
    
    print(p_negbin_sr)
    
    base <- paste0("NEGBIN_Pareto_points_percent_by_strategy_mu_by_groups_students", s, "_r", rv)
    ggsave(file.path(outdir, paste0(base, ".pdf")), plot = p_negbin_sr, width = 7, height = 9, units = "in")
    ggsave(file.path(outdir, paste0(base, ".jpg")), plot = p_negbin_sr, width = 7, height = 9, units = "in", dpi = 300)
  }
}



