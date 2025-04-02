# Load necessary libraries
library(tidyverse)
library(RColorBrewer)
library(viridis)

# Load all CSV files in 'results' folder that match the pattern "experiment_results_*.csv"
SAexpResults <- list.files(path = "results", 
                           pattern = "experiment_results_.*\\.csv$", 
                           full.names = TRUE) %>%
  map_df(read_csv)

# Preview the first rows of the Simulated Annealing results
print(SAexpResults)

# Load all Genetic Algorithm results from the 'results/GA' folder
GAexpResults <- list.files(path = "results/GA", 
                           pattern = "experiment_results_GA_.*\\.csv$", 
                           full.names = TRUE) %>%
  map_df(read_csv)

# Preview the first rows of the GA results
print(GAexpResults)

# Load the basic results summary
BasicexpResults <- read_csv("results/results_summary.csv")

# Transform basic results into long format and label the algorithms
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

# Preview the transformed basic results
print(BasicexpResults_long)

# Transform Simulated Annealing results into long format
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
  ) %>%
  mutate(filename = str_replace(filename, "_datosNivelados\\.rds$", ".csv"))

# Preview SA results in long format
print(SAexpResults_long)

# Extract initial solution performance from SA results where w1 = 0 (only structure considered)
HeuristicexpResults_long <- SAexpResults %>%
  filter(w1 == 0) %>%
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
  ) %>%
  mutate(filename = str_replace(filename, "_datosNivelados\\.rds$", ".csv"))

# Transform GA results into long format
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

# Merge all experimental results into one table
AllExpResults <- bind_rows(BasicexpResults_long, SAexpResults_long, GAexpResults_long, HeuristicexpResults_long)

# Ensure var_components has no missing values
AllExpResults <- AllExpResults %>%
  mutate(var_components = replace_na(var_components, 0))

# Compute Pareto indicators: MinVar (minimum variance) and Pareto dominance
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

# Jitter value to avoid overlapping points
jitter <- 0.05

# Generate and save Pareto front plots per problem instance
AllExpResults %>%
  filter(Pareto == 1) %>%
  group_by(filename) %>%
  group_walk(~ {
    plot <- ggplot(.x, aes(x = n_components, y = var_components, color = Algorithm)) +
      geom_point(size = 3, position = position_jitter(width = jitter, height = jitter)) +
      geom_line(aes(group = Algorithm), position = position_jitter(width = jitter, height = jitter)) +
      labs(title = .y$filename,
           x = "Number of components",
           y = "Variance",
           color = "Algorithm") +
      theme_minimal()
    
    ggsave(filename = paste0("pareto/", .y$filename, ".jpg"), plot = plot, width = 8, height = 6)
  })

# Save the complete data frame with all experimental results to a CSV file
write_csv(AllExpResults, "results/AllExpResults.csv")



#########################################
#### Analysis of the three single-solution techniques ####
#########################################

# Filter only initial strategies with single solutions
single_solutions <- AllExpResults %>%
  filter(Algorithm %in% c("Initial", "Bubble", "HeuristicBalanced")) %>%
  select(filename, groups, poisson_param, Algorithm, n_components, var_components)

# Boxplot of the number of components by strategy
ggplot(single_solutions, aes(x = Algorithm, y = n_components, fill = Algorithm)) +
  geom_boxplot(alpha = 0.7) +
  theme_minimal() +
  labs(title = "Comparison of the Number of Connected Components",
       x = "Strategy",
       y = "Number of components") +
  theme(legend.position = "none")

# Boxplot of variance by strategy
ggplot(single_solutions, aes(x = Algorithm, y = var_components, fill = Algorithm)) +
  geom_boxplot(alpha = 0.7) +
  theme_minimal() +
  labs(title = "Comparison of Component Size Variance",
       x = "Strategy",
       y = "Variance") +
  theme(legend.position = "none")

# Convert to wide format to apply Friedman test on number of components
wide_data <- single_solutions %>%
  select(filename, Algorithm, n_components) %>%
  pivot_wider(names_from = Algorithm, values_from = n_components) %>%
  drop_na()

# Friedman test (non-parametric equivalent of repeated-measures ANOVA)
friedman.test(as.matrix(wide_data[, -1]))

# Same for variance
wide_data_var <- single_solutions %>%
  select(filename, Algorithm, var_components) %>%
  pivot_wider(names_from = Algorithm, values_from = var_components) %>%
  drop_na()

friedman.test(as.matrix(wide_data_var[, -1]))

# Pairwise post-hoc comparisons using Wilcoxon test (paired)
wilcox.test(wide_data$Initial, wide_data$Bubble, paired = TRUE)
wilcox.test(wide_data$Initial, wide_data$HeuristicBalanced, paired = TRUE)
wilcox.test(wide_data$Bubble, wide_data$HeuristicBalanced, paired = TRUE)

# Trend of number of components across Poisson values
ggplot(single_solutions, aes(x = poisson_param, y = n_components, color = Algorithm)) +
  geom_smooth(se = FALSE) +
  theme_minimal() +
  labs(title = "Number of Components vs. Poisson Parameter",
       x = "Poisson Parameter",
       y = "Number of Components")

# Boxplot of variance grouped by number of groups
ggplot(single_solutions, aes(x = factor(groups), y = var_components, fill = Algorithm)) +
  geom_boxplot() +
  theme_minimal() +
  labs(title = "Variance by Number of Groups",
       x = "Number of Groups",
       y = "Variance")

###############################################
#### Number of Components vs. Poisson Parameter by Strategy and Groups
###############################################

# Prepare solution subset and define ordering
single_solutions <- AllExpResults %>%
  filter(Algorithm %in% c("Initial", "Bubble", "HeuristicBalanced")) %>%
  mutate(Algorithm = factor(Algorithm, 
                            levels = c("Initial", "Bubble", "HeuristicBalanced"),
                            labels = c("Initial", "Bubble", "Heuristic")))

# Line plot with LOESS smoothing
p <- ggplot(single_solutions, aes(x = poisson_param, y = n_components, color = Algorithm)) +
  geom_smooth(se = FALSE, method = "loess", span = 0.25, linewidth = 1.1) +
  theme_minimal(base_size = 14) +
  facet_wrap(~ groups, labeller = label_both) +
  scale_color_brewer(palette = "Dark2") +
  labs(title = "Number of Components by Poisson Parameter and Number of Groups",
       x = "Poisson Parameter",
       y = "Number of Components",
       color = "Strategy")

# Save the plot
ggsave("analysis/1_components_poisson_groups.pdf", plot = p, width = 10, height = 6)
ggsave("analysis/1_components_poisson_groups.jpg", plot = p, width = 10, height = 6, dpi = 300)

#############################################################
#### Variance of Components vs. Poisson Parameter by Strategy and Groups
#############################################################

# Reuse same subset with renamed strategy
single_solutions <- single_solutions %>%
  mutate(Algorithm = factor(Algorithm, 
                            levels = c("Initial", "Bubble", "Heuristic"),
                            labels = c("Initial", "Bubble", "Heuristic")))

# Line plot of variance
p_var <- ggplot(single_solutions, aes(x = poisson_param, y = var_components, color = Algorithm)) +
  geom_smooth(se = FALSE, method = "loess", span = 0.25, linewidth = 1.1) +
  theme_minimal(base_size = 14) +
  facet_wrap(~ groups, labeller = label_both) +
  scale_color_brewer(palette = "Dark2") +
  labs(title = "Variance of Component Sizes by Poisson Parameter and Number of Groups",
       x = "Poisson Parameter",
       y = "Variance of Component Sizes",
       color = "Strategy")

# Save the plot
ggsave("analysis/2_variance_poisson_groups.pdf", plot = p_var, width = 10, height = 6)
ggsave("analysis/2_variance_poisson_groups.jpg", plot = p_var, width = 10, height = 6, dpi = 300)

########################################################
#### Boxplots: Number of Components and Variance by Strategy
########################################################

# Use color palette from RColorBrewer
palette_colors <- brewer.pal(3, "Dark2")

# Boxplot of number of components
p1 <- ggplot(single_solutions, aes(x = Algorithm, y = n_components, fill = Algorithm)) +
  geom_boxplot(alpha = 0.7) +
  scale_fill_manual(values = palette_colors) +
  theme_minimal(base_size = 14) +
  labs(title = "Comparison of Number of Connected Components",
       x = "Strategy",
       y = "Number of Components") +
  theme(legend.position = "none")

ggsave("analysis/3_boxplot_components.pdf", plot = p1, width = 8, height = 5)
ggsave("analysis/3_boxplot_components.jpg", plot = p1, width = 8, height = 5, dpi = 300)

# Boxplot of component size variance
p2 <- ggplot(single_solutions, aes(x = Algorithm, y = var_components, fill = Algorithm)) +
  geom_boxplot(alpha = 0.7) +
  scale_fill_manual(values = palette_colors) +
  theme_minimal(base_size = 14) +
  labs(title = "Comparison of Component Size Variance",
       x = "Strategy",
       y = "Variance of Component Sizes") +
  theme(legend.position = "none")

ggsave("analysis/4_boxplot_variance.pdf", plot = p2, width = 8, height = 5)
ggsave("analysis/4_boxplot_variance.jpg", plot = p2, width = 8, height = 5, dpi = 300)

######################################################
#### Friedman Tests on Number of Components and Variance
######################################################

# Friedman test on number of components
wide_data <- single_solutions %>%
  select(filename, Algorithm, n_components) %>%
  pivot_wider(names_from = Algorithm, values_from = n_components) %>%
  drop_na()

test_components <- friedman.test(as.matrix(wide_data[, -1]))

# Save the test result
sink("analysis/friedman_test_components.txt")
cat("Friedman Test for Number of Components:\n\n")
print(test_components)
sink()

# Friedman test on variance
wide_data_var <- single_solutions %>%
  select(filename, Algorithm, var_components) %>%
  pivot_wider(names_from = Algorithm, values_from = var_components) %>%
  drop_na()

test_variance <- friedman.test(as.matrix(wide_data_var[, -1]))

sink("analysis/friedman_test_variance.txt")
cat("Friedman Test for Variance of Component Sizes:\n\n")
print(test_variance)
sink()
