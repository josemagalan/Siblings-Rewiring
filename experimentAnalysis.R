# Load necessary libraries
library(tidyverse)
library(RColorBrewer)
library(viridis)

# Load all CSV files in 'results' that match "experiment_results_*.csv" (Simulated Annealing runs)
SAexpResults <- list.files(path = "results", 
                           pattern = "experiment_results_.*\\.csv$", 
                           full.names = TRUE) %>%
  map_df(read_csv)

# Quick preview of Simulated Annealing results
print(SAexpResults)

# Load all Genetic Algorithm CSV results from 'results/GA'
GAexpResults <- list.files(path = "results/GA", 
                           pattern = "experiment_results_GA_.*\\.csv$", 
                           full.names = TRUE) %>%
  map_df(read_csv)

# Quick preview of GA results
print(GAexpResults)

# Load the basic results summary (Initial and Bubble baselines)
BasicexpResults <- read_csv("results/results_summary.csv")

# Reshape basic results to long format and label algorithms (Initial vs Bubble)
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

# Preview reshaped basic results
print(BasicexpResults_long)

# Convert SA results to a unified long format (final solution metrics)
# NOTE: the source data column is named 'poison' (kept as-is), representing the Poisson parameter
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

# Extract the initial (pre-annealing) performance from SA runs where w1 = 0 (structure-only weight)
# This corresponds to the leveled heuristic baseline we call "HeuristicBalanced"
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

# Convert GA results to the same long format (final solution metrics)
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

# Merge all experimental results (Initial, Bubble, SA, GA, HeuristicBalanced) into one table
AllExpResults <- bind_rows(BasicexpResults_long, SAexpResults_long, GAexpResults_long, HeuristicexpResults_long)

# Ensure 'var_components' has no missing values (replace NAs by 0)
AllExpResults <- AllExpResults %>%
  mutate(var_components = replace_na(var_components, 0))

# Compute simple Pareto indicators within each (filename, Algorithm) slice:
# - MinVar: flag solutions with minimum variance for a given (n_components)
# - Pareto: non-dominated under (maximize n_components, minimize var_components)
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

# Jitter magnitude to alleviate point overlap in plots
jitter <- 0.05

# Generate and save Pareto-front plots per problem instance (only Pareto-flagged points)
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

# Persist the full joined results to CSV for downstream analysis
write_csv(AllExpResults, "results/AllExpResults.csv")



#########################################
#### Analysis of the three single-solution techniques ####
#########################################

# Keep only one-shot strategies (Initial, Bubble, HeuristicBalanced)
single_solutions <- AllExpResults %>%
  filter(Algorithm %in% c("Initial", "Bubble", "HeuristicBalanced")) %>%
  select(filename, groups, poisson_param, Algorithm, n_components, var_components)

# Boxplot: number of components by strategy
ggplot(single_solutions, aes(x = Algorithm, y = n_components, fill = Algorithm)) +
  geom_boxplot(alpha = 0.7) +
  theme_minimal() +
  labs(title = "Comparison of the Number of Connected Components",
       x = "Strategy",
       y = "Number of components") +
  theme(legend.position = "none")

# Boxplot: variance by strategy
ggplot(single_solutions, aes(x = Algorithm, y = var_components, fill = Algorithm)) +
  geom_boxplot(alpha = 0.7) +
  theme_minimal() +
  labs(title = "Comparison of Component Size Variance",
       x = "Strategy",
       y = "Variance") +
  theme(legend.position = "none")

# Convert to wide format for the Friedman test (number of components)
wide_data <- single_solutions %>%
  select(filename, Algorithm, n_components) %>%
  pivot_wider(names_from = Algorithm, values_from = n_components) %>%
  drop_na()

# Friedman test (non-parametric repeated-measures ANOVA analogue) on components
friedman.test(as.matrix(wide_data[, -1]))

# Prepare wide format for variance
wide_data_var <- single_solutions %>%
  select(filename, Algorithm, var_components) %>%
  pivot_wider(names_from = Algorithm, values_from = var_components) %>%
  drop_na()

# Friedman test on variance
friedman.test(as.matrix(wide_data_var[, -1]))

# Pairwise post-hoc Wilcoxon signed-rank tests (paired)
wilcox.test(wide_data$Initial, wide_data$Bubble, paired = TRUE)
wilcox.test(wide_data$Initial, wide_data$HeuristicBalanced, paired = TRUE)
wilcox.test(wide_data$Bubble, wide_data$HeuristicBalanced, paired = TRUE)

# Trend of number of components as a function of the Poisson parameter (smoothed)
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

# Subset to single-solution strategies and set plotting order/labels
single_solutions <- AllExpResults %>%
  filter(Algorithm %in% c("Initial", "Bubble", "HeuristicBalanced")) %>%
  mutate(Algorithm = factor(Algorithm, 
                            levels = c("Initial", "Bubble", "HeuristicBalanced"),
                            labels = c("Initial", "Bubble", "Heuristic")))

# LOESS-smoothed lines of components vs Poisson parameter, faceted by groups
p <- ggplot(single_solutions, aes(x = poisson_param, y = n_components, color = Algorithm)) +
  geom_smooth(se = FALSE, method = "loess", span = 0.25, linewidth = 1.1) +
  theme_minimal(base_size = 14) +
  facet_wrap(~ groups, labeller = label_both) +
  scale_color_brewer(palette = "Dark2") +
  labs(title = "Number of Components by Poisson Parameter and Number of Groups",
       x = "Poisson Parameter",
       y = "Number of Components",
       color = "Strategy")

# Save the plot to PDF and JPG
ggsave("analysis/1_components_poisson_groups.pdf", plot = p, width = 10, height = 6)
ggsave("analysis/1_components_poisson_groups.jpg", plot = p, width = 10, height = 6, dpi = 300)

#############################################################
#### Variance of Components vs. Poisson Parameter by Strategy and Groups
#############################################################

# Reuse same subset with human-friendly strategy label
single_solutions <- single_solutions %>%
  mutate(Algorithm = factor(Algorithm, 
                            levels = c("Initial", "Bubble", "Heuristic"),
                            labels = c("Initial", "Bubble", "Heuristic")))

# LOESS-smoothed lines of variance vs Poisson parameter, faceted by groups
p_var <- ggplot(single_solutions, aes(x = poisson_param, y = var_components, color = Algorithm)) +
  geom_smooth(se = FALSE, method = "loess", span = 0.25, linewidth = 1.1) +
  theme_minimal(base_size = 14) +
  facet_wrap(~ groups, labeller = label_both) +
  scale_color_brewer(palette = "Dark2") +
  labs(title = "Variance of Component Sizes by Poisson Parameter and Number of Groups",
       x = "Poisson Parameter",
       y = "Variance of Component Sizes",
       color = "Strategy")

# Save the plot to PDF and JPG
ggsave("analysis/2_variance_poisson_groups.pdf", plot = p_var, width = 10, height = 6)
ggsave("analysis/2_variance_poisson_groups.jpg", plot = p_var, width = 10, height = 6, dpi = 300)

########################################################
#### Boxplots: Number of Components and Variance by Strategy
########################################################

# Define a 3-color palette from RColorBrewer for consistency
palette_colors <- brewer.pal(3, "Dark2")

# Boxplot: number of components by strategy
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

# Boxplot: variance of component sizes by strategy
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

# Friedman test on number of components (wide matrix input)
wide_data <- single_solutions %>%
  select(filename, Algorithm, n_components) %>%
  pivot_wider(names_from = Algorithm, values_from = n_components) %>%
  drop_na()

test_components <- friedman.test(as.matrix(wide_data[, -1]))

# Persist test result to a text file
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

# Persist variance test result to a text file
sink("analysis/friedman_test_variance.txt")
cat("Friedman Test for Variance of Component Sizes:\n\n")
print(test_variance)
sink()

#########################################



# Function to compute the Pareto front (maximize n_components, minimize var_components)
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




# 1) Select relevant columns (keep all needed fields)
pareto_data <- AllExpResults %>%
  select(filename, Algorithm, groups, poisson_param, seed,
         n_components, var_components) %>%
  distinct()

# 2) Compute the Pareto front per problem instance (filename)
# Use all fields, not just n_components and var_components
pareto_global_detailed <- pareto_data %>%
  group_by(filename) %>%
  nest() %>%
  mutate(
    pareto_points = map(data, pareto_front)
  ) %>%
  select(filename, pareto_points) %>%
  unnest(pareto_points)  # returns all original fields for each Pareto point

# Aggregate algorithms that hit the same Pareto point in an instance
pareto_with_algorithms <- pareto_global_detailed %>%
  group_by(filename, n_components, var_components, groups, poisson_param, seed) %>%
  summarise(
    Algorithms = paste(sort(unique(Algorithm)), collapse = ", "),
    .groups = "drop"
  )

# Unique Pareto points per instance
pareto_unique_points <- pareto_global_detailed %>%
  distinct(filename, n_components, var_components) %>%
  group_by(filename) %>%
  summarise(total_pareto_points = n(), .groups = "drop")

# How many Pareto points each algorithm found per instance
pareto_points_by_algorithm <- pareto_global_detailed %>%
  distinct(filename, n_components, var_components, Algorithm) %>%
  group_by(filename, Algorithm) %>%
  summarise(points_found = n(), .groups = "drop")

# Coverage of the Pareto front per algorithm and instance (% computed later)
pareto_coverage <- pareto_points_by_algorithm %>%
  left_join(pareto_unique_points, by = "filename") %>%
  mutate(percent_covered = round(100 * points_found / total_pareto_points, 2))

# Wide-format table of coverage per instance
pareto_wide <- pareto_coverage %>%
  select(filename, Algorithm, points_found, total_pareto_points) %>%
  pivot_wider(
    names_from = Algorithm,
    values_from = points_found,
    values_fill = 0  # If an algorithm is absent, set count to 0
  )

# Parse filename into structured fields for convenient sorting/filtering
pareto_wide <- pareto_wide %>%
  mutate(filename_clean = str_remove(filename, "\\.csv$")) %>%
  separate(filename_clean, into = c("prefix", "groups", "students", "courses", "poisson_param", "seed"), sep = "_", remove = FALSE) %>%
  mutate(
    groups = as.integer(groups),
    poisson_param = as.numeric(poisson_param),
    seed = as.integer(seed)
  ) %>%
  select(-prefix, -students, -courses, -filename_clean)

# Reorder columns for readability
pareto_final <- pareto_wide %>%
  relocate(filename, total_pareto_points, Initial, Bubble, HeuristicBalanced, SA, GA, groups, poisson_param, seed)

# Harmonize algorithm naming (HeuristicBalanced -> Heuristic)
pareto_final <- pareto_final %>%
  rename(Heuristic = HeuristicBalanced)

# Save the wide-format coverage table
write_csv(pareto_final, "analysis/pareto_coverage_wide.csv")

# Average coverage statistics grouped by (groups, poisson_param)
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

# Round numeric columns for cleaner reporting
pareto_final_summary <- pareto_final_summary %>%
  mutate(across(where(is.numeric), ~ round(.x, 2)))

# Save grouped summary
write_csv(pareto_final_summary, "analysis/pareto_summary_by_group_poisson.csv")

# Long-format version for plotting multiple metrics together
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

# Ensure ordered factors for consistent legend/facets
pareto_long <- pareto_long %>%
  mutate(
    Metric = factor(Metric, levels = c("Total Pareto Points", "Initial", "Bubble", "Heuristic", "SA", "GA"))
  )

# Define colors (dedicated color for "Total", Brewer palette for algorithms)
colors <- c(
  "Total Pareto Points" = "black",
  "Initial" = RColorBrewer::brewer.pal(6, "Dark2")[1],
  "Bubble" = RColorBrewer::brewer.pal(6, "Dark2")[2],
  "Heuristic" = RColorBrewer::brewer.pal(6, "Dark2")[3],
  "SA" = RColorBrewer::brewer.pal(6, "Dark2")[4],
  "GA" = RColorBrewer::brewer.pal(6, "Dark2")[5]
)

# Line types for each metric (dashed for total count)
linetypes <- c(
  "Total Pareto Points" = "dashed",
  "Initial" = "solid",
  "Bubble" = "solid",
  "Heuristic" = "solid",
  "SA" = "solid",
  "GA" = "solid"
)

# Plot: average number of Pareto points by strategy and Poisson parameter (faceted by groups)
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

# Save the plot (PDF and JPG)
ggsave("analysis/7_Average number of Pareto points by strategy and Poisson parameter.pdf", plot = p7,
       width = 8, height = 5, units = "in")

ggsave("analysis/7_Average number of Pareto points by strategy and Poisson parameter.jpg", plot = p7,
       width = 8, height = 5, units = "in", dpi = 300)


##################################################################


# Convert average counts to percentages of the total Pareto points
pareto_percent_summary <- pareto_final_summary %>%
  mutate(
    pct_initial = 100 * avg_initial / avg_total_pareto_points,
    pct_bubble = 100 * avg_bubble / avg_total_pareto_points,
    pct_heuristic = 100 * avg_heuristic / avg_total_pareto_points,
    pct_sa = 100 * avg_sa / avg_total_pareto_points,
    pct_ga = 100 * avg_ga / avg_total_pareto_points
  )

# Long-format table of percentages for heatmap plotting
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

# Enforce a logical algorithm ordering for plotting
pareto_percent_long <- pareto_percent_long %>%
  mutate(
    Algorithm = factor(Algorithm, levels = c("Initial", "Bubble", "Heuristic", "SA", "GA"))
  )

# Heatmap: percentage of Pareto points found by each strategy, vs Poisson parameter and groups
p9 <- ggplot(pareto_percent_long, aes(x = poisson_param, y = Algorithm, fill = Percent)) +
  geom_tile(color = "white") +
  facet_grid(groups ~ ., labeller = label_both) +
  scale_fill_viridis(
    name = "% of Pareto points",
    limits = c(0, 100),
    option = "D",
    direction = -1  # invert color scale so higher values are visually prominent
  ) +
  theme_minimal(base_size = 14) +
  labs(
    title = "Percentage of Pareto points found by strategy",
    x = "Poisson parameter",
    y = "Strategy"
  )

print(p9)

# Save the heatmap (PDF and JPG)
ggsave("analysis/9_Pareto_points_percent_by_strategy_and_poisson_ordered.pdf", plot = p9,
       width = 8, height = 6, units = "in")

ggsave("analysis/9_Pareto_points_percent_by_strategy_and_poisson_ordered.jpg", plot = p9,
       width = 8, height = 6, units = "in", dpi = 300)
