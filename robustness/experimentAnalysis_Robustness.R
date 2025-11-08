# Load core data wrangling and plotting libraries
library(tidyverse)
library(RColorBrewer)
library(viridis)

# Load all CSV files under 'resultsRobustness' that match "experiment_results_*.csv"
# (These are the Simulated Annealing robustness runs.)
SAexpResults_robustness <- list.files(path = "resultsRobustness", 
                                      pattern = "experiment_results_.*\\.csv$", 
                                      full.names = TRUE) %>%
  map_df(read_csv)

# Quick preview of the SA robustness table
# NOTE: Printed object is 'SAexpResults' (not defined here) as in the original script.
print(SAexpResults)

# Load all Genetic Algorithm robustness results from 'resultsRobustness/GA'
GAexpResults_robustness <- list.files(path = "resultsRobustness/GA", 
                                      pattern = "experiment_results_GA_.*\\.csv$", 
                                      full.names = TRUE) %>%
  map_df(read_csv)

# Quick preview of the GA robustness table
# NOTE: Printed object is 'GAexpResults' (not defined here) as in the original script.
print(GAexpResults)

# Load aggregated baseline results for robustness analysis
BasicexpResults_robustness <- read_csv("resultsRobustness/results_summary_robustness.csv")

# Reshape baseline results to a long format and attach human-readable algorithm labels
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

# Preview of reshaped baseline results
print(BasicexpResults_long_robustness)

# Normalize SA robustness results into a long format with unified field names
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

# Quick preview of SA robustness (long format)
print(SAexpResults_robustness_long)

# Derive the leveled heuristic baseline from SA runs with w1 = 0 (structure-only objective)
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

# Parse GA robustness file names to extract the number of groups (e.g., "_2G_")
GAexpResults_robustness <- GAexpResults_robustness %>%
  mutate(grupos = str_extract(archivo, "_\\dG_") |>        # extracts patterns like "_2G_"
           str_remove_all("[^0-9]") |>                     # keep only digits
           as.integer())                                    # cast to integer

# Normalize GA robustness results into the same long format
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

# Combine all robustness results (Initial, Bubble, HeuristicBalanced, SA, GA)
AllExpResults_robustness <- bind_rows(BasicexpResults_long_robustness, SAexpResults_robustness_long, GAexpResults_robustness_long, HeuristicexpResults_robustness_long)

# Ensure 'var_components' has no missing values (replace NAs with 0)
AllExpResults_robustness <- AllExpResults_robustness %>%
  mutate(var_components = replace_na(var_components, 0))

# Compute Pareto-related flags:
# - MinVar: minimum variance for each (filename, Algorithm, n_components)
# - Pareto: non-dominated under (maximize n_components, minimize var_components)
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

# Persist the combined robustness results for downstream analysis
write_csv(AllExpResults_robustness, "resultsRobustness/AllExpResults_robustness.csv")



#########################################
#### Analysis of the three single-solution techniques ####
#########################################

# Keep only one-shot strategies (Initial, Bubble, HeuristicBalanced) for paired comparisons
single_solutions_robustness <- AllExpResults_robustness %>%
  filter(Algorithm %in% c("Initial", "Bubble", "HeuristicBalanced")) %>%
  select(filename,distribution, groups,students_per_group, mu_target,r,p, Algorithm, n_components, var_components)

# Boxplot: number of components by strategy
ggplot(single_solutions_robustness, aes(x = Algorithm, y = n_components, fill = Algorithm)) +
  geom_boxplot(alpha = 0.7) +
  theme_minimal() +
  labs(title = "Comparison of the Number of Connected Components",
       x = "Strategy",
       y = "Number of components") +
  theme(legend.position = "none")

# Boxplot: variance by strategy
ggplot(single_solutions_robustness, aes(x = Algorithm, y = var_components, fill = Algorithm)) +
  geom_boxplot(alpha = 0.7) +
  theme_minimal() +
  labs(title = "Comparison of Component Size Variance",
       x = "Strategy",
       y = "Variance") +
  theme(legend.position = "none")

# Reshape to wide format for Friedman test on number of components
wide_data <- single_solutions_robustness %>%
  select(filename, Algorithm, n_components) %>%
  pivot_wider(names_from = Algorithm, values_from = n_components) %>%
  drop_na()

# Friedman test (non-parametric repeated-measures ANOVA analogue): components
friedman.test(as.matrix(wide_data[, -1]))

# Prepare wide format for variance
wide_data_var <- single_solutions_robustness %>%
  select(filename, Algorithm, var_components) %>%
  pivot_wider(names_from = Algorithm, values_from = var_components) %>%
  drop_na()

# Friedman test: variance
friedman.test(as.matrix(wide_data_var[, -1]))

# Pairwise post-hoc Wilcoxon signed-rank tests (paired) between strategies
wilcox.test(wide_data$Initial, wide_data$Bubble, paired = TRUE)
wilcox.test(wide_data$Initial, wide_data$HeuristicBalanced, paired = TRUE)
wilcox.test(wide_data$Bubble, wide_data$HeuristicBalanced, paired = TRUE)


########################################################
#### Boxplots: Number of Components and Variance by Strategy
########################################################

# Fixed palette for consistent fills across plots
palette_colors <- brewer.pal(3, "Dark2")

# Boxplot: number of components by strategy (saved to robustness figures)
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

# Boxplot: variance of component sizes by strategy (saved to robustness figures)
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

# Friedman test on components (wide matrix input)
wide_data <- single_solutions_robustness %>%
  select(filename, Algorithm, n_components) %>%
  pivot_wider(names_from = Algorithm, values_from = n_components) %>%
  drop_na()

test_components <- friedman.test(as.matrix(wide_data[, -1]))

# Save the test-statistics for components
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

# Save the test-statistics for variance
sink("resultsRobustness/Figuras robustness/friedman_test_variance.txt")
cat("Friedman Test for Variance of Component Sizes:\n\n")
print(test_variance)
sink()


#########################################



# Function to compute the Pareto front
# Objective: maximize 'n_components' and minimize 'var_components'
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




# 1) Select relevant columns for Pareto analysis (keep all necessary factors for grouping)
pareto_data <- AllExpResults_robustness %>%
  select(filename, Algorithm, groups, students_per_group, mu_target,r,p,seed,
         n_components, var_components) %>%
  distinct()

# 2) Compute the Pareto front per problem instance (filename)
# Use all fields so we can later aggregate/join by any of them
pareto_global_detailed <- pareto_data %>%
  group_by(filename) %>%
  nest() %>%
  mutate(
    pareto_points = map(data, pareto_front)
  ) %>%
  select(filename, pareto_points) %>%
  unnest(pareto_points)  # returns each Pareto point with all original fields intact

# Aggregate: list which algorithms achieved each Pareto point within an instance
pareto_with_algorithms <- pareto_global_detailed %>%
  group_by(filename, n_components, var_components, groups, students_per_group, mu_target,r,p,seed) %>%
  summarise(
    Algorithms = paste(sort(unique(Algorithm)), collapse = ", "),
    .groups = "drop"
  )

# Count unique Pareto points per instance
pareto_unique_points <- pareto_global_detailed %>%
  distinct(filename, n_components, var_components) %>%
  group_by(filename) %>%
  summarise(total_pareto_points = n(), .groups = "drop")

# Count how many Pareto points each algorithm discovered per instance
pareto_points_by_algorithm <- pareto_global_detailed %>%
  distinct(filename, n_components, var_components, Algorithm) %>%
  group_by(filename, Algorithm) %>%
  summarise(points_found = n(), .groups = "drop")

# Compute coverage (% of Pareto points) per algorithm and instance
pareto_coverage <- pareto_points_by_algorithm %>%
  left_join(pareto_unique_points, by = "filename") %>%
  mutate(percent_covered = round(100 * points_found / total_pareto_points, 2))

# Wide table of coverage per instance (missing algorithms filled with 0 hits)
pareto_wide <- pareto_coverage %>%
  select(filename, Algorithm, points_found, total_pareto_points) %>%
  pivot_wider(
    names_from = Algorithm,
    values_from = points_found,
    values_fill = 0  # if an algorithm is absent, assume 0 points found
  )


##################
# Empirical / Geometric / Negative Binomial subsets
##################

# Split coverage by distribution label embedded in the filename
pareto_wide_EMP   <- pareto_wide %>% filter(str_detect(filename, regex("EMP",   ignore_case = TRUE)))
pareto_wide_GEOM  <- pareto_wide %>% filter(str_detect(filename, regex("GEOM",  ignore_case = TRUE)))
pareto_wide_NEGBIN<- pareto_wide %>% filter(str_detect(filename, regex("NEGBIN",ignore_case = TRUE)))

# Parse EMP filenames: ds_EMP_<groups>G_<students>S_<courses>C_fixed_<seed>
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

# Parse GEOM filenames: ds_GEOM_<groups>G_<students>S_<courses>C_mu<...>_p<...>_<seed>
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

# Parse NEGBIN filenames: ds_NEGBIN_<groups>G_<students>S_<courses>C_mu<...>_r<...>_p<...>_<seed>
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

# Reorder columns for readability and align algorithm naming
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

# Summaries: average Pareto coverage per distribution family and hyperparameters
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

# Convert averages to percentages of total Pareto points (EMP)
pareto_percent_summary_EMP <- pareto_final_summary_EMP %>%
  mutate(
    pct_initial   = 100 * avg_initial   / avg_total_pareto_points,
    pct_bubble    = 100 * avg_bubble    / avg_total_pareto_points,
    pct_heuristic = 100 * avg_heuristic / avg_total_pareto_points,
    pct_sa        = 100 * avg_sa        / avg_total_pareto_points,
    pct_ga        = 100 * avg_ga        / avg_total_pareto_points
  ) %>%
  mutate(across(starts_with("pct_"), ~ round(.x, 2)))

# Convert averages to percentages of total Pareto points (GEOM)
pareto_percent_summary_GEOM <- pareto_final_summary_GEOM %>%
  mutate(
    pct_initial   = 100 * avg_initial   / avg_total_pareto_points,
    pct_bubble    = 100 * avg_bubble    / avg_total_pareto_points,
    pct_heuristic = 100 * avg_heuristic / avg_total_pareto_points,
    pct_sa        = 100 * avg_sa        / avg_total_pareto_points,
    pct_ga        = 100 * avg_ga        / avg_total_pareto_points
  ) %>%
  mutate(across(starts_with("pct_"), ~ round(.x, 2)))

# Convert averages to percentages of total Pareto points (NEGBIN)
pareto_percent_summary_NEGBIN <- pareto_final_summary_NEGBIN %>%
  mutate(
    pct_initial   = 100 * avg_initial   / avg_total_pareto_points,
    pct_bubble    = 100 * avg_bubble    / avg_total_pareto_points,
    pct_heuristic = 100 * avg_heuristic / avg_total_pareto_points,
    pct_sa        = 100 * avg_sa        / avg_total_pareto_points,
    pct_ga        = 100 * avg_ga        / avg_total_pareto_points
  ) %>%
  mutate(across(starts_with("pct_"), ~ round(.x, 2)))

##### EMP Plots

# Ensure output directory exists
outdir <- "resultsRobustness/Figuras robustness"
dir.create(outdir, recursive = TRUE, showWarnings = FALSE)

# Long-format table for EMP heatmap
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

# Fix x-axis ticks to the observed 'students' values
students_breaks <- sort(unique(pareto_percent_long_EMP$students))

# Heatmap: % of Pareto points by strategy (EMP), faceted by number of groups
p_EMP <- ggplot(pareto_percent_long_EMP,
                aes(x = students, y = Algorithm, fill = Percent)) +
  geom_tile(color = "white") +
  facet_wrap(~ groups, ncol = 1, labeller = label_both) +   # one panel per 'groups'
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

# Save EMP heatmap
ggsave(file.path(outdir, "EMP_Pareto_points_percent_by_strategy_students_by_groups.pdf"),
       plot = p_EMP, width = 10, height = 5, units = "in")
ggsave(file.path(outdir, "EMP_Pareto_points_percent_by_strategy_students_by_groups.jpg"),
       plot = p_EMP, width = 10, height = 5, units = "in", dpi = 300)

# Re-ensure output directory (idempotent)
outdir <- "resultsRobustness/Figuras robustness"
dir.create(outdir, recursive = TRUE, showWarnings = FALSE)

# GEOM: build long-format with percentages (from averages) for plotting
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

# GEOM: one heatmap per 'students' value; panels by 'groups'
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
  
  # Save GEOM heatmap per-students setting
  base <- paste0("GEOM_Pareto_points_percent_by_strategy_p_by_groups_students", s)
  ggsave(file.path(outdir, paste0(base, ".pdf")), plot = p_geom_s, width = 7, height = 9, units = "in")
  ggsave(file.path(outdir, paste0(base, ".jpg")), plot = p_geom_s, width = 7, height = 9, units = "in", dpi = 300)
}

# Redundant imports are kept as in the original script
library(dplyr)
library(tidyr)
library(ggplot2)
library(viridis)

# Ensure output directory exists (safe to rerun)
outdir <- "resultsRobustness/Figuras robustness"
dir.create(outdir, recursive = TRUE, showWarnings = FALSE)

# NEGBIN: long-format with percentages (from averages) for plotting
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

# NEGBIN: one heatmap per (students, r) combination; panels by 'groups'
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
      facet_wrap(~ groups, ncol = 1, labeller = label_both) +  # stacked vertically
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
    
    # Save NEGBIN heatmap per-(students, r) setting
    base <- paste0("NEGBIN_Pareto_points_percent_by_strategy_mu_by_groups_students", s, "_r", rv)
    ggsave(file.path(outdir, paste0(base, ".pdf")), plot = p_negbin_sr, width = 7, height = 9, units = "in")
    ggsave(file.path(outdir, paste0(base, ".jpg")), plot = p_negbin_sr, width = 7, height = 9, units = "in", dpi = 300)
  }
}

