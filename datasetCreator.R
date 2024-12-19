library(tidyverse)
library(ggplot2)

# Input parameters.
num_groups <- 3          # Number of groups per course
students_per_group <- 20 # Number of students per group
num_courses <- 9         # Number of courses
lambda_siblings <- 0.5   # Lambda parameter for the Poisson distribution

# Function to generate a dataset of students, groups, and families.
generate_dataset <- function(num_groups, students_per_group, num_courses, lambda_siblings) {
  
  # Calculate the total number of students.
  total_students <- num_groups * students_per_group * num_courses
  
  # Step 1: Generate family sizes
  family_sizes <- c()
  while (sum(family_sizes) < total_students) {
    # Generate a family size using a Poisson distribution and add 1 (to include the student).
    family_size <- rpois(1, lambda_siblings) + 1
    family_sizes <- c(family_sizes, family_size)
  }
  
  # Adjust the size of the last family if necessary
  difference <- sum(family_sizes) - total_students
  if (difference > 0) {
    family_sizes[length(family_sizes)] <- family_sizes[length(family_sizes)] - difference
    # Remove the family if its size becomes zero
    if (family_sizes[length(family_sizes)] == 0) {
      family_sizes <- family_sizes[-length(family_sizes)]
    }
  }
  
  # Assign IDs to students
  student_ids <- 1:total_students
  
  # Assign families to students
  families <- rep(1:length(family_sizes), times = family_sizes)
  students_family <- data.frame(student_id = student_ids, family_id = families)
  
  # Step 2: Randomly assign students to courses and groups
  # Create a list of all available courses and groups
  total_groups <- num_courses * num_groups
  groups <- expand.grid(course = 1:num_courses, group = 1:num_groups)
  
  # Repeat the data frame of groups to cover all students
  repeated_groups <- groups[rep(1:nrow(groups), each = students_per_group), ]
  
  # Ensure there are enough groups for all students
  if (nrow(repeated_groups) < total_students) {
    stop("Not enough groups to assign all students.")
  }
  
  # Select only the necessary groups
  assigned_groups <- repeated_groups[1:total_students, ]
  
  # Randomly shuffle students
  shuffled_students <- sample(student_ids)
  
  # Assign course and group to shuffled students
  data <- data.frame(
    student_id = shuffled_students,
    course = assigned_groups$course,
    group = assigned_groups$group
  )
  
  # Add family_id
  data <- merge(data, students_family, by = "student_id")
  
  # Sort by student_id
  data <- data[order(data$student_id), ]
  
  # Step 3: Add a column for siblings
  data <- data %>%
    group_by(family_id) %>%
    mutate(sibling_ids = map(student_id, ~ setdiff(student_id, .x))) %>%
    ungroup()
  
  # Convert the list of siblings into a text string
  data$sibling_ids <- sapply(data$sibling_ids, function(s) {
    if (length(s) == 0) {
      return(NA)
    } else {
      paste(sort(s), collapse = ",")
    }
  })
  
  # Add a column for the number of siblings
  data$num_siblings <- sapply(data$sibling_ids, function(s) {
    if (is.na(s) || s == "") {
      return(0)
    } else {
      length(strsplit(s, ",")[[1]])
    }
  })
  
  # Rearrange columns
  data <- data %>% select(student_id, course, group, family_id, sibling_ids, num_siblings)
  
  return(data)
}

# Variable parameters
num_groups_list <- 2:4                # Number of groups ranging between 2 and 4
lambda_siblings_list <- seq(0.1, 2.0, by = 0.1) # Lambda values for the Poisson distribution
seed_list <- c(6666, 7777, 14141414)  # Different seeds for each dataset

# Create the 'datasets' folder if it doesn't exist
if (!dir.exists("datasets")) {
  dir.create("datasets")
}

# Iterate over the parameter combinations
for (num_groups in num_groups_list) {
  for (lambda_siblings in lambda_siblings_list) {
    for (seed in seed_list) {
      # Set the seed
      set.seed(seed)
      
      # Generate the dataset
      dataset <- generate_dataset(num_groups, students_per_group, num_courses, lambda_siblings)
      
      # Create the filename including all parameters
      file_name <- paste0(
        "ds_",
        num_groups, "_",
        students_per_group, "_",
        num_courses, "_",
        format(lambda_siblings, nsmall = 1), "_",
        seed, ".csv"
      )
      
      # Save the dataset in the 'datasets' folder
      write.csv(dataset, file = file.path("datasets", file_name), row.names = FALSE)
    }
  }
}




##############################

# Valores de lambda para la distribución Poisson
lambda_values <- c(0.1, 0.25, 0.5, 0.75, 1, 1.25, 1.5, 1.75, 2.0)

# Número de simulaciones por cada lambda
n_simulations <- 10000

# Crear un data frame para almacenar los resultados
distribution_data <- data.frame(
  family_size = integer(),
  lambda = numeric()
)

# Generar tamaños de familia para cada lambda
for (lambda in lambda_values) {
  family_sizes <- rpois(n_simulations, lambda) + 1
  temp_data <- data.frame(
    family_size = family_sizes,
    lambda = as.factor(lambda)
  )
  distribution_data <- rbind(distribution_data, temp_data)
}

# Calcular frecuencias relativas para representar PMF
pmf_data <- distribution_data %>%
  group_by(lambda, family_size) %>%
  summarise(frequency = n() / n_simulations, .groups = 'drop')

# Graficar las distribuciones como líneas y puntos
plot <- ggplot(pmf_data, aes(x = family_size, y = frequency, color = lambda, group = lambda)) +
  geom_line(size = 1) +
  geom_point(size = 2) +
  scale_x_continuous(breaks = seq(0, max(pmf_data$family_size), by = 1), expand = c(0, 0)) +
  labs(
    title = "Family Size Distribution for Different Lambda Values",
    x = "P(X = k | \u03bb) + 1",
    y = "Relative Frequency",
    color = "Lambda"
  ) +
  theme_minimal()

# Guardar el gráfico en PDF y JPG
ggsave("results/family_size_distribution.pdf", plot, width = 8, height = 6)
ggsave("results/family_size_distribution.jpg", plot, width = 8, height = 6)

# Mostrar el gráfico
print(plot)

