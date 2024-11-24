library(dplyr)
library(tidyr)
library(purrr)
library(ggplot2)

data <- readRDS("local_lecture_dirigee/data/survey_data_holes.rds")

# Functions --------------------------------------------------------------
# Fonction pour calculer le pourcentage de lignes complètes entre deux variables
compute_completeness_index <- function(var1, var2, data) {
  complete_cases <- data %>% filter(!is.na(!!sym(var1)) & !is.na(!!sym(var2)))
  return(nrow(complete_cases) / nrow(data))
}

# Créer une matrice de complétude entre toutes les paires de variables
compute_completeness_matrix <- function(data) {
  var_names <- colnames(data)
  completeness_matrix <- matrix(0, ncol = length(var_names), nrow = length(var_names))
  colnames(completeness_matrix) <- var_names
  rownames(completeness_matrix) <- var_names
  
  # Calculer l'indice de complétude pour chaque paire
  for (i in 1:(length(var_names) - 1)) {
    for (j in (i + 1):length(var_names)) {
      completeness_matrix[i, j] <- compute_completeness_index(var_names[i], var_names[j], data)
      completeness_matrix[j, i] <- completeness_matrix[i, j] # Matrice symétrique
    }
    message(i, " / ", (length(var_names) - 1))
  }
  
  return(completeness_matrix)
}

# Appliquer un clustering hiérarchique et diviser en clusters
cluster_completeness_matrix <- function(completeness_matrix, h = 0.95) {
  dist_matrix <- as.dist(1 - completeness_matrix)  # Distance basée sur l'inverse de la similarité
  hc <- hclust(dist_matrix)
  clusters <- cutree(hc, h = h)  # Ajustez 'h' pour contrôler le nombre de clusters
  results <- lapply(split(names(clusters), clusters), function(vars) {
    data_selected <- data %>% select(all_of(vars))
    complete_cases <- sum(complete.cases(data_selected))
    list(vars = vars, complete_cases = complete_cases)
  })
  return(results)
}

get_clusters <- function(data, h = 0.95) {
  completeness_matrix <- compute_completeness_matrix(data)
  return(cluster_completeness_matrix(completeness_matrix, h))
}

results <- get_clusters(data, h = 0.95)

# Apply function ---------------------------------------------------------

necessary_variables <- c(
  "id", "source_id", "riding_id", "age",
  "age_cat", "educ", "income", "male",
  "lang", "religion",
  "irc_CAQ", "irc_PLQ", "irc_PQ", "irc_QS", "irc_PCQ"
)

data_to_cluster <- data |> 
  tidyr::drop_na(all_of(necessary_variables)) |> 
  select(
    -all_of(necessary_variables),
    -ends_with("model"),
      -starts_with("probVote"),
      -starts_with("reception")
  )

results <- get_clusters(data_to_cluster, h = 0.999)

# Check by source id -----------------------------------------------------

data_by_source_id <- data |>
  tidyr::drop_na(all_of(necessary_variables)) |> 
  select(
    -all_of(necessary_variables),
    source_id,
    -ends_with("model"),
    -starts_with("probVote"),
    -voteInt
  )

table(data_by_source_id$source_id)

df_na_by_variable_by_source_id <- data_by_source_id |> 
  group_by(source_id) |> 
  summarise(across(everything(), ~ sum(is.na(.)) / n())) |> 
  tidyr::pivot_longer(
    cols = -source_id,
    names_to = "variable",
    values_to = "proportion_na"
  ) |> 
  mutate(
    available_data = 1 - proportion_na
  )

ggplot(
  df_na_by_variable_by_source_id,
  aes(
    x = available_data, y = variable,
    fill = available_data, color = available_data)
  ) +
  lemon::facet_rep_wrap(~ source_id, nrow = 1, repeat.tick.labels = "y") +
  geom_col(color = NA) +
  geom_point() +
  scale_color_gradient(low = "red", high = "green") +
  scale_fill_gradient(low = "red", high = "green") +
  clessnize::theme_clean_light()

ggsave(
  "local_lecture_dirigee/graphs/missing_data.png",
  width = 21, height = 10
)
