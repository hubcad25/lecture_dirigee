library(dplyr)
library(tidyr)
library(GA)
library(ggplot2)

data <- readRDS("local_lecture_dirigee/data/survey_data_holes.rds")

names(data)

sondr::glimpse_with_table(data)

optimize_complete_cases <- function(data, s = 10, iterations = 100, min_vars = 5, min_obs = 100) {
  library(tidyr)
  
  # Étape 1: Trier les variables par proportion croissante de NA
  na_weights <- colMeans(is.na(data))
  sorted_vars <- names(sort(na_weights))  # Variables avec moins de NA en premier
  
  # Initialiser la meilleure combinaison et son nombre de cas complets
  best_vars <- NULL
  max_complete_cases <- 0
  
  # Étape 2-4: Itérations aléatoires pour trouver une solution initiale
  for (i in 1:iterations) {
    # Sélection aléatoire d'au moins min_vars variables
    random_vars <- sample(sorted_vars, size = sample(min_vars:length(sorted_vars), 1))
    complete_cases <- nrow(data %>% drop_na(all_of(random_vars)))
    
    # Mise à jour si une meilleure combinaison est trouvée et respecte min_obs
    if (complete_cases >= min_obs && complete_cases > max_complete_cases) {
      best_vars <- random_vars
      max_complete_cases <- complete_cases
    }
  }
  
  # Étape 5-6: Amélioration incrémentale en ajoutant des variables restantes
  remaining_vars <- setdiff(sorted_vars, best_vars)
  for (var in remaining_vars) {
    candidate_vars <- c(best_vars, var)
    new_complete_cases <- nrow(data %>% drop_na(all_of(candidate_vars)))
    
    # Ajouter la variable si elle réduit les observations de moins que le seuil s et respecte min_obs
    if ((max_complete_cases - new_complete_cases) <= s && new_complete_cases >= min_obs) {
      best_vars <- candidate_vars
      max_complete_cases <- new_complete_cases
    }
  }
  
  # Vérifier si le nombre minimal de variables est respecté
  if (length(best_vars) < min_vars) {
    additional_vars <- setdiff(sorted_vars, best_vars)
    best_vars <- c(best_vars, head(additional_vars, min_vars - length(best_vars)))
  }
  
  # Résultat final
  list(
    selected_variables = best_vars,
    complete_cases = nrow(data %>% drop_na(all_of(best_vars)))
  )
}

test <- optimize_complete_cases(
  data |>
    select(
      -ends_with("model"),
      -starts_with("probVote"),
      -starts_with("reception"),
      -c(
        "iss_liberty_mesuresSanitairesDictature",
        "iss_liberty_qcMissLiberty",
        "iss_nationalisme_qcRightDirection"
      )
    ),
  s = 50,
  iterations = 10000,
  min_vars = 28,
  min_obs = 1000
)

variables <- test$selected_variables

df_test <- data |> 
  select(all_of(c(necessary_variables, sort(variables)))) |> 
  tidyr::drop_na()

names(df_test)

visualize_combinations <- function(data, variables) {
  # Nombre total de lignes dans le jeu de données
  total_rows <- nrow(data)
  
  # Étape 1 : Initialiser un tableau pour stocker les résultats
  results <- expand.grid(var1 = variables, var2 = variables, stringsAsFactors = FALSE)
  
  # Étape 2 : Calculer le pourcentage de complete cases pour chaque paire
  results$complete_cases_percent <- apply(results, 1, function(row) {
    vars <- unique(c(row["var1"], row["var2"])) # Éviter les doublons dans les paires
    complete_cases <- nrow(data %>% drop_na(all_of(vars)))
    (complete_cases / total_rows) * 100
  })
  
  # Étape 3 : Visualisation sous forme de heatmap
  ggplot(results, aes(x = var1, y = var2, fill = complete_cases_percent)) +
    geom_tile(color = "white") +
    scale_fill_gradient(low = "white", high = "blue", labels = scales::percent_format(scale = 1)) +
    labs(
      title = "Heatmap des complete cases par paires de variables (%)",
      x = "Variable 1",
      y = "Variable 2",
      fill = "% Complete Cases"
    ) +
    theme_minimal() +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1),
      panel.grid = element_blank()
    )
}

# Liste des variables à analyser
variables <- c(
  "iss_nationalisme_qcBefCan", "iss_lang_worriedFrProv", "iss_laic_relSignsTeachersNo",
  "iss_immig_immAdapt", "iss_immig_immLess", "iss_newleft_wokeSysRaci",
  "iss_liberty_covidRevealAutoritharian", "iss_lien3_accord", 
  "iss_enviro_envGvtMore", "mi_issue_nationalisme"
)

# Appeler la fonction
visualize_combinations(data, variables)
