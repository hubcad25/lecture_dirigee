# Packages ---------------------------------------------------------------
library(dplyr)
library(ggplot2)

# Data -------------------------------------------------------------------
data_list <- readRDS("local_lecture_dirigee/data/training_testing_data.rds")

# Load priors ------------------------------------------------------------
df_priors <- readRDS("local_lecture_dirigee/data/bayesian/priors.rds")

# Loop to adjust models --------------------------------------------------------------

blocks <- 1:3

for (i in 1:length(data_list)){
  list <- data_list[[i]]
  message(i)
  for (j in potgrowth::qc_parties){
    df_train <- list$train
    df_test <- list$test

    # Filtrer les priors pour le parti actuel
    priors_for_party <- df_priors %>%
      filter(party == j) %>% # Filtrer pour le parti actuel
      group_by(variable) %>%
      summarise(mean = mean(mean), sd = mean(sd))

    df_train$vd <- df_train[[paste0("vd_", j)]]
    df_train <- df_train |>
      select(vd, starts_with(paste0("bloc", blocks)))
    df_test$vd <- df_test[[paste0("vd_", j)]]
    df_test <- df_test |>
      select(vd, starts_with(paste0("bloc", blocks)))

    df_priors_for_model <- priors_for_party %>%
      mutate(prior = paste0("normal(", mean, ", ", sd, ")"))
    priors <- brms::set_prior(df_priors_for_model$prior, coef = df_priors_for_model$variable)

    variable_names <- colnames(df_train)[colnames(df_train) != "vd"]

    # Formuler un modèle
    formula <- as.formula(paste("vd ~", paste(variable_names, collapse = " + ")))

    # Construire le modèle
    model <- brms::brm(
      formula = formula,
      data = df_train,
      prior = priors,
      family = gaussian(),
      chains = 4,
      iter = 2000,
      seed = 123
    )
    model$df_test <- df_test
    saveRDS(model, paste0("local_lecture_dirigee/data/models/bayesian_bloc", max(blocks), "_", j, "_iter", i, ".rds"))
    message("    ", j)
  }
}
