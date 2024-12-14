# Packages ---------------------------------------------------------------
library(dplyr)
library(ggplot2)
library(randomForest)

# Data -------------------------------------------------------------------
data_list <- readRDS("local_lecture_dirigee/data/training_testing_data.rds")

# Loop to train models --------------------------------------------------------------

blocks <- 1:3

for (i in 1:length(data_list)){
  list <- data_list[[i]]
  message(i)
  for (j in potgrowth::qc_parties){
    df_train <- list$train
    df_test <- list$test
    df_train$vd <- df_train[[paste0("vd_", j)]]
    df_train <- df_train |>
      select(vd, starts_with(paste0("bloc", blocks)))
    df_test$vd <- df_test[[paste0("vd_", j)]]
    df_test <- df_test |>
      select(vd, starts_with(paste0("bloc", blocks)))
    model <- tuneRF(
      x = df_train[ , -which(names(df_train) == "vd")],
      y = df_train$vd,
      ntreeTry = 1000, # Nombre d'arbres à essayer
      mtryStart = 2,
      stepFactor = 1.5, # Facteur d'ajustement des mtry
      improve = 0.01, # Seuil d'amélioration minimal
      trace = FALSE, # Affiche les résultats à chaque étape
      plot = FALSE, # Génère un graphique des performances
      doBest = TRUE # Retourne le modèle
    )
    model$df_test <- df_test
    saveRDS(model, paste0("local_lecture_dirigee/data/models/randomforest_bloc", max(blocks), "_", j, "_iter", i, ".rds"))
    message("    ", j)
  }
}
