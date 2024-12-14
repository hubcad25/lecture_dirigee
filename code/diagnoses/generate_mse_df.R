# Packages ---------------------------------------------------------------
library(dplyr)
library(ggplot2)
library(randomForest)

# Load models ------------------------------------------------------------
models_files <- list.files(
  path = "local_lecture_dirigee/data/models",
  full.names = TRUE
  )

# Load models and calculate accuracy -------------------------------------
for (i in models_files){
  model <- readRDS(i)
  predictions <- predict(model, newdata = model$df_test)
  actual_values <- model$df_test$vd
  errors <- actual_values - predictions
  mse <- mean(errors^2)
  model_params <- strsplit(gsub(".rds", "", basename(i)), split = "_")[[1]]
  if (i == models_files[1]){
    df_mse <- data.frame(
      approche = model_params[1],
      bloc = gsub("bloc", "", model_params[2]),
      party = model_params[3],
      iter = gsub("iter", "", model_params[4]),
      mse = mse
    )
  } else {
    df_mse <- rbind(
      df_mse,
      data.frame(
      approche = model_params[1],
      bloc = gsub("bloc", "", model_params[2]),
      party = model_params[3],
      iter = gsub("iter", "", model_params[4]),
      mse = mse
      )
    )
  }
  message(which(models_files == i), " --- ", paste0(model_params, collapse = " - "))
}

saveRDS(df_mse, "local_lecture_dirigee/data/diagnoses/mse.rds")
