# Packages ---------------------------------------------------------------
library(dplyr)
library(ggplot2)

# Data -------------------------------------------------------------------
data_list <- readRDS("local_lecture_dirigee/data/training_testing_data.rds")

# Loop to adjust models --------------------------------------------------------------

blocks <- 1

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
    model <- potgrowth::lm_with_residuals(
      vd ~ .,
      data = df_train
    )
    model$df_test <- df_test
    saveRDS(model, paste0("local_lecture_dirigee/data/models/frequentist_bloc", max(blocks), "_", j, "_iter", i, ".rds"))
    message("    ", j)
  }
}
