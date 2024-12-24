# Packages ---------------------------------------------------------------
library(dplyr)
library(ggplot2)
library(rstanarm)

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
    model <- stan_lm(
      vd ~ .,
      data = df_train,
      chains = 4,
      iter = 2000,
      seed = 123,
      prior = NULL,
      verbose = 0
    )
    model$df_test <- df_test
    saveRDS(model, paste0("local_lecture_dirigee/data/models/bayesian_bloc", max(blocks), "_", j, "_iter", i, ".rds"))
    message("    ", j)
  }
}
