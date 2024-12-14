library(dplyr)

# Data -------------------------------------------------------------------
data <- readRDS("local_lecture_dirigee/data/full_survey_data.rds")

set.seed(123)
for (i in 1:15){
  ix_train <- sample(1:nrow(data), size = 0.85 * nrow(data))
  ix_test <- setdiff(1:nrow(data), ix_train)
  df_train <- data[ix_train,]
  df_test <- data[ix_test,]
  if (i == 1){
    data_list <- list()
    data_list[[as.character(i)]][["train"]] <- df_train
    data_list[[as.character(i)]][["test"]] <- df_test
  } else {
    data_list[[as.character(i)]][["train"]] <- df_train
    data_list[[as.character(i)]][["test"]] <- df_test
  }
  message(i)
}

saveRDS(data_list, "local_lecture_dirigee/data/training_testing_data.rds")
