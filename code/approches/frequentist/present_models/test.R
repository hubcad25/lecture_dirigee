# Packages ---------------------------------------------------------------
library(dplyr)
library(ggplot2)
library(randomForest)
library(iml)

# Data -------------------------------------------------------------------
data <- readRDS("local_lecture_dirigee/data/full_survey_data.rds") |> 
  mutate(irc = vd_PCQ) |> 
  dplyr::select(starts_with("bloc1"), irc, -bloc1_age_cat)

model <- randomForest(
  irc ~ .,
  data = data
)

randomForest::varImpPlot(model)

predictor <- Predictor$new(
  model = model, 
  data = data %>% select(-irc),  # Données sans la variable cible
  y = data$irc                  # Variable cible
)

shap <- Shapley$new(predictor)

plot(shap)



partialPlot(model, data, bloc1_educ, "educUniv")
partialPlot(model, data, )

hist(predict(model))


data |> 
  tidyr::pivot_longer(
    cols = starts_with("vd")
  ) |>
  group_by(name, bloc3_iss_immig_immLess) |> 
  summarise(mean = mean(value)) |>
  ggplot(aes(x = bloc3_iss_immig_immLess, y = mean)) +
  geom_col() +
  facet_wrap(~name)
