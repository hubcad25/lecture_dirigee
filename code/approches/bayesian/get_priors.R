# Packages ---------------------------------------------------------------
library(dplyr)
library(googlesheets4)

# Retrieve data from google sheets ---------------------------------------

clean_issue_names_to_variables <- c(
  "d'être en faveur d'un test de français obligatoire pour obtenir son diplôme au cégep" = "bloc3_iss_lang_englishCegep",
  "d'être en faveur que le Québec devienne un pays souverain" = "bloc3_iss_nationalisme_souv",
  "d'être en accord que la langue française est en danger" = "bloc3_iss_lang_afraidDisappear",
  "d'être en faveur que le Québec accueille MOINS d'immigrants" = "bloc3_iss_immig_immLess",
  "d'être en accord que son propre mode de vie a contribué aux problèmes environnementaux actuels" = "bloc3_iss_enviro_envLifestyle",
  "d'être en accord que les enseignants NE devraient PAS pouvoir porter des symboles religieux visibles au travail" = "bloc3_iss_laic_relSignsTeachersNo",
  "d'être en accord que les employés de l'État NE devraient PAS pouvoir porter des symboles religieux visibles au travail" = "bloc3_iss_laic_relSignsWorkNo",
  "d'être en accord que l'immigration est une menace à la culture de la province" = "bloc3_iss_immig_immThreat",
  "d'être en accord qu'il est nécessaire que les immigrants apprennent le français dans les mois suivants leur arrivée au Québec" = "bloc3_iss_immig_immLearnFr",
  "de se considérer Québécois avant Canadien" = "bloc3_iss_nationalisme_qcBefCan",
  "d'être en accord que le français devrait être la seule langue d'accueil permise dans les commerces au Québec" = "bloc3_iss_lang_businessFrench"
)


df_priors <- read_sheet("https://docs.google.com/spreadsheets/d/13wJZivc7lpYPaMX2A28AhYAI2QlwXT-pE2m-nOA1eTo/edit?resourcekey=&gid=796480965#gid=796480965") %>% 
  mutate(Horodateur = 1:nrow(.)) |> 
  rename(id = Horodateur) |> 
  tidyr::pivot_longer(
    cols = -id,
    names_to = "variable",
    values_to = "prior"
  ) |>
  mutate(
    party = stringr::str_extract(variable, "(?<=\\[).*?(?=\\])"),
    variable = gsub("Selon vous, en 2022, toutes choses étant égales par ailleurs, quel est l'effet sur la probabilité de voter pour les partis suivants ", "", variable),
    variable = gsub(" : \\[\\w+\\]", "", variable),
    variable = gsub(": \\[\\w+\\]", "", variable),
    variable = clean_issue_names_to_variables[variable],
    prior = case_when(
      prior == "Très négatif" ~ -1,
      prior == "Négatif"      ~ -0.5,
      prior == "Aucun effet"  ~ 0,
      prior == "Positif"      ~ 0.5,
      prior == "Très positif" ~ 1
    )
  ) |> 
  group_by(variable, party) |>
  summarise(
    mean = mean(prior),
    sd = sd(prior)
  )

saveRDS(df_priors, "local_lecture_dirigee/data/bayesian/priors.rds")

