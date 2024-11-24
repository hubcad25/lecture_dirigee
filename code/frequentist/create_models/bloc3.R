# Packages ---------------------------------------------------------------
library(dplyr)
library(ggplot2)

# Data -------------------------------------------------------------------
data <- readRDS("local_lecture_dirigee/data/full_survey_data.rds") |> 
  select(starts_with(c("vd", "bloc1", "bloc2", "bloc3")))

sondr::glimpse_with_table(data)

# Functions --------------------------------------------------------------

for (i in potgrowth::qc_parties){
  data$vd <- data[[paste0("vd_", i)]]
  model <- potgrowth::lm_with_residuals(
    vd ~ bloc1_age + bloc1_lang +
      bloc1_male + bloc1_income +
      bloc1_educ + bloc1_religion +
      bloc1_political_knowledge +
      bloc2_party_id +
      bloc3_iss_nationalisme_souv +
      bloc3_iss_nationalisme_qcBefCan +
      bloc3_iss_lang_englishCegep +
      bloc3_iss_lang_businessFrench +
      bloc3_iss_lang_afraidDisappear +
      bloc3_iss_laic_relSignsWorkNo +
      bloc3_iss_laic_relSignsTeachersNo +
      bloc3_iss_immig_immThreat +
      bloc3_iss_immig_immLess +
      bloc3_iss_immig_immLearnFr +
      bloc3_iss_enviro_envLifestyle,
    data = data
  )
  saveRDS(model, paste0("local_lecture_dirigee/data/models/frequentist/bloc3_", i, ".rds"))
  saveRDS(model, paste0("code/frequentist/present_models/models/bloc3_", i, ".rds"))
  message(i)
}

