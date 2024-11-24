## Remove missing data and and superfluous variables
## Name variables according to their bloc

# Packages ---------------------------------------------------------------
library(dplyr)

# Data -------------------------------------------------------------------

df <- readRDS("local_lecture_dirigee/data/survey_data_holes.rds") |> 
  filter(
    source_id == "pilote2"
  ) |> 
  select(
  id, source_id,
  vd_CAQ = irc_CAQ,
  vd_PLQ = irc_PLQ, 
  vd_PQ = irc_PQ,
  vd_QS = irc_QS, 
  vd_PCQ = irc_PCQ,
  bloc1_age = age, bloc1_age_cat = age_cat, 
  bloc1_educ = educ, bloc1_income = income, 
  bloc1_male = male, bloc1_lang = lang, 
  bloc1_religion = religion,
  bloc1_political_knowledge = political_knowledge,
  bloc2_party_id = partyId,
  bloc3_scale_leftRight = scale_leftRight,
  bloc3_iss_newleft_wokeArtists = iss_newleft_wokeArtists,
  bloc3_iss_nationalisme_souv = iss_nationalisme_souv,
  bloc3_iss_nationalisme_qcBefCan = iss_nationalisme_qcBefCan,
  bloc3_iss_lang_englishCegep = iss_lang_englishCegep,
  bloc3_iss_lang_businessFrench = iss_lang_businessFrench,
  bloc3_iss_lang_afraidDisappear = iss_lang_afraidDisappear,
  bloc3_iss_laic_relSignsWorkNo = iss_laic_relSignsWorkNo,
  bloc3_iss_laic_relSignsTeachersNo = iss_laic_relSignsTeachersNo,
  bloc3_iss_immig_immThreat = iss_immig_immThreat,
  bloc3_iss_immig_immLess = iss_immig_immLess,
  bloc3_iss_immig_immLearnFr = iss_immig_immLearnFr,
  bloc3_iss_enviro_envLifestyle = iss_enviro_envLifestyle
) |> 
  tidyr::drop_na()

df_to_csv_codebook <- data.frame(
  variable = names(df)
)

write.csv(df_to_csv_codebook, "local_lecture_dirigee/data/codebook.csv", row.names = FALSE)

# Change factor levels ---------------------------------------------------

## age_cat
levels(df$bloc1_age_cat)
table(df$bloc1_age_cat)
df$bloc1_age_cat <- relevel(df$bloc1_age_cat, ref = "age60m74")
levels(df$bloc1_age_cat)


## bloc1_educ
levels(df$bloc1_educ)
table(df$bloc1_educ)
df$bloc1_educ <- relevel(df$bloc1_educ, ref = "educUniv")
levels(df$bloc1_educ)

## bloc1_income
levels(df$bloc1_income)
table(df$bloc1_income)
df$bloc1_income <- relevel(df$bloc1_income, ref = "incomeMid")
levels(df$bloc1_income)


## lang
levels(df$bloc1_lang)
table(df$bloc1_lang)
df$bloc1_lang <- relevel(df$bloc1_lang, ref = "french")
levels(df$bloc1_lang)

saveRDS(df, "local_lecture_dirigee/data/full_survey_data.rds")
