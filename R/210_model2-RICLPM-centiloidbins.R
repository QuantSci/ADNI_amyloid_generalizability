rm(list = setdiff(ls(), lsf.str())[!(setdiff(ls(), lsf.str()) %in% 'params')])
source(here::here('R', '002_folder-paths-and-options.R'))
source(here::here('R', '005_libraries.R'))

# Import data

pet_data <- readRDS(here::here("R_objects", "020_amy_pet_04.RDS"))
survey <- readRDS(here::here("R_objects", "042_survey_data.RDS"))
uw_cog_data <- readRDS(here::here("R_objects", "020_uw_cog_data.RDS"))
adas_cog_data <- readRDS(here::here("R_objects", "020_cog_data_03.RDS"))

# Make survey data

adni_data <- survey %>%
  dplyr::filter(DATA == "ADNI") %>%
  rename(RID = ID) %>%
  mutate(RID = as.numeric(RID))

merge1 <- left_join(pet_data, adni_data, by = "RID")
merge2 <- left_join(merge1, adas_cog_data, by = "RID")

unweighteddata <- left_join(merge2, uw_cog_data, by = "RID") %>%
  select(RID, scaled_weights,
         bl_wholecereb,
         bl_composite, m24_composite,
         bl_adni_mem, m24_adni_mem,
         bl_adni_ef, m24_adni_ef,
         bl_adas13, m24_adas13,
         AGE, MMSE, APOE41Y0N, DX) %>%
  mutate(scaled_weights = as.vector(scaled_weights))

# svy_subset_engage <- subset(svy_design, bl_wholecereb > 25)
# svy_subset_donanemab <- subset(svy_design, bl_wholecereb > 36)
# svy_subset_shouldbe <- subset(svy_design, between(bl_wholecereb, 15, 50))

# Create CLPM code for different cognition outcomes

clpm_model_adnimem <- 'm24_adni_mem ~ bl_adni_mem +  bl_composite
                       m24_composite ~ 1.35*bl_adni_mem + bl_composite
                       m24_adni_mem ~ m24_composite
                       bl_adni_mem ~ bl_composite'

clpm_model_adnief <- 'm24_adni_ef ~ bl_adni_ef + bl_composite
                      m24_composite ~ .053*bl_adni_ef + bl_composite
                      m24_adni_ef ~ m24_composite
                      bl_adni_ef ~ bl_composite'

clpm_model_adas13 <- 'm24_adas13 ~ bl_adas13 + bl_composite
                      m24_composite ~ -.197*bl_adas13 + bl_composite
                      m24_adas13 ~ m24_composite
                      bl_adas13 ~ bl_composite'

### Remember each has one n.s. pathway fixed to unstandardized estimate for model identification

# Run CLPMs -- overall sample

clpm_adnimem <- lavaan::sem(model = clpm_model_adnimem,
                            data = unweighteddata,
                            missing = "ml",
                            estimator = "MLR",
                            sampling.weights = "scaled_weights",
                            fixed.x = FALSE)

summary(clpm_adnimem, fit.measures = T, standardized = T)

clpm_adnief <- lavaan::sem(model = clpm_model_adnief,
                           data = unweighteddata,
                           missing = "ml",
                           estimator = "MLR",
                           sampling.weights = "scaled_weights",
                           fixed.x = FALSE)

summary(clpm_adnief, fit.measures = T, standardized = T)

clpm_adas13 <- lavaan::sem(model = clpm_model_adas13,
                           data = unweighteddata,
                           missing = "ml",
                           estimator = "MLR",
                           sampling.weights = "scaled_weights",
                           fixed.x = FALSE)

summary(clpm_adas13, fit.measures = T, standardized = T)

# ENGAGE CRITERIA

engage_subset <- unweighteddata %>%
  filter(bl_wholecereb > 25)

engage_clpm_adnimem <- lavaan::sem(model = clpm_model_adnimem,
                                   data = engage_subset,
                                   missing = "ml",
                                   estimator = "MLR",
                                   sampling.weights = "scaled_weights",
                                   fixed.x = FALSE)

summary(engage_clpm_adnimem, fit.measures = T, standardized = T)

engage_adnimem_results <- lavaan::standardizedSolution(engage_clpm_adnimem)

engage_clpm_adnief <- lavaan.survey::lavaan.survey(clpm_adnief, survey.design = svy_subset_engage)

summary(engage_clpm_adnief, fit.measures = T, standardized = T)

engage_adnief_results <- lavaan::standardizedSolution(engage_clpm_adnief)

engage_clpm_adas13 <- lavaan.survey::lavaan.survey(clpm_adas13, survey.design = svy_subset_engage)

summary(engage_clpm_adas13, fit.measures = T, standardized = T)

engage_adas13_results <- lavaan::standardizedSolution(engage_clpm_adas13)

# donanemab CRITERIA

donanemab_clpm_adnimem <- lavaan.survey::lavaan.survey(clpm_adnimem, survey.design = svy_subset_donanemab)

summary(donanemab_clpm_adnimem, fit.measures = T, standardized = T)

donanemab_adnimem_results <- lavaan::standardizedSolution(donanemab_clpm_adnimem)

donanemab_clpm_adnief <- lavaan.survey::lavaan.survey(clpm_adnief, survey.design = svy_subset_donanemab)

summary(donanemab_clpm_adnief, fit.measures = T, standardized = T)

donanemab_adnief_results <- lavaan::standardizedSolution(donanemab_clpm_adnief)

donanemab_clpm_adas13 <- lavaan.survey::lavaan.survey(clpm_adas13, survey.design = svy_subset_donanemab)

summary(donanemab_clpm_adas13, fit.measures = T, standardized = T)

donanemab_adas13_results <- lavaan::standardizedSolution(donanemab_clpm_adas13)

# shouldbe CRITERIA

shouldbe_clpm_adnimem <- lavaan.survey::lavaan.survey(clpm_adnimem, survey.design = svy_subset_shouldbe)

summary(shouldbe_clpm_adnimem, fit.measures = T, standardized = T)

shouldbe_adnimem_results <- lavaan::standardizedSolution(shouldbe_clpm_adnimem)

shouldbe_clpm_adnief <- lavaan.survey::lavaan.survey(clpm_adnief, survey.design = svy_subset_shouldbe)

summary(shouldbe_clpm_adnief, fit.measures = T, standardized = T)

shouldbe_adnief_results <- lavaan::standardizedSolution(shouldbe_clpm_adnief)

shouldbe_clpm_adas13 <- lavaan.survey::lavaan.survey(clpm_adas13, survey.design = svy_subset_shouldbe)

summary(shouldbe_clpm_adas13, fit.measures = T, standardized = T)

shouldbe_adas13_results <- lavaan::standardizedSolution(shouldbe_clpm_adas13)

## Save out results

saveRDS(engage_adnimem_results, here::here("R_objects", "210_engage_adnimem.RDS"))
saveRDS(engage_adnief_results, here::here("R_objects", "210_engage_adnief.RDS"))
saveRDS(engage_adas13_results, here::here("R_objects", "210_engage_adas13.RDS"))

saveRDS(donanemab_adnimem_results, here::here("R_objects", "210_donanemab_adnimem.RDS"))
saveRDS(donanemab_adnief_results, here::here("R_objects", "210_donanemab_adnief.RDS"))
saveRDS(donanemab_adas13_results, here::here("R_objects", "210_donanemab_adas13.RDS"))

saveRDS(shouldbe_adnimem_results, here::here("R_objects", "210_shouldbe_adnimem.RDS"))
saveRDS(shouldbe_adnief_results, here::here("R_objects", "210_shouldbe_adnief.RDS"))
saveRDS(shouldbe_adas13_results, here::here("R_objects", "210_shouldbe_adas13.RDS"))
