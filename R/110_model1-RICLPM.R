rm(list = setdiff(ls(), lsf.str())[!(setdiff(ls(), lsf.str()) %in% 'params')])
source(here::here('R', '002_folder-paths-and-options.R'))
source(here::here('R', '005_libraries.R'))

# Import data

pet_data <- readRDS(here::here("R_objects", "020_amy_pet_04.RDS"))
survey <- readRDS(here::here("R_objects", "042_survey_data.RDS"))
uw_cog_data <- readRDS(here::here("R_objects", "020_uw_cog_data.RDS"))

## Checking Bins

pet_data_bins <- pet_data %>%
  dplyr::mutate(ENGAGE_BIN = dplyr::if_else(bl_composite > 25, 1, 0), # ENGAGE/EMERGE
                DONANEMAB_BIN = dplyr::if_else(bl_composite > 36, 1, 0), # donanemab
                should_be_bin_1 = dplyr::if_else(between(bl_composite, 20, 50), 1, 0),
                should_be_bin_2 = dplyr::if_else(between(bl_composite, 26, 50), 1, 0),
                should_be_bin_3 = dplyr::if_else(between(bl_composite, 15, 50), 1, 0))

pet_data_bins_wholecereb <- pet_data %>%
  dplyr::mutate(ENGAGE_BIN = dplyr::if_else(bl_wholecereb > 25, 1, 0), # ENGAGE/EMERGE
                DONANEMAB_BIN = dplyr::if_else(bl_wholecereb > 36, 1, 0), # donanemab
                should_be_bin_1 = dplyr::if_else(between(bl_wholecereb, 20, 50), 1, 0),
                should_be_bin_2 = dplyr::if_else(between(bl_wholecereb, 26, 50), 1, 0),
                should_be_bin_3 = dplyr::if_else(between(bl_wholecereb, 15, 50), 1, 0))





adni_data <- survey %>%
  dplyr::filter(DATA == "ADNI") %>%
  rename(RID = ID) %>%
  mutate(RID = as.numeric(RID))

merge1 <- left_join(pet_data, adni_data, by = "RID")
model1data <- left_join(merge1, uw_cog_data, by = "RID") %>%
  select(RID, scaled_weights, bl_composite, m24_composite, m48_composite,
         bl_adni_mem, m24_adni_mem, m48_adni_mem,
         bl_adni_ef, m24_adni_ef, m48_adni_ef,
         AGE, MMSE, APOE41Y0N, DX) %>%
  mutate(scaled_weights = as.vector(scaled_weights))

model1data_imp <- mice::mice(data = model1data,
                             m = 1,
                             seed = 8675309,
                             pred = mice::quickpred(model1data,
                             inc = c("m24_composite", "m24_wholecereb", "m24_adni_mem", "m24_adni_ef"),
                             # this line only calls for imputation on vars with missing data
                             exc = c("RID", "scaled_weights")))

impdata <- complete(model1data_imp, 1)


svy_design <- survey::svydesign(data = impdata,
                               ids = ~RID,
                               weights = ~scaled_weights)

## CLPM - adni mem

clpm_model_adnimem <- 'm24_adni_mem ~ bl_adni_mem + -.008*bl_composite
               m24_composite ~ bl_adni_mem + bl_composite
               m24_adni_mem ~ m24_composite
               bl_adni_mem ~ bl_composite'

clpm1 <- lavaan::sem(model = clpm_model_adnimem, data = impdata)

summary(clpm1, fit.measures = T, standardized = T)

clpm_w1 <- lavaan.survey::lavaan.survey(clpm1, survey.design = svy_design)

summary(clpm_w1, fit.measures = T, standardized = T)

lavaan::standardizedSolution(clpm_w1)


# Rich back of napkin math
# .50/.20 = 2.5 (number of SDs)

# .28 / (78/52) = .19 (SD change in amyloid)

# 2.5 / .19 = 13 years

# adni-ef

clpm_model_adnief <- 'm24_adni_ef ~ bl_adni_ef + bl_composite
               m24_composite ~ -.08*bl_adni_ef + bl_composite
               m24_adni_ef ~ m24_composite
               bl_adni_ef ~ bl_composite'

clpm2 <- lavaan::sem(model = clpm_model_adnief, data = impdata)

summary(clpm2, fit.measures = T, standardized = T)

clpm_w2 <- lavaan.survey::lavaan.survey(clpm2, survey.design = svy_design)

summary(clpm_w2, fit.measures = T, standardized = T)

lavaan::standardizedSolution(clpm_w2)

# Rich back of napkin math
# .50/.30 = 1.7 (number of SDs)

# .28 / (78/52) = .19 (SD change in amyloid)

# 1.7 / .19 = 9 years

# (.2/.3) / .19 = 3.5 years

