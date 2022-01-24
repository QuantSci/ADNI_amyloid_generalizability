rm(list = setdiff(ls(), lsf.str())[!(setdiff(ls(), lsf.str()) %in% 'params')])
source(here::here('R', '002_folder-paths-and-options.R'))

# Import data

pet_data <- readRDS(here::here("R_objects", "020_amy_pet_04.RDS"))
survey <- readRDS(here::here("R_objects", "041_survey_data.RDS"))
cog_data <- readRDS(here::here("R_objects", "020_cog_data_03.RDS"))

adni_data <- survey %>%
  dplyr::filter(DATA == "ADNI") %>%
  rename(RID = ID) %>%
  mutate(RID = as.numeric(RID))


## TAKE THESE IDS TO GET AMYLOID DATA - goes back to step 020

adni_data_IDs <- adni_data %>%
  select(RID)

saveRDS(adni_data_IDs, here::here("R_objects", "041_adni_IDs.RDS"))

# Merge together

merge1 <- left_join(pet_data, adni_data, by = "RID")
model1data <- left_join(merge1, cog_data, by = "RID") %>%
  select(RID, scaled_weights, bl_composite, m24_composite, bl_wholecereb, m24_wholecereb, bl_adas11, m24_adas11, bl_adas13, m24_adas13,
         AGE, MMSE, APOE41Y0N, DX)

## Impute missing

model1data_imp <- mice::mice(data = model1data,
              m = 1,
              seed = 8675309,
              pred = mice::quickpred(model1data,
                               inc = c("m24_composite", "m24_wholecereb", "m24_adas11", "m24_adas13"),
                               # this line only calls for imputation on vars with missing data
                               exc = c("RID", "scaled_weights")))

impdata <- complete(model1data_imp, 1)

## Get change scores

delta_data <- impdata %>%
  dplyr::mutate(delta_composite = m24_composite - bl_composite,
                delta_wholecereb = m24_wholecereb - bl_wholecereb,
                delta_adas11 = m24_adas11 - bl_adas11,
                delta_adas13 = m24_adas13 - bl_adas13)

# Correlations

# Baseline

# Unweighted
cor.test(delta_data$bl_wholecereb, delta_data$bl_adas11)

cor.test(delta_data$bl_wholecereb, delta_data$bl_adas13)

# Weighted
weights::wtd.cor(delta_data$bl_wholecereb, delta_data$bl_adas11, weight = delta_data$scaled_weights)

weights::wtd.cor(delta_data$bl_wholecereb, delta_data$bl_adas13, weight = delta_data$scaled_weights)


# Change Scores

# Unweighted
cor.test(delta_data$delta_composite, delta_data$delta_adas11)

cor.test(delta_data$delta_composite, delta_data$delta_adas13)

# Weighted

weights::wtd.cor(delta_data$delta_composite, delta_data$delta_adas11, weight = delta_data$scaled_weights)

weights::wtd.cor(delta_data$delta_composite, delta_data$delta_adas13, weight = delta_data$scaled_weights)

# Get fully standardized regression coefficients

#Amyloid at baseline & adas13 at baseline, no weights

m1 <- lm(bl_adas13 ~ bl_wholecereb, data = delta_data)
summary(m1)

#Amyloid at baseline and adas 13 change over 24 months, no weights

m2 <- lm(delta_adas13 ~ bl_wholecereb, data = delta_data)
summary(m2)

# Amyloid change over 24 months and adas 13 at baseline, no weights

m3 <- lm(bl_adas13 ~ delta_composite, data = delta_data)
summary(m3)

#Amyloid change over 24 months and adas 13 change of 24 months, no weights

m4 <- lm(delta_adas13 ~ delta_composite, data = delta_data)
summary(m4)

# amyloid at baseline and adas 13 at baseline, weights
delta_svy <- survey::svydesign(data = delta_data,
                               ids = ~RID,
                               weights = ~scaled_weights)

wm1 <- svyglm(bl_adas13 ~ bl_wholecereb, design = delta_svy)
summary(wm1)

#Amyloid at baseline and adas 13 change over 24 months, no weights

wm2 <- svyglm(delta_adas13 ~ bl_wholecereb, design = delta_svy)
summary(wm2)

# Amyloid change over 24 months and adas 13 at baseline, no weights

wm3 <- svyglm(bl_adas13 ~ delta_composite, design = delta_svy)
summary(wm3)

#Amyloid change over 24 months and adas 13 change of 24 months, no weights

wm4 <- svyglm(delta_adas13 ~ delta_composite, design = delta_svy)
summary(wm4)

# Amyloid at baseline predicting change in amyloid, no weights

m5 <- lm(delta_composite ~ bl_composite, data = delta_data)
summary(m5)

# Amyloid at baseline predicting change in amyloid, weights

wm5 <- svyglm(delta_composite ~ bl_composite, design = delta_svy)
summary(wm5)

# Cognition at baseline predicting change in amyloid, no weights

m6 <- lm(delta_composite ~ bl_adas13, data = delta_data)
summary(m6)

# Cognition at baseline predicting change in amyloid, weights

wm6 <- svyglm(delta_composite ~ bl_adas13, design = delta_svy)
summary(wm6)

# Cognition at baseline predicting change in cognition, no weights

m7 <- lm(delta_adas13 ~ bl_adas13, data = delta_data)
summary(m7)

# Cognition at baseline predicting change in cognition, weights

wm7 <- svyglm(delta_adas13 ~ bl_adas13, design = delta_svy)
summary(wm7)

## Cross-lagged panel model

clpm_model <- 'delta_adas13 ~ bl_adas13 + bl_composite
               delta_composite ~ bl_adas13 + bl_composite
               delta_adas13 ~ delta_composite
               bl_adas13 ~ bl_composite'

clpm1 <- lavaan::sem(model = clpm_model, data = delta_data)

summary(clpm1, fit.measures = T, standardized = T)

clpm_w1 <- lavaan.survey::lavaan.survey(clpm1, survey.design = delta_svy)

summary(clpm_w1, fit.measures = T, standardized = T)

lavaan::standardizedSolution(clpm_w1)

# Look in just MCI group

mci_data <- delta_data %>%
  dplyr::filter(DX == "MCI")


# Baseline

# Unweighted
cor.test(mci_data$bl_wholecereb, mci_data$bl_adas11)

cor.test(mci_data$bl_wholecereb, mci_data$bl_adas13)

# Weighted
weights::wtd.cor(mci_data$bl_wholecereb, mci_data$bl_adas11, weight = mci_data$scaled_weights)

weights::wtd.cor(mci_data$bl_wholecereb, mci_data$bl_adas13, weight = mci_data$scaled_weights)


# Change Scores

# Unweighted
cor.test(mci_data$delta_composite, mci_data$delta_adas11)

cor.test(mci_data$delta_composite, mci_data$delta_adas13)

# Weighted

weights::wtd.cor(mci_data$delta_composite, mci_data$delta_adas11, weight = mci_data$scaled_weights)

weights::wtd.cor(mci_data$delta_composite, mci_data$delta_adas13, weight = mci_data$scaled_weights)
