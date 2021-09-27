rm(list = setdiff(ls(), lsf.str())[!(setdiff(ls(), lsf.str()) %in% 'params')])
source(here::here('R', '002_folder-paths-and-options.R'))

# Import data

pet_data <- readRDS(here::here("R_objects", "020_amy_pet_04.RDS"))
harmonized <- readRDS(here::here("R_objects", "041_harmonized_main.RDS"))
cog_data <- readRDS(here::here("R_objects", "020_cog_data_03.RDS"))

adni_data <- harmonized %>%
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
