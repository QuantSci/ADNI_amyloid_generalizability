rm(list = setdiff(ls(), lsf.str())[!(setdiff(ls(), lsf.str()) %in% 'params')])
source(here::here('R', '002_folder-paths-and-options.R'))

# Load Data

harmonized <- readRDS(here::here("R_objects", "040_imp_data.RDS")) %>%
  dplyr::mutate(dataset = dplyr::if_else(DATA == "HRS", 1, 0)) # recode for LR



# LR model

lr1 <- glm(dataset ~ AGE + DX + MMSE + GENDER + EDYRS + ETHNICITY + RACE_3CAT + APOE41Y0N,
           data = harmonized,
           family = "binomial"(link = "logit"))

summary(lr1)

# Generate IOWs

harmonized$predprob <- predict(lr1, type = "response")

harmonized_01 <- harmonized %>%
  dplyr::mutate(weights = 1/predprob)
