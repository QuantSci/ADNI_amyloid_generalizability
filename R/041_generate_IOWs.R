rm(list = setdiff(ls(), lsf.str())[!(setdiff(ls(), lsf.str()) %in% 'params')])
source(here::here('R', '002_folder-paths-and-options.R'))

# Load Data

harmonized <- readRDS(here::here("R_objects", "040_imp_data.RDS")) %>%
  dplyr::mutate(dataset = dplyr::if_else(DATA == "HRS", 1, 0)) # recode for LR



# LR model

lr1 <- glm(dataset ~ AGE + DX + MMSE + GENDER + EDYRS + ETHNICITY + RACE_3CAT + APOE41Y0N,
           data = harmonized,
           family = "binomial"(link = "logit"),
           weights = WEIGHT)

summary(lr1)

# Generate IOWs
# Weights only needed for ADNI

harmonized$predprob <- predict(lr1, type = "response")

sum_weights_hrs <- harmonized %>%
  dplyr::filter(DATA == "HRS") %>%
  dplyr::summarize(sum = sum(WEIGHT)) %>%
  pull()

sum_weights_adni <- harmonized %>%
  dplyr::filter(DATA == "ADNI") %>%
  dplyr::summarize(sum = sum(WEIGHT)) %>%
  pull()


harmonized_01 <- harmonized %>%
  dplyr::filter(DATA == "ADNI") %>%
  dplyr::mutate(weights = (1 - predprob) / predprob,
                std_weights = weights * sum_weights_adni / sum_weights_hrs)

hist(harmonized_01$std_weights)
