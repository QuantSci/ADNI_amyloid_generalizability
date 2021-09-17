rm(list = setdiff(ls(), lsf.str())[!(setdiff(ls(), lsf.str()) %in% 'params')])
source(here::here('R', '002_folder-paths-and-options.R'))

# Load Data

harmonized <- readRDS(here::here("R_objects", "040_imp_data.RDS")) %>%
  dplyr::mutate(dataset = dplyr::if_else(DATA == "HRS", 1, 0)) # recode for LR

# Assessing covariate balance

# (mean(ADNI) - mean(HRS)) / st.dev(HRS)

covariate_balance_data <- harmonized %>%
  mutate(SEXM1F0 = dplyr::if_else(GENDER == "1", 1, 0),
         HISP1Y0N = dplyr::if_else(ETHNICITY == "2", 1, 0),
         BLACK1Y0N = dplyr::if_else(RACE_3CAT == "Black", 1, 0)) %>%
  select(-GENDER, -ETHNICITY, -RACE_3CAT) %>%
  group_by(DATA, DX) %>%
  summarise(mean_age = mean(AGE, na.rm = TRUE),
            std_age = sd(AGE, na.rm = TRUE),
            mean_mmse = mean(MMSE, na.rm = T),
            std_mmse = sd(MMSE, na.rm = T),
            mean_edyrs = mean(EDYRS, na.rm = T),
            std_edyrs = sd(EDYRS, na.rm = T),
            mean_APOE = mean(as.numeric(APOE41Y0N), na.rm = T),
            std_APOE = mean(as.numeric(APOE41Y0N), na.rm = T),
            mean_sex = mean(SEXM1F0, na.rm = T),
            std_sex = sd(SEXM1F0, na.rm = T),
            mean_hisp = mean(HISP1Y0N, na.rm = T),
            std_hisp = sd(HISP1Y0N, na.rm = T),
            mean_blackrace = mean(BLACK1Y0N, na.rm = T),
            std_blackrace = sd(BLACK1Y0N, na.rm = T)) %>%
  ungroup()

get_std_mean_diff <- function(data, mean_var, sd_var){

mean_hrs_CN <- data %>%
  filter(DATA == "HRS" & DX == "CN") %>%
  select(mean_var) %>%
  pull()

mean_adni_CN <- data %>%
  filter(DATA == "ADNI" & DX == "CN") %>%
  select(mean_var) %>%
  pull()

sd_hrs_CN <- data %>%
  filter(DATA == "HRS" & DX == "CN") %>%
  select(sd_var) %>%
  pull()

mean_hrs_MCI <- data %>%
  filter(DATA == "HRS" & DX == "MCI") %>%
  select(mean_var) %>%
  pull()

mean_adni_MCI <- data %>%
  filter(DATA == "ADNI" & DX == "MCI") %>%
  select(mean_var) %>%
  pull()

sd_hrs_MCI <- data %>%
  filter(DATA == "HRS" & DX == "MCI") %>%
  select(sd_var) %>%
  pull()

mean_hrs_Dementia <- data %>%
  filter(DATA == "HRS" & DX == "Dementia") %>%
  select(mean_var) %>%
  pull()

mean_adni_Dementia <- data %>%
  filter(DATA == "ADNI" & DX == "Dementia") %>%
  select(mean_var) %>%
  pull()

sd_hrs_Dementia <- data %>%
  filter(DATA == "HRS" & DX == "Dementia") %>%
  select(sd_var) %>%
  pull()


std_mean_diff_CN <- (mean_adni_CN - mean_hrs_CN) / sd_hrs_CN
std_mean_diff_MCI <- (mean_adni_MCI - mean_hrs_MCI) / sd_hrs_MCI
std_mean_diff_Dementia <- (mean_adni_Dementia - mean_hrs_Dementia) / sd_hrs_Dementia

results <- data.frame(
  DX_group = c("CN", "MCI", "Dementia"),
  std_mean_diff = c(std_mean_diff_CN, std_mean_diff_MCI, std_mean_diff_Dementia))

return(results)

}

age_std_mean_diff <- get_std_mean_diff(data = covariate_balance_data,
                                       mean_var = "mean_age",
                                       sd_var = "std_age") %>%
  mutate(var = "age")

mmse_std_mean_diff <- get_std_mean_diff(data = covariate_balance_data,
                                       mean_var = "mean_mmse",
                                       sd_var = "std_mmse") %>%
  mutate(var = "mmse")

edyrs_std_mean_diff <- get_std_mean_diff(data = covariate_balance_data,
                                         mean_var = "mean_edyrs",
                                         sd_var = "std_edyrs") %>%
  mutate(var = "edyrs")

APOE_std_mean_diff <- get_std_mean_diff(data = covariate_balance_data,
                                         mean_var = "mean_APOE",
                                         sd_var = "std_APOE") %>%
  mutate(var = "APOE")

sex_std_mean_diff <- get_std_mean_diff(data = covariate_balance_data,
                                         mean_var = "mean_sex",
                                         sd_var = "std_sex") %>%
  mutate(var = "sex")

hisp_std_mean_diff <- get_std_mean_diff(data = covariate_balance_data,
                                         mean_var = "mean_hisp",
                                         sd_var = "std_hisp") %>%
  mutate(var = "hisp")

blackrace_std_mean_diff <- get_std_mean_diff(data = covariate_balance_data,
                                         mean_var = "mean_blackrace",
                                         sd_var = "std_blackrace") %>%
  mutate(var = "blackrace")


std_mean_diff_data <- rbind(age_std_mean_diff,
                            mmse_std_mean_diff,
                            edyrs_std_mean_diff,
                            APOE_std_mean_diff,
                            sex_std_mean_diff,
                            hisp_std_mean_diff,
                            blackrace_std_mean_diff)

ggplot() +
  geom_point(data = std_mean_diff_data, aes(y = var, x = std_mean_diff, color = DX_group)) +
  geom_vline(xintercept = 0, color = "black") +
  geom_vline(xintercept = -.25, color = "red") +
  geom_vline(xintercept = .25, color = "red")


# LR model

lr1 <- glm(dataset ~ AGE + DX + MMSE + GENDER + EDYRS + ETHNICITY + RACE_3CAT + APOE41Y0N,
           data = harmonized,
           family = "binomial"(link = "logit"),
           weights = WEIGHT)

summary(lr1)

summary(glm(dataset ~ AGE + DX + MMSE + GENDER + EDYRS + ETHNICITY + RACE_3CAT + APOE41Y0N,
            data = harmonized,
            family = "binomial"(link = "logit")))

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
                std_weights = weights * (sum_weights_adni / sum_weights_hrs))


psych::describeBy(harmonized$predprob, group = harmonized$DATA)

hist(harmonized_01$std_weights)

psych::describe(harmonized_01$std_weights)
