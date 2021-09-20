rm(list = setdiff(ls(), lsf.str())[!(setdiff(ls(), lsf.str()) %in% 'params')])
source(here::here('R', '002_folder-paths-and-options.R'))

# Load Data

harmonized <- readRDS(here::here("R_objects", "040_imp_data.RDS")) %>%
  dplyr::mutate(dataset = dplyr::if_else(DATA == "HRS", 1, 0)) # recode for LR

## Checking for linear functional form of MMSE, AGE, EDYRS

mmse_m1 <- glm(dataset ~ MMSE, data = harmonized, family = "binomial"(link = "logit"))

summary(mmse_m1)

mmse_m2 <- glm(dataset ~ rms::rcs(MMSE), data = harmonized, family = "binomial"(link = "logit"))

summary(mmse_m2)

anova(mmse_m1, mmse_m2, test = "LRT")

AGE_m1 <- glm(dataset ~ AGE, data = harmonized, family = "binomial"(link = "logit"))

summary(AGE_m1)

AGE_m2 <- glm(dataset ~ rms::rcs(AGE), data = harmonized, family = "binomial"(link = "logit"))

summary(AGE_m2)

anova(AGE_m1, AGE_m2, test = "LRT")

EDYRS_m1 <- glm(dataset ~ EDYRS, data = harmonized, family = "binomial"(link = "logit"))

summary(EDYRS_m1)

EDYRS_m2 <- glm(dataset ~ rms::rcs(EDYRS), data = harmonized, family = "binomial"(link = "logit"))

summary(EDYRS_m2)

anova(EDYRS_m1, EDYRS_m2, test = "LRT")

# Assessing covariate balance
# (mean(ADNI) - mean(HRS)) / st.dev(HRS)

## DX STRATIFIED

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

get_dxstrat_std_mean_diff <- function(data, mean_var, sd_var){

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

dxstrat_age_std_mean_diff <- get_dxstrat_std_mean_diff(data = covariate_balance_data,
                                       mean_var = "mean_age",
                                       sd_var = "std_age") %>%
  mutate(var = "age")

dxstrat_mmse_std_mean_diff <- get_dxstrat_std_mean_diff(data = covariate_balance_data,
                                       mean_var = "mean_mmse",
                                       sd_var = "std_mmse") %>%
  mutate(var = "mmse")

dxstrat_edyrs_std_mean_diff <- get_dxstrat_std_mean_diff(data = covariate_balance_data,
                                         mean_var = "mean_edyrs",
                                         sd_var = "std_edyrs") %>%
  mutate(var = "edyrs")

dxstrat_APOE_std_mean_diff <- get_dxstrat_std_mean_diff(data = covariate_balance_data,
                                         mean_var = "mean_APOE",
                                         sd_var = "std_APOE") %>%
  mutate(var = "APOE")

dxstrat_sex_std_mean_diff <- get_dxstrat_std_mean_diff(data = covariate_balance_data,
                                         mean_var = "mean_sex",
                                         sd_var = "std_sex") %>%
  mutate(var = "sex")

dxstrat_hisp_std_mean_diff <- get_dxstrat_std_mean_diff(data = covariate_balance_data,
                                         mean_var = "mean_hisp",
                                         sd_var = "std_hisp") %>%
  mutate(var = "hisp")

dxstrat_blackrace_std_mean_diff <- get_dxstrat_std_mean_diff(data = covariate_balance_data,
                                         mean_var = "mean_blackrace",
                                         sd_var = "std_blackrace") %>%
  mutate(var = "blackrace")


dxstrat_std_mean_diff_data <- rbind(dxstrat_std_mean_diffage_std_mean_diff,
                           dxstrat_std_mean_diffmmse_std_mean_diff,
                           dxstrat_std_mean_diffedyrs_std_mean_diff,
                           dxstrat_std_mean_diffAPOE_std_mean_diff,
                           dxstrat_std_mean_diffsex_std_mean_diff,
                           dxstrat_std_mean_diffhisp_std_mean_diff,
                           dxstrat_std_mean_diffblackrace_std_mean_diff)

ggplot() +
  geom_point(data = dxstrat_std_mean_diff_data, aes(y = var, x = std_mean_diff, color = DX_group)) +
  geom_vline(xintercept = 0, color = "black") +
  geom_vline(xintercept = -.25, color = "red") +
  geom_vline(xintercept = .25, color = "red")

# NO DX STRATA

get_std_mean_diff <- function(data, mean_var, sd_var){

  mean_hrs <- data %>%
    filter(DATA == "HRS") %>%
    select(mean_var) %>%
    pull()

  mean_adni <- data %>%
    filter(DATA == "ADNI") %>%
    select(mean_var) %>%
    pull()

  sd_hrs <- data %>%
    filter(DATA == "HRS") %>%
    select(sd_var) %>%
    pull()

  std_mean_diff <- (mean_adni - mean_hrs) / sd_hrs

}

covariate_balance_data <- harmonized %>%
  mutate(SEXM1F0 = dplyr::if_else(GENDER == "1", 1, 0),
         HISP1Y0N = dplyr::if_else(ETHNICITY == "2", 1, 0),
         BLACK1Y0N = dplyr::if_else(RACE_3CAT == "Black", 1, 0)) %>%
  select(-GENDER, -ETHNICITY, -RACE_3CAT) %>%
  group_by(DATA) %>%
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

age_std_mean_diff <- get_std_mean_diff(covariate_balance_data, "mean_age", "std_age")
mmse_std_mean_diff <- get_std_mean_diff(covariate_balance_data, "mean_mmse", "std_mmse")
edyrs_std_mean_diff <- get_std_mean_diff(covariate_balance_data, "mean_edyrs", "std_edyrs")
APOE_std_mean_diff <- get_std_mean_diff(covariate_balance_data, "mean_APOE", "std_APOE")
sex_std_mean_diff <- get_std_mean_diff(covariate_balance_data, "mean_sex", "std_sex")
hisp_std_mean_diff <- get_std_mean_diff(covariate_balance_data, "mean_hisp", "std_hisp")
blackrace_std_mean_diff <- get_std_mean_diff(covariate_balance_data, "mean_blackrace", "std_blackrace")

std_mean_diff_data <- data.frame(
  var = c("age", "mmse", "edyrs", "APOE", "sex", "hispanic", "black"),
  stdmeandiff = c(age_std_mean_diff, mmse_std_mean_diff, edyrs_std_mean_diff, APOE_std_mean_diff, sex_std_mean_diff,
                  hisp_std_mean_diff, blackrace_std_mean_diff)
)

ggplot() +
  geom_point(data = std_mean_diff_data, aes(y = var, x = stdmeandiff)) +
  geom_vline(xintercept = 0, color = "black") +
  geom_vline(xintercept = -.25, color = "red") +
  geom_vline(xintercept = .25, color = "red")

# WEIGHTED

# create survey objects

hrs_svy_design <- harmonized %>%
  dplyr::filter(DATA == "HRS") %>%
  srvyr::as_survey_design(ids = ID, weight = WEIGHT) %>%
  mutate(SEXM1F0 = dplyr::if_else(GENDER == "1", 1, 0),
         HISP1Y0N = dplyr::if_else(ETHNICITY == "2", 1, 0),
         BLACK1Y0N = dplyr::if_else(RACE_3CAT == "Black", 1, 0),
         APOE41Y0N = as.numeric(APOE41Y0N)) %>%
  select(-GENDER, -ETHNICITY, -RACE_3CAT)

adni_svy_design <- harmonized %>%
  dplyr::filter(DATA == "ADNI") %>%
  srvyr::as_survey_design(ids = ID, weight = WEIGHT) %>%
  mutate(SEXM1F0 = dplyr::if_else(GENDER == "1", 1, 0),
         HISP1Y0N = dplyr::if_else(ETHNICITY == "2", 1, 0),
         BLACK1Y0N = dplyr::if_else(RACE_3CAT == "Black", 1, 0),
         APOE41Y0N = as.numeric(APOE41Y0N)) %>%
  select(-GENDER, -ETHNICITY, -RACE_3CAT)

# Age

hrs_svy_age_mean <- hrs_svy_design %>%
  summarize(age = survey_mean(AGE)) %>%
  select(age) %>%
  pull()

hrs_svy_age_sd <- hrs_svy_design %>%
  summarise(age = survey_sd(AGE)) %>%
  pull()

adni_svy_age_mean <- adni_svy_design %>%
  summarize(age = survey_mean(AGE)) %>%
  select(age) %>%
  pull()

# MMSE

hrs_svy_MMSE_mean <- hrs_svy_design %>%
  summarize(MMSE = survey_mean(MMSE)) %>%
  select(MMSE) %>%
  pull()

hrs_svy_MMSE_sd <- hrs_svy_design %>%
  summarise(MMSE = survey_sd(MMSE)) %>%
  pull()

adni_svy_MMSE_mean <- adni_svy_design %>%
  summarize(MMSE = survey_mean(MMSE)) %>%
  select(MMSE) %>%
  pull()

# EDYRS

hrs_svy_EDYRS_mean <- hrs_svy_design %>%
  summarize(EDYRS = survey_mean(EDYRS)) %>%
  select(EDYRS) %>%
  pull()

hrs_svy_EDYRS_sd <- hrs_svy_design %>%
  summarise(EDYRS = survey_sd(EDYRS)) %>%
  pull()

adni_svy_EDYRS_mean <- adni_svy_design %>%
  summarize(EDYRS = survey_mean(EDYRS)) %>%
  select(EDYRS) %>%
  pull()

# APOE

hrs_svy_APOE_mean <- hrs_svy_design %>%
  summarize(APOE = survey_mean(APOE41Y0N)) %>%
  select(APOE) %>%
  pull()

hrs_svy_APOE_sd <- hrs_svy_design %>%
  summarise(APOE = survey_sd(APOE41Y0N)) %>%
  pull()

adni_svy_APOE_mean <- adni_svy_design %>%
  summarize(APOE = survey_mean(APOE41Y0N)) %>%
  select(APOE) %>%
  pull()

# SEX

hrs_svy_SEX_mean <- hrs_svy_design %>%
  summarize(SEX = survey_mean(SEXM1F0)) %>%
  select(SEX) %>%
  pull()

hrs_svy_SEX_sd <- hrs_svy_design %>%
  summarise(SEX = survey_sd(SEXM1F0)) %>%
  pull()

adni_svy_SEX_mean <- adni_svy_design %>%
  summarize(SEX = survey_mean(SEXM1F0)) %>%
  select(SEX) %>%
  pull()

# HISP

hrs_svy_HISP_mean <- hrs_svy_design %>%
  summarize(HISP = survey_mean(HISP1Y0N)) %>%
  select(HISP) %>%
  pull()

hrs_svy_HISP_sd <- hrs_svy_design %>%
  summarise(HISP = survey_sd(HISP1Y0N)) %>%
  pull()

adni_svy_HISP_mean <- adni_svy_design %>%
  summarize(HISP = survey_mean(HISP1Y0N)) %>%
  select(HISP) %>%
  pull()

# BLACKRACE

hrs_svy_BLACKRACE_mean <- hrs_svy_design %>%
  summarize(BLACKRACE = survey_mean(BLACK1Y0N)) %>%
  select(BLACKRACE) %>%
  pull()

hrs_svy_BLACKRACE_sd <- hrs_svy_design %>%
  summarise(BLACKRACE = survey_sd(BLACK1Y0N)) %>%
  pull()

adni_svy_BLACKRACE_mean <- adni_svy_design %>%
  summarize(BLACKRACE = survey_mean(BLACK1Y0N)) %>%
  select(BLACKRACE) %>%
  pull()

AGE_std_mean_diff_svy <- (hrs_svy_age_mean - adni_svy_age_mean) / hrs_svy_age_sd
MMSE_std_mean_diff_svy <- (hrs_svy_MMSE_mean - adni_svy_MMSE_mean) / hrs_svy_MMSE_sd
EDYRS_std_mean_diff_svy <- (hrs_svy_EDYRS_mean - adni_svy_EDYRS_mean) / hrs_svy_EDYRS_sd
APOE_std_mean_diff_svy <- (hrs_svy_APOE_mean - adni_svy_APOE_mean) / hrs_svy_APOE_sd
SEX_std_mean_diff_svy <- (hrs_svy_SEX_mean - adni_svy_SEX_mean) / hrs_svy_SEX_sd
HISP_std_mean_diff_svy <- (hrs_svy_HISP_mean - adni_svy_HISP_mean) / hrs_svy_HISP_sd
BLACKRACE_std_mean_diff_svy <- (hrs_svy_BLACKRACE_mean - adni_svy_BLACKRACE_mean) / hrs_svy_BLACKRACE_sd

std_mean_diff_data_svy <- data.frame(
  var = c("age", "mmse", "edyrs", "APOE", "sex", "hispanic", "black"),
  stdmeandiff = c(AGE_std_mean_diff_svy, MMSE_std_mean_diff_svy, EDYRS_std_mean_diff_svy,
                  APOE_std_mean_diff_svy, SEX_std_mean_diff_svy,
                  HISP_std_mean_diff_svy, BLACKRACE_std_mean_diff_svy)
)

ggplot() +
  geom_point(data = std_mean_diff_data_svy, aes(y = var, x = stdmeandiff)) +
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
