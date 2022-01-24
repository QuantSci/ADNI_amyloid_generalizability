rm(list = setdiff(ls(), lsf.str())[!(setdiff(ls(), lsf.str()) %in% 'params')])
source(here::here('R', '002_folder-paths-and-options.R'))

# Load Data

harmonized <- readRDS(here::here("R_objects", "040_imp_data.RDS")) %>%
  dplyr::mutate(dataset = dplyr::if_else(DATA == "HRS", 1, 0)) %>%
  mutate(SEXM1F0 = dplyr::if_else(GENDER == "1", 1, 0),
         HISP1Y0N = dplyr::if_else(ETHNICITY == "2", 1, 0),
         BLACK1Y0N = dplyr::if_else(RACE_3CAT == "Black", 1, 0))

# Looking at just sex

lr_sexonly <- glm(dataset ~ SEXM1F0,
                     data = harmonized,
                     family = "binomial"(link = "logit"),
                     weights = WEIGHT)

summary(lr_sexonly)

# Generate IOWs
# Sex Only

harmonized$predprob_sexonly <- predict(lr_sexonly, type = "response")

sum_weights_hrs <- harmonized %>%
  dplyr::filter(DATA == "HRS") %>%
  dplyr::summarize(sum = sum(WEIGHT)) %>%
  pull()

sum_weights_adni <- harmonized %>%
  dplyr::filter(DATA == "ADNI") %>%
  dplyr::summarize(sum = sum(WEIGHT)) %>%
  pull()

adni_sample_size <- nrow(adni_08)

harmonized_main <- harmonized %>%
  dplyr::mutate(weights = predprob_sexonly / (1 - predprob_sexonly))

sum_IPWs_ADNI <- harmonized_main %>%
  dplyr::filter(DATA == "ADNI") %>%
  dplyr::summarize(sum = sum(weights)) %>%
  pull()


harmonized_main <- harmonized_main %>%
  dplyr::mutate(scaled_weights = (adni_sample_size/sum_IPWs_ADNI) * weights)

mean(harmonized_main$scaled_weights)

sum_scaled_IPWs <- harmonized_main %>%
  dplyr::summarize(sum = sum(scaled_weights)) %>%
  pull()

scaled_IPWs_ADNI <- harmonized_main %>%
  dplyr::filter(DATA == "ADNI") %>%
  dplyr::summarise(mean = mean(scaled_weights),
                   sum = sum(scaled_weights))

survey_data_ADNI <- harmonized_main %>%
  dplyr::filter(DATA == "ADNI")

survey_data_HRS <- harmonized_main %>%
  dplyr::filter(DATA == "HRS") %>%
  dplyr::mutate(scaled_weights = WEIGHT)

survey_data <- rbind(survey_data_ADNI, survey_data_HRS)



harmonzed_main_svydesign <- survey_data %>%
  srvyr::as_survey_design(ids = ID, weight = scaled_weights)

ipw_main <- harmonzed_main_svydesign %>%
  group_by(DATA) %>%
  summarize(mean_age = survey_mean(AGE),
            std_age = survey_sd(AGE),
            mean_mmse = survey_mean(MMSE),
            std_mmse = survey_sd(MMSE),
            mean_edyrs = survey_mean(EDYRS),
            std_edyrs = survey_sd(EDYRS),
            mean_APOE = survey_mean(as.numeric(APOE41Y0N)),
            std_APOE = survey_mean(as.numeric(APOE41Y0N)),
            mean_sex = survey_mean(SEXM1F0),
            std_sex = survey_sd(SEXM1F0),
            mean_hisp = survey_mean(HISP1Y0N),
            std_hisp = survey_sd(HISP1Y0N),
            mean_blackrace = survey_mean(BLACK1Y0N),
            std_blackrace = survey_sd(BLACK1Y0N))

age_std_mean_diff_ipw_main <- get_std_mean_diff(ipw_main, "mean_age", "std_age")
mmse_std_mean_diff_ipw_main <- get_std_mean_diff(ipw_main, "mean_mmse", "std_mmse")
edyrs_std_mean_diff_ipw_main <- get_std_mean_diff(ipw_main, "mean_edyrs", "std_edyrs")
APOE_std_mean_diff_ipw_main <- get_std_mean_diff(ipw_main, "mean_APOE", "std_APOE")
sex_std_mean_diff_ipw_main <- get_std_mean_diff(ipw_main, "mean_sex", "std_sex")
hisp_std_mean_diff_ipw_main <- get_std_mean_diff(ipw_main, "mean_hisp", "std_hisp")
blackrace_std_mean_diff_ipw_main <- get_std_mean_diff(ipw_main, "mean_blackrace", "std_blackrace")

std_mean_diff_ipw_main_data <- data.frame(
  var = c("age", "mmse", "edyrs", "APOE", "sex", "hispanic", "black"),
  stdmeandiff = c(age_std_mean_diff_ipw_main, mmse_std_mean_diff_ipw_main, edyrs_std_mean_diff_ipw_main, APOE_std_mean_diff_ipw_main, sex_std_mean_diff_ipw_main,
                  hisp_std_mean_diff_ipw_main, blackrace_std_mean_diff_ipw_main)
)

ggplot() +
  geom_point(data = std_mean_diff_ipw_main_data, aes(y = var, x = stdmeandiff)) +
  geom_vline(xintercept = 0, color = "black") +
  geom_vline(xintercept = -.25, color = "red") +
  geom_vline(xintercept = .25, color = "red")
