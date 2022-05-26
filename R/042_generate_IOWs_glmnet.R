rm(list = setdiff(ls(), lsf.str())[!(setdiff(ls(), lsf.str()) %in% 'params')])
source(here::here('R', '002_folder-paths-and-options.R'))

# Load Data

harmonized <- readRDS(here::here("R_objects", "040_imp_data.RDS")) %>%
  dplyr::mutate(dataset = dplyr::if_else(DATA == "HRS", 1, 0)) %>%
  mutate(SEXM1F0 = dplyr::if_else(GENDER == "1", 1, 0),
         HISP1Y0N = dplyr::if_else(ETHNICITY == "2", 1, 0),
         BLACK1Y0N = dplyr::if_else(RACE_3CAT == "Black", 1, 0),
         MCI1Y0N = dplyr::if_else(DX == "MCI", 1, 0),
         DEM1Y0N = dplyr::if_else(DX == "Dementia", 1, 0),
         APOE41Y0N = as.numeric(APOE41Y0N),
         APOE41Y0N = dplyr::if_else(APOE41Y0N == 2, 1, 0)) %>% # recode for LR
  arrange(ID)


## load functions

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

get_cohen_h <- function(p1, p2){

  h <- 2*asin(sqrt(p1))-2*asin(sqrt(p2))
}


## Use elastic net to look at all second orderinteractions

response <- as.matrix(harmonized$dataset)

predictors <- harmonized %>%
  dplyr::transmute(age_rcs = rms::rcs(AGE),
                MCI1Y0N = MCI1Y0N,
                DEM1Y0N = DEM1Y0N,
                MMSE_rcs = rms::rcs(MMSE),
                SEXM1F0 = SEXM1F0,
                EDYRS_rcs = rms::rcs(EDYRS),
                HISP1Y0N = HISP1Y0N,
                BLACK1Y0N = BLACK1Y0N,
                APOE41Y0N = APOE41Y0N)

## see https://stackoverflow.com/questions/27580267/how-to-make-all-interactions-before-using-glmnet
## for making interaction terms

create_intx <- as.formula(response ~ .^2)
predictors_intx <- model.matrix(create_intx, predictors)[, -1]


elasticnet_second <- glmnet::glmnet(x = predictors_intx,
                                    y = response,
                     family = "binomial"(link = "logit"),
                     weights = harmonized$WEIGHT,
                     nlambda = 50)

print(elasticnet_second)

# 20-fold cross validation

cv_second <- glmnet::cv.glmnet(x = predictors_intx,
                       y = response,
                       family = "binomial"(link = "logit"),
                       weights = harmonized$WEIGHT,
                       type.measure = "mse",
                       nfolds = 20)

cv_second$lambda.min
cv_second$lambda.1se

coef(elasticnet_second, s = cv_second$lambda.min)

# Generate IOWs
# Main effects Only

harmonized$predprob_glmnet <- predict(elasticnet_second, newx = predictors_intx[1:1960,], type = "response", s = cv_second$lambda.min)

sum_weights_hrs <- harmonized %>%
  dplyr::filter(DATA == "HRS") %>%
  dplyr::summarize(sum = sum(WEIGHT)) %>%
  pull()

sum_weights_adni <- harmonized %>%
  dplyr::filter(DATA == "ADNI") %>%
  dplyr::summarize(sum = sum(WEIGHT)) %>%
  pull()

harmonized_elastic <- harmonized %>%
  dplyr::mutate(weights = predprob_glmnet / (1 - predprob_glmnet))
# ppt formula was backwards, was initially 1-predprob / predprob

sum_IPWs_ADNI <- harmonized_elastic %>%
  dplyr::filter(DATA == "ADNI") %>%
  dplyr::summarize(sum = sum(weights)) %>%
  pull() ## Pull sum of the IPWs in just ADNI For scaling

adni_sample_size <- harmonized_elastic %>%
  dplyr::filter(DATA == "ADNI") %>%
  nrow()

harmonized_elastic <- harmonized_elastic %>%
  dplyr::mutate(scaled_weights = (adni_sample_size/sum_IPWs_ADNI) * weights) # scale weights

survey_data_ADNI <- harmonized_elastic %>%
  dplyr::filter(DATA == "ADNI") # want scaled weights ONLY IN ADNI

survey_data_HRS <- harmonized_elastic %>%
  dplyr::filter(DATA == "HRS") %>% # want original ADAMS weights, so rename them to scaled_weights to make the rbind in the next step
  dplyr::mutate(scaled_weights = WEIGHT)

survey_data <- rbind(survey_data_ADNI, survey_data_HRS)

harmonized_elastic_svydesign <- survey_data %>%
  srvyr::as_survey_design(ids = ID, weight = scaled_weights) #survey design for st mean diff

ipw_elastic <- harmonized_elastic_svydesign %>% # calculated std mean diff
  group_by(DATA) %>%
  summarize(mean_age = survey_mean(AGE),
            std_age = survey_sd(AGE),
            mean_mmse = survey_mean(MMSE),
            std_mmse = survey_sd(MMSE),
            mean_edyrs = survey_mean(EDYRS),
            std_edyrs = survey_sd(EDYRS))

# now calculate cohen's h

hrs_ipw_design <- survey_data_HRS %>%
  srvyr::as_survey_design(ids = ID, weight = scaled_weights)

adni_ipw_design <- survey_data_ADNI %>%
  srvyr::as_survey_design(ids = ID, weight = scaled_weights)

# APOE

hrs_ipw_APOE_Y <- hrs_ipw_design %>%
  group_by(APOE41Y0N) %>%
  summarize(prop = survey_mean()) %>%
  filter(APOE41Y0N == 1) %>%
  select(prop) %>%
  pull()

hrs_ipw_APOE_N <- hrs_ipw_design %>%
  group_by(APOE41Y0N) %>%
  summarize(prop = survey_mean()) %>%
  filter(APOE41Y0N == 0) %>%
  select(prop) %>%
  pull()

adni_ipw_APOE_Y <- adni_ipw_design %>%
  group_by(APOE41Y0N) %>%
  summarize(prop = survey_mean()) %>%
  filter(APOE41Y0N == 1) %>%
  select(prop) %>%
  pull()

adni_ipw_APOE_N <- adni_ipw_design %>%
  group_by(APOE41Y0N) %>%
  summarize(prop = survey_mean()) %>%
  filter(APOE41Y0N == 0) %>%
  select(prop) %>%
  pull()

# SEX

hrs_ipw_SEX_M <- hrs_ipw_design %>%
  group_by(SEXM1F0) %>%
  summarize(prop = survey_mean()) %>%
  filter(SEXM1F0 == 1) %>%
  select(prop) %>%
  pull()

hrs_ipw_SEX_F <- hrs_ipw_design %>%
  group_by(SEXM1F0) %>%
  summarize(prop = survey_mean()) %>%
  filter(SEXM1F0 == 0) %>%
  select(prop) %>%
  pull()

adni_ipw_SEX_M <- adni_ipw_design %>%
  group_by(SEXM1F0) %>%
  summarize(prop = survey_mean()) %>%
  filter(SEXM1F0 == 1) %>%
  select(prop) %>%
  pull()

adni_ipw_SEX_F <- adni_ipw_design %>%
  group_by(SEXM1F0) %>%
  summarize(prop = survey_mean()) %>%
  filter(SEXM1F0 == 0) %>%
  select(prop) %>%
  pull()

# HISP

hrs_ipw_HISP_Y <- hrs_ipw_design %>%
  group_by(HISP1Y0N) %>%
  summarize(prop = survey_mean()) %>%
  filter(HISP1Y0N == 1) %>%
  select(prop) %>%
  pull()

hrs_ipw_HISP_N <- hrs_ipw_design %>%
  group_by(HISP1Y0N) %>%
  summarize(prop = survey_mean()) %>%
  filter(HISP1Y0N == 0) %>%
  select(prop) %>%
  pull()

adni_ipw_HISP_Y <- adni_ipw_design %>%
  group_by(HISP1Y0N) %>%
  summarize(prop = survey_mean()) %>%
  filter(HISP1Y0N == 1) %>%
  select(prop) %>%
  pull()

adni_ipw_HISP_N <- adni_ipw_design %>%
  group_by(HISP1Y0N) %>%
  summarize(prop = survey_mean()) %>%
  filter(HISP1Y0N == 0) %>%
  select(prop) %>%
  pull()


# BLACKRACE

hrs_ipw_BLACK_Y <- hrs_ipw_design %>%
  group_by(BLACK1Y0N) %>%
  summarize(prop = survey_mean()) %>%
  filter(BLACK1Y0N == 1) %>%
  select(prop) %>%
  pull()

hrs_ipw_BLACK_N <- hrs_ipw_design %>%
  group_by(BLACK1Y0N) %>%
  summarize(prop = survey_mean()) %>%
  filter(BLACK1Y0N == 0) %>%
  select(prop) %>%
  pull()

adni_ipw_BLACK_Y <- adni_ipw_design %>%
  group_by(BLACK1Y0N) %>%
  summarize(prop = survey_mean()) %>%
  filter(BLACK1Y0N == 1) %>%
  select(prop) %>%
  pull()

adni_ipw_BLACK_N <- adni_ipw_design %>%
  group_by(BLACK1Y0N) %>%
  summarize(prop = survey_mean()) %>%
  filter(BLACK1Y0N == 0) %>%
  select(prop) %>%
  pull()


#MCI

hrs_ipw_MCI_Y <- hrs_ipw_design %>%
  group_by(MCI1Y0N) %>%
  summarize(prop = survey_mean()) %>%
  filter(MCI1Y0N == 1) %>%
  select(prop) %>%
  pull()

hrs_ipw_MCI_N <- hrs_ipw_design %>%
  group_by(MCI1Y0N) %>%
  summarize(prop = survey_mean()) %>%
  filter(MCI1Y0N == 0) %>%
  select(prop) %>%
  pull()

adni_ipw_MCI_Y <- adni_ipw_design %>%
  group_by(MCI1Y0N) %>%
  summarize(prop = survey_mean()) %>%
  filter(MCI1Y0N == 1) %>%
  select(prop) %>%
  pull()

adni_ipw_MCI_N <- adni_ipw_design %>%
  group_by(MCI1Y0N) %>%
  summarize(prop = survey_mean()) %>%
  filter(MCI1Y0N == 0) %>%
  select(prop) %>%
  pull()


#DEM

hrs_ipw_DEM_Y <- hrs_ipw_design %>%
  group_by(DEM1Y0N) %>%
  summarize(prop = survey_mean()) %>%
  filter(DEM1Y0N == 1) %>%
  select(prop) %>%
  pull()

hrs_ipw_DEM_N <- hrs_ipw_design %>%
  group_by(DEM1Y0N) %>%
  summarize(prop = survey_mean()) %>%
  filter(DEM1Y0N == 0) %>%
  select(prop) %>%
  pull()

adni_ipw_DEM_Y <- adni_ipw_design %>%
  group_by(DEM1Y0N) %>%
  summarize(prop = survey_mean()) %>%
  filter(DEM1Y0N == 1) %>%
  select(prop) %>%
  pull()

adni_ipw_DEM_N <- adni_ipw_design %>%
  group_by(DEM1Y0N) %>%
  summarize(prop = survey_mean()) %>%
  filter(DEM1Y0N == 0) %>%
  select(prop) %>%
  pull()



age_effect_size_ipw_elastic <- get_std_mean_diff(ipw_elastic, "mean_age", "std_age")
mmse_effect_size_ipw_elastic <- get_std_mean_diff(ipw_elastic, "mean_mmse", "std_mmse")
edyrs_effect_size_ipw_elastic <- get_std_mean_diff(ipw_elastic, "mean_edyrs", "std_edyrs")
APOE_effect_size_ipw_elastic      <- get_cohen_h(hrs_ipw_APOE_Y, adni_ipw_APOE_Y)
sex_effect_size_ipw_elastic      <- get_cohen_h(hrs_ipw_SEX_F, adni_ipw_SEX_F)
hisp_effect_size_ipw_elastic      <- get_cohen_h(hrs_ipw_HISP_Y, adni_ipw_HISP_Y)
blackrace_effect_size_ipw_elastic <- get_cohen_h(hrs_ipw_BLACK_Y, adni_ipw_BLACK_Y)
MCI_effect_size_ipw_elastic       <- get_cohen_h(hrs_ipw_MCI_Y, adni_ipw_MCI_Y)
DEM_effect_size_ipw_elastic       <- get_cohen_h(hrs_ipw_DEM_Y, adni_ipw_DEM_Y)

effect_size_ipw_elastic_data <- data.frame(
  var = c("female", "age", "black", "hispanic", "edyrs", "mmse", "MCI", "dem", "APOE"),
  stdmeandiff = c(sex_effect_size_ipw_elastic, age_effect_size_ipw_elastic,
                  blackrace_effect_size_ipw_elastic, hisp_effect_size_ipw_elastic,
                  edyrs_effect_size_ipw_elastic, mmse_effect_size_ipw_elastic,
                  MCI_effect_size_ipw_elastic, DEM_effect_size_ipw_elastic,
                  APOE_effect_size_ipw_elastic)
)

# make covariate balance plot

ggplot() +
  geom_point(data = effect_size_ipw_elastic_data, aes(y = var, x = stdmeandiff)) +
  geom_vline(xintercept = 0, color = "black") +
  geom_vline(xintercept = -.25, color = "red") +
  geom_vline(xintercept = .25, color = "red")


# get un-weighted estimates


hrs_svy_design <- harmonized %>%
  dplyr::filter(DATA == "HRS") %>%
  srvyr::as_survey_design(ids = ID, weight = WEIGHT) %>%
  mutate(SEXM1F0 = dplyr::if_else(GENDER == "1", 1, 0),
         HISP1Y0N = dplyr::if_else(ETHNICITY == "2", 1, 0),
         BLACK1Y0N = dplyr::if_else(RACE_3CAT == "Black", 1, 0),
         MCI1Y0N = dplyr::if_else(DX == "MCI", 1, 0),
         DEM1Y0N = dplyr::if_else(DX == "Dementia", 1, 0)) %>%
  select(-GENDER, -ETHNICITY, -RACE_3CAT, -DX)

adni_svy_design <- harmonized %>%
  dplyr::filter(DATA == "ADNI") %>%
  srvyr::as_survey_design(ids = ID, weight = WEIGHT) %>%
  mutate(SEXM1F0 = dplyr::if_else(GENDER == "1", 1, 0),
         HISP1Y0N = dplyr::if_else(ETHNICITY == "2", 1, 0),
         BLACK1Y0N = dplyr::if_else(RACE_3CAT == "Black", 1, 0),
         MCI1Y0N = dplyr::if_else(DX == "MCI", 1, 0),
         DEM1Y0N = dplyr::if_else(DX == "Dementia", 1, 0)) %>%
  select(-GENDER, -ETHNICITY, -RACE_3CAT, -DX)

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

hrs_svy_APOE_Y <- hrs_svy_design %>%
  group_by(APOE41Y0N) %>%
  summarize(prop = survey_mean()) %>%
  filter(APOE41Y0N == 1) %>%
  select(prop) %>%
  pull()

hrs_svy_APOE_N <- hrs_svy_design %>%
  group_by(APOE41Y0N) %>%
  summarize(prop = survey_mean()) %>%
  filter(APOE41Y0N == 0) %>%
  select(prop) %>%
  pull()

adni_svy_APOE_Y <- adni_svy_design %>%
  group_by(APOE41Y0N) %>%
  summarize(prop = survey_mean()) %>%
  filter(APOE41Y0N == 1) %>%
  select(prop) %>%
  pull()

adni_svy_APOE_N <- adni_svy_design %>%
  group_by(APOE41Y0N) %>%
  summarize(prop = survey_mean()) %>%
  filter(APOE41Y0N == 0) %>%
  select(prop) %>%
  pull()

# SEX

hrs_svy_SEX_M <- hrs_svy_design %>%
  group_by(SEXM1F0) %>%
  summarize(prop = survey_mean()) %>%
  filter(SEXM1F0 == 1) %>%
  select(prop) %>%
  pull()

hrs_svy_SEX_F <- hrs_svy_design %>%
  group_by(SEXM1F0) %>%
  summarize(prop = survey_mean()) %>%
  filter(SEXM1F0 == 0) %>%
  select(prop) %>%
  pull()

adni_svy_SEX_M <- adni_svy_design %>%
  group_by(SEXM1F0) %>%
  summarize(prop = survey_mean()) %>%
  filter(SEXM1F0 == 1) %>%
  select(prop) %>%
  pull()

adni_svy_SEX_F <- adni_svy_design %>%
  group_by(SEXM1F0) %>%
  summarize(prop = survey_mean()) %>%
  filter(SEXM1F0 == 0) %>%
  select(prop) %>%
  pull()

# HISP

hrs_svy_HISP_Y <- hrs_svy_design %>%
  group_by(HISP1Y0N) %>%
  summarize(prop = survey_mean()) %>%
  filter(HISP1Y0N == 1) %>%
  select(prop) %>%
  pull()

hrs_svy_HISP_N <- hrs_svy_design %>%
  group_by(HISP1Y0N) %>%
  summarize(prop = survey_mean()) %>%
  filter(HISP1Y0N == 0) %>%
  select(prop) %>%
  pull()

adni_svy_HISP_Y <- adni_svy_design %>%
  group_by(HISP1Y0N) %>%
  summarize(prop = survey_mean()) %>%
  filter(HISP1Y0N == 1) %>%
  select(prop) %>%
  pull()

adni_svy_HISP_N <- adni_svy_design %>%
  group_by(HISP1Y0N) %>%
  summarize(prop = survey_mean()) %>%
  filter(HISP1Y0N == 0) %>%
  select(prop) %>%
  pull()


# BLACKRACE

hrs_svy_BLACK_Y <- hrs_svy_design %>%
  group_by(BLACK1Y0N) %>%
  summarize(prop = survey_mean()) %>%
  filter(BLACK1Y0N == 1) %>%
  select(prop) %>%
  pull()

hrs_svy_BLACK_N <- hrs_svy_design %>%
  group_by(BLACK1Y0N) %>%
  summarize(prop = survey_mean()) %>%
  filter(BLACK1Y0N == 0) %>%
  select(prop) %>%
  pull()

adni_svy_BLACK_Y <- adni_svy_design %>%
  group_by(BLACK1Y0N) %>%
  summarize(prop = survey_mean()) %>%
  filter(BLACK1Y0N == 1) %>%
  select(prop) %>%
  pull()

adni_svy_BLACK_N <- adni_svy_design %>%
  group_by(BLACK1Y0N) %>%
  summarize(prop = survey_mean()) %>%
  filter(BLACK1Y0N == 0) %>%
  select(prop) %>%
  pull()


#MCI

hrs_svy_MCI_Y <- hrs_svy_design %>%
  group_by(MCI1Y0N) %>%
  summarize(prop = survey_mean()) %>%
  filter(MCI1Y0N == 1) %>%
  select(prop) %>%
  pull()

hrs_svy_MCI_N <- hrs_svy_design %>%
  group_by(MCI1Y0N) %>%
  summarize(prop = survey_mean()) %>%
  filter(MCI1Y0N == 0) %>%
  select(prop) %>%
  pull()

adni_svy_MCI_Y <- adni_svy_design %>%
  group_by(MCI1Y0N) %>%
  summarize(prop = survey_mean()) %>%
  filter(MCI1Y0N == 1) %>%
  select(prop) %>%
  pull()

adni_svy_MCI_N <- adni_svy_design %>%
  group_by(MCI1Y0N) %>%
  summarize(prop = survey_mean()) %>%
  filter(MCI1Y0N == 0) %>%
  select(prop) %>%
  pull()


#DEM

hrs_svy_DEM_Y <- hrs_svy_design %>%
  group_by(DEM1Y0N) %>%
  summarize(prop = survey_mean()) %>%
  filter(DEM1Y0N == 1) %>%
  select(prop) %>%
  pull()

hrs_svy_DEM_N <- hrs_svy_design %>%
  group_by(DEM1Y0N) %>%
  summarize(prop = survey_mean()) %>%
  filter(DEM1Y0N == 0) %>%
  select(prop) %>%
  pull()

adni_svy_DEM_Y <- adni_svy_design %>%
  group_by(DEM1Y0N) %>%
  summarize(prop = survey_mean()) %>%
  filter(DEM1Y0N == 1) %>%
  select(prop) %>%
  pull()

adni_svy_DEM_N <- adni_svy_design %>%
  group_by(DEM1Y0N) %>%
  summarize(prop = survey_mean()) %>%
  filter(DEM1Y0N == 0) %>%
  select(prop) %>%
  pull()

# Calculate effect sizes

AGE_effect_size_svy <- (hrs_svy_age_mean - adni_svy_age_mean) / hrs_svy_age_sd
MMSE_effect_size_svy <- (hrs_svy_MMSE_mean - adni_svy_MMSE_mean) / hrs_svy_MMSE_sd
EDYRS_effect_size_svy <- (hrs_svy_EDYRS_mean - adni_svy_EDYRS_mean) / hrs_svy_EDYRS_sd
APOE_effect_size_svy <- get_cohen_h(hrs_svy_APOE_Y, adni_svy_APOE_Y)
SEX_effect_size_svy <- get_cohen_h(hrs_svy_SEX_F, adni_svy_SEX_F)
HISP_effect_size_svy <- get_cohen_h(hrs_svy_HISP_Y, adni_svy_HISP_Y)
BLACK_effect_size_svy <- get_cohen_h(hrs_svy_BLACK_Y, adni_svy_BLACK_Y)
MCI_effect_size_svy <- get_cohen_h(hrs_svy_MCI_Y, adni_svy_MCI_Y)
DEM_effect_size_svy <- get_cohen_h(hrs_svy_DEM_Y, adni_svy_DEM_Y)


effect_size_data_svy <- data.frame(
  var = c("female", "age", "black", "hispanic", "edyrs", "mmse", "MCI", "dem", "APOE"),
  stdmeandiff = c(SEX_effect_size_svy, AGE_effect_size_svy,
                  BLACK_effect_size_svy, HISP_effect_size_svy,
                  EDYRS_effect_size_svy, MMSE_effect_size_svy,
                  MCI_effect_size_svy, DEM_effect_size_svy,
                  APOE_effect_size_svy)
)

ggplot() +
  geom_point(data = effect_size_data_svy, aes(y = var, x = stdmeandiff)) +
  geom_vline(xintercept = 0, color = "black") +
  geom_vline(xintercept = -.25, color = "red") +
  geom_vline(xintercept = .25, color = "red")




svy_data <- effect_size_data_svy %>%
  dplyr::mutate(IPW = "No")

ipw_data <- effect_size_ipw_elastic_data %>%
  dplyr::mutate(IPW = "Yes")

plot_data <- rbind(ipw_data, svy_data) %>%
  dplyr::mutate(names = dplyr::case_when(var == "age" ~ "Age (in years)",
                                         var == "mmse" ~ "MMSE",
                                         var == "hispanic" ~ "Hispanic ethnicity",
                                         var == "edyrs" ~ "Years of education",
                                         var == "black" ~ "Black/African American",
                                         var == "APOE" ~ "APOEe4 Allele present",
                                         var == "female" ~ "Female sex",
                                         var == "MCI" ~ "MCI",
                                         var == "dem" ~ "Dementia"))



stdmeandiff_plot_elastic <-
  ggplot(data = plot_data, aes(y = fct_inorder(names), x = stdmeandiff)) +
  geom_vline(xintercept = 0, color = "black") +
  geom_vline(xintercept = -.25, color = "grey", linetype = "longdash") +
  geom_vline(xintercept = .25, color = "grey", linetype = "longdash") +
  geom_line(aes(group = var), color = "black") +
  geom_point(aes(shape = IPW), fill = "white") +
  scale_shape_manual(values = c(21,16)) +
  # scale_color_manual(values = c("snow4", "black")) +
  theme_classic() +
  xlab("Effect size") +
  theme(axis.title.y = element_blank(),
        legend.position = "none")




## Assess overlap of participation

adni_predprob_data <- harmonized_elastic %>%
  dplyr::filter(DATA == "ADNI")

hrs_predprob_data <- harmonized_elastic %>%
  dplyr::filter(DATA == "HRS")


proboverlap_plot <- ggplot() +
  geom_density(data = adni_predprob_data, aes(x = predprob_glmnet)) +
  geom_density(data = hrs_predprob_data, aes(x = predprob_glmnet), linetype = "dashed") +
  theme_classic() +
  theme(legend.position = "none") +
  ylab("Density") +
  scale_x_continuous(name = "Probability", breaks = c(0, 1))

saveRDS(survey_data, here::here("R_objects", "042_survey_data.RDS"))
