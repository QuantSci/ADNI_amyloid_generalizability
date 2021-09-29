rm(list = setdiff(ls(), lsf.str())[!(setdiff(ls(), lsf.str()) %in% 'params')])
source(here::here('R', '002_folder-paths-and-options.R'))

# import data

survey <- readRDS(here::here("R_objects", "041_survey_data.RDS"))
adams <- readRDS(here::here("R_objects", "025_hrs_data_02.RDS"))

# split into ADAMS and ADNI

adams_data <- survey %>%
  dplyr::filter(DATA == "HRS") %>%
  mutate(AGE_C = AGE - 70,
         ID = as.numeric(ID))

adni_data <- survey %>%
  dplyr::filter(DATA == "ADNI") %>%
  mutate(AGE_C = AGE - 70)

# there has been an issue with the re-scaled weights used in ADAMS. here, I obtain just the ids used in our analyses
# so I can pull in the original ADAMS sampling weights

adams_ids <- adams_data %>%
  select(ID)

adams_weights <- adams %>%
  select(ADAMSSID, AASAMPWT_F) %>%
  filter(ADAMSSID %in% adams_ids$ID) %>%
  rename(ID = ADAMSSID) %>%
  mutate(ID = as.numeric(ID))

adams_data_01 <- left_join(adams_data, adams_weights, by = "ID")


# Get MMSE/AGE in ADAMS

adams_svy <- survey::svydesign(data = adams_data_01,
                       ids = ~ID,
                       weights = ~AASAMPWT_F)

adams_age_cog <- svyglm(MMSE ~ AGE_C, design = adams_svy)

summary(adams_age_cog)

# MMSE/AGE in ADNI (unweighted)

adni_age_cog_un <- lm(MMSE ~ AGE_C, data = adni_data)

summary(adni_age_cog_un)

# MMSE/AGE in ADNI (weighted)

adni_svy <- survey::svydesign(data = adni_data,
                      ids = ~ID,
                      weights = ~scaled_weights)

adni_age_cog <- svyglm(MMSE ~ AGE_C, design = adni_svy)


summary(adni_age_cog)

## Get distribution of APOE4 in ADAMS

survey::svytable(~APOE41Y0N, design = adams_svy)

# APOE41Y0N
# 0         1
# 38582.16 110091.38

(110091.38 / (110091.38 + 38582.16)) * 100
74

## get distribution of APOE4 in ADNI (unweighted)

table(adni_data$APOE41Y0N)

# 0   1
# 880 684

(684 / (684 + 880)) * 100

44

## get distribution of APOE4 in ADNI (weighted)

svytable(~APOE41Y0N, design= adni_svy)

# APOE41Y0N
# 0        1
# 277.9765 107.2111

(107.2111 / (277.9765 + 107.2111)) * 100

28
