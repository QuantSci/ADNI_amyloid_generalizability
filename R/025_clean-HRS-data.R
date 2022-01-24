rm(list = setdiff(ls(), lsf.str())[!(setdiff(ls(), lsf.str()) %in% 'params')])
source(here::here('R', '002_folder-paths-and-options.R'))

hrs <- readRDS(here::here("R_objects", "010_hrs_adams_subsample.RDS"))

hrs_subsample_ids <- hrs %>%
  dplyr::filter(subsample == 1) %>%
  dplyr::select(adamssid, dxcat) %>%
  dplyr::rename(ADAMSSID = adamssid)

hrs_data <- readRDS(here::here("R_objects", "020_analysis01.RDS"))

hrs_data_01 <- hrs_data %>%
  dplyr::filter(ADAMSSID %in% hrs_subsample_ids$ADAMSSID) %>%
  dplyr::select(ADAMSSID, AAGE, GENDER, EDYRS, ETHNIC, RACE, APOE41Y0N, ANMSETOT, AASAMPWT_F)

hrs_data_02 <- left_join(hrs_data_01, hrs_subsample_ids, by = "ADAMSSID")

saveRDS(hrs_data_02, here::here("R_objects", "025_hrs_data_02.RDS"))


hrs_svy <- hrs_data %>%
  srvyr::as_survey_design(ids = ADAMSSID, weight = AASAMPWT_F)

hrs_svy %>%
  group_by(ADAPOE) %>%
  summarize(prop = survey_mean())

hrs_svy_01 <- hrs_data_01 %>%
  srvyr::as_survey_design(ids = ADAMSSID, weight = AASAMPWT_F)

hrs_svy_01 %>%
  group_by(APOE41Y0N) %>%
  summarize(prop = survey_mean())
