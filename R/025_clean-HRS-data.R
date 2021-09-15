rm(list = setdiff(ls(), lsf.str())[!(setdiff(ls(), lsf.str()) %in% 'params')])
source(here::here('R', '002_folder-paths-and-options.R'))

hrs <- readRDS(here::here("R_objects", "010_hrs_adams_subsample.RDS"))

hrs_subsample_ids <- hrs %>%
  dplyr::filter(subsample == 1) %>%
  dplyr::select(adamssid)

hrs_data <- readRDS(here::here("R_objects", "020_analysis01.RDS"))

hrs_data_01 <- hrs_data %>%
  dplyr::filter(ADAMSSID %in% hrs_subsample_ids$adamssid) %>%
  dplyr::select(ADAMSSID, AAGE, GENDER, EDYRS, ETHNIC, RACE, APOE41Y0N, demcat4, ANMSETOT)

saveRDS(hrs_data_01, here::here("R_objects", "025_hrs_data_01.RDS"))
