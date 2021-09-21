rm(list = setdiff(ls(), lsf.str())[!(setdiff(ls(), lsf.str()) %in% 'params')])
source(here::here('R', '002_folder-paths-and-options.R'))

# Load Data

adni <- readRDS(here::here("R_objects", "020_adnimerge_03.RDS")) %>%
  dplyr::select(RID, AGE, PTGENDER, PTEDUCAT, PTETHCAT, PTRACCAT, APOE4, DX, MMSE)
# Retaining only variables to harmonize in adni

hrs <- readRDS(here::here("R_objects", "025_hrs_data_02.RDS"))

# Harmonize ID (this is just renaming the ID variables to be consistent)

adni_01 <- adni %>%
  rename(ID = RID)

hrs_01 <- hrs %>%
  rename(ID = ADAMSSID)

# Harmonize Age
# In ADNI, age goes out to one decimal place. HRS age is rounded to nearest whole number. Rounded ADNI to be consistent.

adni_02 <- adni_01 %>%
  dplyr::mutate(AGE = floor(AGE)) %>%
  dplyr::filter(AGE > 69)

hrs_02 <- hrs_01 %>%
  rename(AGE = AAGE) # giving similar variable name

# Harmonize Gender
# In ADNI, 1 = MALE; 2 = FEMALE
# In HRS, 1 = MALE; 2 = FEMALE
# Data are labeled in ADNI, not labeled in HRS

adni_03 <- adni_02 %>%
  dplyr::mutate(GENDER = dplyr::if_else(PTGENDER == "Male", 1, 2)) %>% # Removing labels and renaming in same step
  dplyr::select(-PTGENDER) # Dropping variable


# Harmonize Education
# In ADNI, in years - 16 = BA/BS, 18 = MA/MS, 20 = PHD/MD
# In HRS, in years - 16 = BA/BS, 17 = beyond college

table(hrs_02$EDYRS)
#   6   7   8   9  10  11  12  13  14  15  16  17
#  12  17  53  20  40  24 112  27  36  11  22  34

table(adni_03$PTEDUCAT)
# 4   6   7   8   9  10  11  12  13  14  15  16  17  18  19  20
# 2   3   6  12   7  23  17 274  96 257  92 566 105 455 109 311

adni_04 <- adni_03 %>%
  dplyr::mutate(EDYRS = dplyr::if_else(PTEDUCAT > 17, 17, as.numeric(PTEDUCAT))) %>%  #Renaming and recoding in same step
  dplyr::select(-PTEDUCAT)

# Harmonize Ethnicity
# In ADNI, 1 = Hispanic/Latinx, 2 = Not Hispanic/Latinx, 3 = Unknown
# In HRS, 3 = Hispanic, 1 = White/Non Hispanic, 2 = Black Non-Hispanic

table(hrs_02$ETHNIC)
# 1   2   3
# 325  65  18

table(adni_04$PTETHCAT)
# Unknown Not Hisp/Latino     Hisp/Latino
# 12            2223             100

hrs_03 <- hrs_02 %>%
  dplyr::mutate(ETHNICITY = dplyr::if_else(ETHNIC == 3, 2, 1)) %>%   # If ethnic is 3, hispanic, otherwise non-hispanic
  dplyr::select(-ETHNIC)

adni_05 <- adni_04 %>%
  dplyr::mutate(ETHNICITY = dplyr::case_when(PTETHCAT == "Unknown" ~ NA_real_,
                                             PTETHCAT == "Hisp/Latino" ~ 2,
                                             PTETHCAT == "Not Hisp/Latino" ~ 1)) %>%
  dplyr::select(-PTETHCAT)


# Harmonize Race
# In ADNI, 1= American Indian/Alaska Native; 2 = Asian, 3 = Native Hawaiian or Other Pacific Islander, 4 = Black,
# 5 = White, 6 = Multiracial, 7 = Unknown
# IN HRS, 0 = Not Obtained, 1 = White, 2 = Black, 7 = Other

table(hrs_03$RACE)

#   1   2   7
# 335  67   6

table(adni_05$PTRACCAT)

# Am Indian/Alaskan             Asian Hawaiian/Other PI             Black             White     More than one           Unknown
#                 5                51                 2               131              2113                27                 6

hrs_04 <- hrs_03 %>%
  dplyr::mutate(RACE_3CAT = dplyr::case_when(RACE == 1 ~ 1,
                                             RACE == 2 ~ 2,
                                             RACE == 7 ~ 3)) %>%
  dplyr::select(-RACE)


adni_06 <- adni_05 %>%
  dplyr::mutate(RACE_3CAT = dplyr::case_when(PTRACCAT == "White" ~ 1,
                                             PTRACCAT == "Black" ~ 2,
                                             PTRACCAT == "Unknown" ~ NA_real_,
                                             TRUE ~ 3)) %>%
  dplyr::select(-PTRACCAT)

# Add Labels

hrs_04$RACE_3CAT <- factor(hrs_04$RACE_3CAT, levels = c(1, 2, 3),
                           labels = c("White", "Black", "Other"))

adni_06$RACE_3CAT <- factor(adni_06$RACE_3CAT, levels = c(1, 2, 3),
                           labels = c("White", "Black", "Other"))

# Harmonize APOE
# In ADNI, 2 = 2 APOE4 alleles, 1 = 1 APOE4 allele, 0 = None
# In HRS, 1 = at least one APOE4 allele, 0 = None

adni_07 <- adni_06 %>%
  dplyr::mutate(APOE41Y0N = dplyr::if_else(APOE4 > 0, 1, 0)) %>%
  dplyr::select(-APOE4)

# Harmonize DX
# In ADNI, 1  = CN, 2 = MCI, 3 = AD
# In HRS, 1 = AD, 2 = MCI, 3 = CN

hrs_05 <- hrs_04 %>%
  dplyr::mutate(DX = dplyr::case_when(dxcat == 3 ~ 1, #just flipping 1 and 3 to be consistent with ADNI
                                      dxcat == 1 ~ 3,
                                      TRUE ~ 2)) %>%
  dplyr::select(-dxcat)

hrs_05$DX <- factor(hrs_05$DX, levels = c(1, 2, 3),
                    labels = c("CN", "MCI", "Dementia"))

# Harmonize MMSE
hrs_06 <- hrs_05 %>%
  dplyr::rename(MMSE = ANMSETOT)

# Add indicator for dataset

adni_08 <- adni_07 %>%
  dplyr::mutate(DATA = "ADNI",
                WEIGHT = 1)


# This line of code gets the HRS sample size and sum of weights, used for rescaling the weights below
hrs_sample_size <- nrow(hrs)

sum_weights_hrs <- hrs %>%
  dplyr::summarize(sum = sum(AASAMPWT_F)) %>%
  pull()


hrs_07 <- hrs_06 %>%
  dplyr::mutate(DATA = "HRS") %>%
  dplyr::rename(WEIGHT = AASAMPWT_F) %>%
  dplyr::mutate(NEW_WEIGHT = (hrs_sample_size/sum_weights_hrs) * WEIGHT) %>%  # scaling weights
  dplyr::select(-WEIGHT) %>%  # drop old weight variable
  dplyr::rename(WEIGHT = NEW_WEIGHT) # renaming new weight variable to WEIGHT for rbind, below

hrs_07 %>%
  dplyr::summarise(sum = sum(WEIGHT)) %>%
  pull() # should total to HRS sample size

adni_sample_size <- nrow(adni_08) # remember that we drop participants younger than 70, so cannot use original adni data

sum_weights_adni <- adni_08 %>%
  dplyr::summarize(sum = sum(WEIGHT)) %>%
  pull()


adni_09 <- adni_08 %>%
  dplyr::mutate(NEW_WEIGHT = adni_sample_size * (WEIGHT / sum_weights_adni)) %>%  # scaling weights
  dplyr::select(-WEIGHT) %>%  # drop old weight variable
  dplyr::rename(WEIGHT = NEW_WEIGHT) # renaming new weight variable to WEIGHT for rbind, below



# Merge data

merged <- rbind(adni_09, hrs_07)


# Imputing missing data

merged$APOE41Y0N <- as.factor(merged$APOE41Y0N)
merged$GENDER <- as.factor(merged$GENDER)
merged$ETHNICITY <- as.factor(merged$ETHNICITY)

# One participant in ADNI had age = 0, marking as missing and imputing

merged <- merged %>%
  dplyr::mutate(AGE = dplyr::if_else(ID == 6884 & DATA == "ADNI", NA_real_, as.numeric(AGE)))

set.seed(09042016)

imputed <- mice::mice(merged, m = 1)

imp_data <- complete(imputed, 1)

saveRDS(imp_data, here::here("R_objects", "040_imp_data.RDS"))
