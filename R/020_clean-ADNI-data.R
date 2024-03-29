rm(list = setdiff(ls(), lsf.str())[!(setdiff(ls(), lsf.str()) %in% 'params')])
source(here::here('R', '002_folder-paths-and-options.R'))
source(here::here('R', '005_libraries.R'))

## Variables needed:
# Abeta, Which PET tracer, scanner strength, cognition (adascog), age, sex, education, race/ethnicity, ApoE, Diagnosis group

# Using ADNIMERGE to get Abeta, adascog, age, sex, education, race/ethnicity, APOE4, diagnosis group, site, and scanner strength


adnimerge_01 <- adnimerge %>%
  dplyr::select(RID, ORIGPROT, COLPROT, VISCODE, EXAMDATE, EXAMDATE.bl, AV45, ABETA, ABETA.bl, AGE, PTGENDER, PTEDUCAT, PTETHCAT,
                PTRACCAT, APOE4, MMSE, ADAS11, ADAS13, DX, FLDSTRENG, FLDSTRENG.bl, SITE) %>%
  dplyr::mutate(RID = as.numeric(RID),
                MRI_STR_CHANGE = dplyr::case_when(FLDSTRENG.bl == "1.5 Tesla MRI" & FLDSTRENG == "1.5 Tesla MRI" ~ 0,
                                                  FLDSTRENG.bl == "3 Tesla MRI" & FLDSTRENG == "3 Tesla MRI" ~ 0,
                                                  FLDSTRENG.bl == "1.5 Tesla MRI" & FLDSTRENG == "3 Tesla MRI" ~ 1,
                                                  FLDSTRENG.bl == "3 Tesla MRI" & FLDSTRENG == "1.5 Tesla MRI" ~ 2))

# Calculate days since baseline visit

adnimerge_02 <- adnimerge_01 %>%
  dplyr::mutate(baseline_date = ymd(EXAMDATE.bl),
                exam_date = ymd(EXAMDATE)) %>%
  dplyr::group_by(RID) %>%
  dplyr::mutate(days_since_baseline = exam_date - baseline_date)

## Identify participants with a change in MRI scanner strength

table(adnimerge_02$FLDSTRENG, adnimerge_02$FLDSTRENG.bl)

scanner_change_ids <- adnimerge_02 %>%
  select(RID, MRI_STR_CHANGE) %>%
  filter(MRI_STR_CHANGE == 1) %>%
  distinct(RID)


## NEXT NEED TO GET PET ABETA PET

av45_01 <- ucberkeleyav45 %>%
  dplyr::select(RID, ORIGPROT, EXAMDATE, VISCODE, SUMMARYSUVR_WHOLECEREBNORM, SUMMARYSUVR_COMPOSITE_REFNORM) %>%
  dplyr::mutate(TRACER = "FBP")

fbb_01 <- ucberkeleyfbb %>%
  dplyr::select(RID, ORIGPROT, EXAMDATE, VISCODE, SUMMARYSUVR_WHOLECEREBNORM, SUMMARYSUVR_COMPOSITE_REFNORM) %>%
  dplyr::mutate(TRACER = "FBB")

av45_02 <- sjlabelled::remove_all_labels(av45_01)

fbb_02 <- sjlabelled::remove_all_labels(fbb_01)

amy_pet_01 <- bind_rows(av45_02, fbb_02) %>%
  dplyr::mutate(EXAMDATE = as_date(EXAMDATE))

## Now the goal is to get just a simple change score for Oct 1 analyses

## Get the IDs used in analysis:
adni_ids <- readRDS(here::here("R_objects", "041_adni_IDs.RDS"))

# get PET data

amy_pet_02 <- amy_pet_01 %>%
  dplyr::filter(RID %in% adni_ids$RID)

table(amy_pet_02$VISCODE)

#     bl m108  m12 m120 m132 m144 m156 m168  m24  m36  m48  m60  m72  m84  m96
# 1  824   34    3   26   33   13   18    4  396   17  257  107  137   77   76

# going to filter for just bl and m24 for ease

amy_pet_test <- amy_pet_02 %>%
  dplyr::filter(VISCODE == "bl" | VISCODE == "m24" | VISCODE == "m48")

# Convert to CLs

amy_pet_03 <- amy_pet_test %>%
  dplyr::mutate(CL_CEREBNORM = dplyr::if_else(TRACER == "FBB", 157.15*SUMMARYSUVR_WHOLECEREBNORM - 151.87,
                                              188.22*SUMMARYSUVR_WHOLECEREBNORM - 189.16),
                CL_COMPOSITE = dplyr::if_else(TRACER == "FBB", 244.20 * SUMMARYSUVR_COMPOSITE_REFNORM - 170.8,
                                              300.66 * SUMMARYSUVR_COMPOSITE_REFNORM - 208.84))

# go long to wide

amy_pet_03_wholecerebnorm <- amy_pet_03 %>%
  pivot_wider(id_cols = RID, names_from = VISCODE, values_from = CL_CEREBNORM) %>%
  rename(bl_wholecereb = bl,
         m24_wholecereb = m24,
         m48_wholecereb = m48) %>%
  arrange(RID)

amy_pet_03_composite <- amy_pet_03 %>%
  pivot_wider(id_cols = RID, names_from = VISCODE, values_from = CL_COMPOSITE) %>%
  rename(bl_composite = bl,
         m24_composite = m24,
         m48_composite = m48)  %>%
  arrange(RID)

amy_pet_03_examdate <- amy_pet_03 %>%
  pivot_wider(id_cols = RID, names_from = VISCODE, values_from = EXAMDATE) %>%
  rename(bl_examdate = bl,
         m24_examdate = m24,
         m48_examdate = m48)  %>%
  arrange(RID)

amy_pet_03_merge1 <- left_join(amy_pet_03_composite, amy_pet_03_wholecerebnorm, by = c("RID"))
amy_pet_03 <- left_join(amy_pet_03_merge1, amy_pet_03_examdate, by = c("RID"))

#there's problesm with VISCODE which we used to get bl and m24. let's do a quick
#check to make sure the exam date difference is around 2 years

amy_pet_04 <- amy_pet_03 %>%
  dplyr::mutate(bl_examdate = lubridate::ymd(bl_examdate),
                m24_examdate = lubridate::ymd(m24_examdate),
                diff_examdate = m24_examdate - bl_examdate,
                diff_examdate = as.numeric(diff_examdate) / 365) #divide by 365 bc we get
                                                                 #unit of days as default

psych::describe(amy_pet_04$diff_examdate)
#    vars   n mean   sd median trimmed  mad  min  max range skew kurtosis se
# X1    1 697 2.03 0.13   2.01    2.02 0.06 1.43 2.63   1.2 1.04     5.21  0

# looks pretty good
# can save out this data for use down the line

# Now, get cog data at bl and m24

cog_data <- adnimerge_01 %>%
  select(RID, VISCODE, EXAMDATE, ADAS11, ADAS13) %>%
  dplyr::filter(RID %in% adni_ids$RID) %>%
  dplyr::filter(VISCODE == "bl" | VISCODE == "m24" | VISCODE == "m48")

# check that exam date difference is still around 2

cog_data_examdate <-  cog_data %>%
  pivot_wider(id_cols = RID, names_from = VISCODE, values_from = EXAMDATE) %>%
  rename(bl_examdate = bl,
         m24_examdate = m24,
         m48_examdate = m48) %>%
  arrange(RID) %>%
  dplyr::mutate(bl_examdate = lubridate::ymd(bl_examdate),
               m24_examdate = lubridate::ymd(m24_examdate),
               diff_examdate = m24_examdate - bl_examdate,
               diff_examdate = as.numeric(diff_examdate) / 365) #divide by 365 again for years
#unit of days as default

psych::describe(cog_data_examdate$diff_examdate)
#    vars    n mean  sd median trimmed  mad  min  max range skew kurtosis se
# X1    1 1070 2.03 0.1   2.01    2.02 0.03 1.48 3.28  1.81 4.72    43.53  0

# a little worse. average is still 2 but range is up to 3.3.

cog_data_adas11 <- cog_data %>%
  pivot_wider(id_cols = RID, names_from = VISCODE, values_from = ADAS11) %>%
  rename(bl_adas11 = bl,
         m24_adas11 = m24,
         m48_adas11 = m48) %>%
  arrange(RID)

cog_data_adas13 <- cog_data %>%
  pivot_wider(id_cols = RID, names_from = VISCODE, values_from = ADAS13) %>%
  rename(bl_adas13 = bl,
         m24_adas13 = m24,
         m48_adas13 = m48) %>%
  arrange(RID)

cog_data_03 <- left_join(cog_data_adas11, cog_data_adas13, by = c("RID"))

## Get ADNI_EF and ADNI_MEM data from uwnpsychsum

uwnpsychsum_01 <- uwnpsychsum %>%
  select(RID, ORIGPROT, VISCODE, EXAMDATE, ADNI_MEM, ADNI_EF) %>%
  dplyr::filter(RID %in% adni_ids$RID) %>%
  dplyr::filter(VISCODE == "bl" | VISCODE == "m24" | VISCODE == "m48") %>%
  dplyr::select(-EXAMDATE, -ORIGPROT)

uwnpsychsum_adni_mem <- uwnpsychsum_01 %>%
  pivot_wider(id_cols = RID, names_from = VISCODE, values_from = ADNI_MEM) %>%
  rename(bl_adni_mem = bl,
         m24_adni_mem = m24,
         m48_adni_mem = m48) %>%
  arrange(RID)

uwnpsychsum_adni_ef <- uwnpsychsum_01 %>%
  pivot_wider(id_cols = RID, names_from = VISCODE, values_from = ADNI_EF) %>%
  rename(bl_adni_ef = bl,
         m24_adni_ef = m24,
         m48_adni_ef = m48) %>%
  arrange(RID)

uw_cog_data <- left_join(uwnpsychsum_adni_mem, uwnpsychsum_adni_ef, by = "RID")

## Creating dataset to do LR to get probability of HRS vs ADNI

adnimerge_03 <- adnimerge %>%
  select(RID, ORIGPROT, VISCODE, EXAMDATE, EXAMDATE.bl, AGE, PTGENDER, PTEDUCAT, PTETHCAT,
         PTRACCAT, APOE4, MMSE, DX, DX.bl) %>%
  filter(VISCODE == "bl") %>%
  rename(EXAMDATE_BL = EXAMDATE.bl,
         DX_BL = DX.bl)

haven::write_dta(adnimerge_03, "adni.dta")

saveRDS(adnimerge_03, here::here("R_objects", "020_adnimerge_03.RDS"))
saveRDS(amy_pet_04, here::here("R_objects", "020_amy_pet_04.RDS"))
saveRDS(cog_data_03, here::here("R_objects", "020_cog_data_03.RDS"))
saveRDS(uw_cog_data, here::here("R_objects", "020_uw_cog_data.RDS"))
