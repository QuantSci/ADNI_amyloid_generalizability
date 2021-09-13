rm(list = setdiff(ls(), lsf.str())[!(setdiff(ls(), lsf.str()) %in% 'params')])
source(here::here('R', '002_folder-paths-and-options.R'))

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

## Obtain State Data

enrolltr_01 <- enrolltr %>%
  dplyr::select(RID, USERDATE, STATE) %>%
  dplyr::mutate(RID = as.numeric(RID))


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


## Merging Datasets Together

merge1 <- left_join(adnimerge_02, enrolltr_01, by = c("RID"))


## Creating dataset to do LR to get probability of HRS vs ADNI

adnimerge_03 <- adnimerge %>%
  select(RID, ORIGPROT, VISCODE, EXAMDATE, EXAMDATE.bl, AGE, PTGENDER, PTEDUCAT, PTETHCAT,
         PTRACCAT, APOE4, MMSE, DX, DX.bl) %>%
  filter(VISCODE == "bl") %>%
  rename(EXAMDATE_BL = EXAMDATE.bl,
         DX_BL = DX.bl)

haven::write_dta(adnimerge_03, "adni.dta")
