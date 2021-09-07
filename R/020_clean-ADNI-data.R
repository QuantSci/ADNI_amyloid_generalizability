rm(list = setdiff(ls(), lsf.str())[!(setdiff(ls(), lsf.str()) %in% 'params')])
source(here::here('R', '002_folder-paths-and-options.R'))

## Variables needed:
# Abeta, Which PET tracer, scanner strength, cognition (adascog), age, sex, education, race/ethnicity, ApoE, Diagnosis group

# Using ADNIMERGE to get Abeta, adascog, age, sex, education, race/ethnicity, APOE4, and diagnosis group


adnimerge_01 <- adnimerge %>%
  dplyr::select(RID, ORIGPROT, VISCODE, AV45, ABETA, AGE, PTGENDER, PTEDUCAT, PTETHCAT,
                PTRACCAT, APOE4, MMSE, ADAS11, ADAS13, DX) %>%
  dplyr::mutate(RID = as.numeric(RID),
                ORIGPROT = as.character(ORIGPROT),
                VISCODE = as.character(VISCODE))


# Using MRIB1CALIB to get scanner strength

mri_01 <- mrib1calib %>%
  dplyr::select(RID, ORIGPROT, VISCODE, FLDSTRENG) %>%
  dplyr::mutate(RID = as.numeric(RID),
                ORIGPROT = as.character(ORIGPROT),
                VISCODE = as.character(VISCODE))

# Using amymeta to get tracer type

amymeta_01 <- amymeta %>%
  dplyr::select(RID, ORIGPROT, VISCODE, TRACERTYPE) %>%
  dplyr::mutate(RID = as.numeric(RID),
                ORIGPROT = as.character(ORIGPROT),
                VISCODE = as.character(VISCODE))

merge1 <- left_join(adnimerge_01, mri_01, by = c("RID", "ORIGPROT", "VISCODE"))
adnidata_01 <- left_join(merge1, amymeta_01, by = c("RID", "ORIGPROT", "VISCODE"))
