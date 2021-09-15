

# Import ADNI data here
# If we want to use the data prepared for the conference
adni_box_data <- haven::read_dta(fs::path(box_adni_path, "ADNIPsyMCA_210813.dta"))


adnimerge <- ADNIMERGE::adnimerge %>%
  as_tibble() %>%
  select(RID, COLPROT, ORIGPROT, VISCODE, M, EXAMDATE, Years.bl, Month.bl, everything())

# Some R functions (mostly tidyverse) don't like labeled data
# When removing the labels some aspects of the data got lost, and am adding those back in
adnimerge_nolabel <- sjlabelled::remove_all_labels(adnimerge) %>%
  mutate(EXAMDATE = lubridate::as_date(EXAMDATE),
         ORIGPROT = factor(ORIGPROT, levels = c(1, 2, 3, 4),
                           labels = c("ADNI1", "ADNIGO", "ADNI2", "ADNI3"),
                           ordered=TRUE))

ucberkeleyav1451 <- ADNIMERGE::ucberkeleyav1451 %>%
  as_tibble()

# Import HRS data here
hrs_adams_subsample <- haven::read_dta("R:/BM_QuantitativeSciencesPrg/STUDIES/HRSADAMS/data/hrs_adams_subsample.dta")


# Save the R object
saveRDS(adnimerge, here::here("R_objects", "010_adnimerge.RDS"))
saveRDS(adnimerge_nolabel, here::here("R_objects", "010_adnimerge_nolabel.RDS"))
saveRDS(ucberkeleyav1451, here::here("R_objects", "010_aucberkeleyav1451.RDS"))
saveRDS(hrs_adams_subsample, here::here("R_objects", "010_hrs_adams_subsample.RDS"))

