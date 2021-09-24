rm(list = setdiff(ls(), lsf.str())[!(setdiff(ls(), lsf.str()) %in% 'params')])
source(here::here('R', '002_folder-paths-and-options.R'))

#---- Package loading ----
if (!require("pacman")){
  install.packages("pacman", repos='http://cran.us.r-project.org')
}

p_load("labelled", "gtsummary", "gt")

#---- Load data ----
harmonized_imp <- readRDS(here::here("R_objects", "040_imp_data.RDS"))

table(harmonized_imp$GENDER, useNA = "ifany")
table(as.numeric(harmonized_imp$GENDER), useNA = "ifany")
table(harmonized_imp$DX, useNA = "ifany")

#---- Table 1 ----
options(
  gtsummary.tbl_summary.percent_fun = function(x) sprintf(x * 100, fmt='%#.2f'))



table_1_df <- harmonized_imp %>%
  select(DATA, AGE, GENDER, EDYRS, RACE_3CAT, ETHNICITY, MMSE, APOE41Y0N, DX) %>%
  mutate(DATA = ifelse(DATA == "HRS", "ADAMS", DATA),
         DX = ifelse(DX == "CN", "Normal", as.character(DX)),
         DX = factor(DX, levels = c("Normal", "MCI", "Dementia")),
         GENDER = as.numeric(GENDER) - 1,
         ETHNICITY = as.numeric(ETHNICITY) - 1,
         APOE41Y0N = as.numeric(APOE41Y0N) - 1
  ) %>%
  drop_unused_value_labels() %>%
  set_variable_labels(
    AGE = "Age",
    DX = "Dementia diagnosis",
    MMSE = "MMSE score",
    GENDER = "Female",
    EDYRS = "Education years",
    ETHNICITY = "Ethnicity: Hispanic/Latino",
    RACE_3CAT = "Race",
    APOE41Y0N = "APOE 4 allele: at least 1",
    DATA = "Dataset"
  ) %>%
  # set_value_labels(
  #   APOE41Y0N = c("Yes" = "1", "No" = "0")) %>%
  modify_if(is.labelled, to_factor) %>%
  tbl_summary(statistic = list(all_categorical() ~ "{n} ({p}%)",
                               all_continuous() ~ "{mean} ({sd})"),
              digits = all_continuous() ~ 2,
              by =  DATA) %>%
  # add_p %>%
  add_overall %>%
  modify_header(label = "") %>%
  modify_spanning_header(starts_with("stat_") ~ "**Dataset**") %>%
  bold_labels()


table_1_df %>% as_flex_table()

writexl::write_xlsx(as_tibble(table_1_df), here::here("tables", "table1.xlsx"))
