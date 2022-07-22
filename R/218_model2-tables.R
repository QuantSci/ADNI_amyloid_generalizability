rm(list = setdiff(ls(), lsf.str())[!(setdiff(ls(), lsf.str()) %in% 'params')])
source(here::here('R', '002_folder-paths-and-options.R'))
source(here::here('R', '005_libraries.R'))

# Import data

engage_adnimem_results <-    readRDS(here::here("R_objects", "210_engage_adnimem.RDS"))    %>% as.data.frame()
engage_adnief_results <-     readRDS(here::here("R_objects", "210_engage_adnief.RDS"))     %>% as.data.frame()
engage_adas13_results <-   readRDS(here::here("R_objects", "210_engage_adas13.RDS"))       %>% as.data.frame()
donanemab_adnimem_results <- readRDS(here::here("R_objects", "210_donanemab_adnimem.RDS")) %>% as.data.frame()
donanemab_adnief_results <- readRDS(here::here("R_objects", "210_donanemab_adnief.RDS"))   %>% as.data.frame()
donanemab_adas13_results <-  readRDS(here::here("R_objects", "210_donanemab_adas13.RDS"))  %>% as.data.frame()
shouldbe_adnimem_results <-  readRDS(here::here("R_objects", "210_shouldbe_adnimem.RDS"))  %>% as.data.frame()
shouldbe_adnief_results <-   readRDS(here::here("R_objects", "210_shouldbe_adnief.RDS"))   %>% as.data.frame()
shouldbe_adas13_results <-   readRDS(here::here("R_objects", "210_shouldbe_adas13.RDS"))   %>% as.data.frame()

names(engage_adnimem_results) <- sub(".*\\.", "", names(engage_adnimem_results))
names(engage_adnief_results) <- sub(".*\\.", "", names(engage_adnief_results))
names(engage_adas13_results) <- sub(".*\\.", "", names(engage_adas13_results))

names(donanemab_adnimem_results) <- sub(".*\\.", "", names(donanemab_adnimem_results))
names(donanemab_adnief_results) <- sub(".*\\.", "", names(donanemab_adnief_results))
names(donanemab_adas13_results) <- sub(".*\\.", "", names(donanemab_adas13_results))

names(shouldbe_adnimem_results) <- sub(".*\\.", "", names(shouldbe_adnimem_results))
names(shouldbe_adnief_results) <- sub(".*\\.", "", names(shouldbe_adnief_results))
names(shouldbe_adas13_results) <- sub(".*\\.", "", names(shouldbe_adas13_results))

# Functions

pvf <- function(p) {
  # For expressing P values in manuscripts and articles,
  # - If P > .999
  if (p > .999) {
    foo <- paste0("> .99")
  }
  # Else, if P ≥.01 [whether or not P is significant]
  if ((p >= .01) & (p < .999)) {
    # -  The actual value for P should be expressed
    # - Express to 2 digits, unless the 3-digit P-value is between .045 and .049, in which case express in 3 digits.
    foo <- sub("0.",".",paste0(round(p,digits=2)))
    if ((p < .055) &  (p>= .045)) {
      foo <- sub("0.",".",paste0(round(p,digits=3)))
    }
  }
  # - Else, if P < .01 and P > .001
  # - Express to 3 digits.
  if ((p < .01) & (p >= .001)) {
    foo <- sub("0.",".",paste0(round(p,digits=3)))
  }
  # - Else, If P < .001
  if (p < .001) {
    # - use P < .001 (because precise P values with extreme results are sensitive to biases or departures from the statistical model.37 [p198])
    foo <- paste0("< .001")
  }
  foo
}


#####
### Create tables
#####

## ENGAGE CRITERIA

# ADAS13

engage_adas13_results_table <- engage_adas13_results %>%
  filter(op == "~") %>%
  select(lhs, rhs, est, se, pvalue, all) %>%
  rename(Unstandardized = est,
         Standardized = all,
         SE = se,
         p = pvalue) %>%
  mutate(Pathway = dplyr::case_when(lhs == "m24_adas13" & rhs == "bl_adas13" ~ "Baseline cognition to month 24 cognition",
                                    lhs == "m24_adas13" & rhs == "bl_composite" ~ "Baseline amyloid to month 24 cognition",
                                    lhs == "m24_adas13" & rhs == "m24_composite" ~ "Month 24 amyloid to month 24 cognition",
                                    lhs == "bl_adas13"  & rhs == "bl_composite" ~ "Baseline amyloid to baseline cognition",
                                    lhs == "m24_composite" & rhs == "bl_adas13" ~ "Baseline cognition to month 24 amyloid",
                                    lhs == "m24_composite" & rhs == "bl_composite" ~ "Baseline amyloid to month 24 amyloid")) %>%
  mutate_at(c("Unstandardized", "SE", "Standardized"), round, digits = 2) %>%
  mutate_at(c("p"), round, digits = 3) %>%
  select(-lhs, -rhs) %>%
  relocate(Pathway, .before = Unstandardized)

# ADNI-MEM

engage_adnimem_table <- engage_adnimem_results %>%
  filter(op == "~") %>%
  select(lhs, rhs, est, se, pvalue, all) %>%
  rename(Unstandardized = est,
         Standardized = all,
         SE = se,
         p = pvalue) %>%
  mutate(Pathway = dplyr::case_when(lhs == "m24_adni_mem" & rhs == "bl_adni_mem" ~ "Baseline cognition to month 24 cognition",
                                    lhs == "m24_adni_mem" & rhs == "bl_composite" ~ "Baseline amyloid to month 24 cognition",
                                    lhs == "m24_adni_mem" & rhs == "m24_composite" ~ "Month 24 amyloid to month 24 cognition",
                                    lhs == "bl_adni_mem"  & rhs == "bl_composite" ~ "Baseline amyloid to baseline cognition",
                                    lhs == "m24_composite" & rhs == "bl_adni_mem" ~ "Baseline cognition to month 24 amyloid",
                                    lhs == "m24_composite" & rhs == "bl_composite" ~ "Baseline amyloid to month 24 amyloid")) %>%
  mutate_at(c("Unstandardized", "SE", "Standardized"), round, digits = 2) %>%
  mutate_at(c("p"), round, digits = 3) %>%
  select(-lhs, -rhs) %>%
  relocate(Pathway, .before = Unstandardized)

# ADNI-EF

engage_adnief_table <- engage_adnief_results %>%
  filter(op == "~") %>%
  select(lhs, rhs, est, se, pvalue, all) %>%
  rename(Unstandardized = est,
         Standardized = all,
         SE = se,
         p = pvalue) %>%
  mutate(Pathway = dplyr::case_when(lhs == "m24_adni_ef" & rhs == "bl_adni_ef" ~ "Baseline cognition to month 24 cognition",
                                    lhs == "m24_adni_ef" & rhs == "bl_composite" ~ "Baseline amyloid to month 24 cognition",
                                    lhs == "m24_adni_ef" & rhs == "m24_composite" ~ "Month 24 amyloid to month 24 cognition",
                                    lhs == "bl_adni_ef"  & rhs == "bl_composite" ~ "Baseline amyloid to baseline cognition",
                                    lhs == "m24_composite" & rhs == "bl_adni_ef" ~ "Baseline cognition to month 24 amyloid",
                                    lhs == "m24_composite" & rhs == "bl_composite" ~ "Baseline amyloid to month 24 amyloid")) %>%
  mutate_at(c("Unstandardized", "SE", "Standardized"), round, digits = 2) %>%
  mutate_at(c("p"), round, digits = 3) %>%
  select(-lhs, -rhs) %>%
  relocate(Pathway, .before = Unstandardized)

## DONANEMAB CRITERIA

# ADAS13

donanemab_adas13_results_table <- donanemab_adas13_results %>%
  filter(op == "~") %>%
  select(lhs, rhs, est, se, pvalue, all) %>%
  rename(Unstandardized = est,
         Standardized = all,
         SE = se,
         p = pvalue) %>%
  mutate(Pathway = dplyr::case_when(lhs == "m24_adas13" & rhs == "bl_adas13" ~ "Baseline cognition to month 24 cognition",
                                    lhs == "m24_adas13" & rhs == "bl_composite" ~ "Baseline amyloid to month 24 cognition",
                                    lhs == "m24_adas13" & rhs == "m24_composite" ~ "Month 24 amyloid to month 24 cognition",
                                    lhs == "bl_adas13"  & rhs == "bl_composite" ~ "Baseline amyloid to baseline cognition",
                                    lhs == "m24_composite" & rhs == "bl_adas13" ~ "Baseline cognition to month 24 amyloid",
                                    lhs == "m24_composite" & rhs == "bl_composite" ~ "Baseline amyloid to month 24 amyloid")) %>%
  mutate_at(c("Unstandardized", "SE", "Standardized"), round, digits = 2) %>%
  mutate_at(c("p"), round, digits = 3) %>%
  select(-lhs, -rhs) %>%
  relocate(Pathway, .before = Unstandardized)

# ADNI-MEM

donanemab_adnimem_table <- donanemab_adnimem_results %>%
  filter(op == "~") %>%
  select(lhs, rhs, est, se, pvalue, all) %>%
  rename(Unstandardized = est,
         Standardized = all,
         SE = se,
         p = pvalue) %>%
  mutate(Pathway = dplyr::case_when(lhs == "m24_adni_mem" & rhs == "bl_adni_mem" ~ "Baseline cognition to month 24 cognition",
                                    lhs == "m24_adni_mem" & rhs == "bl_composite" ~ "Baseline amyloid to month 24 cognition",
                                    lhs == "m24_adni_mem" & rhs == "m24_composite" ~ "Month 24 amyloid to month 24 cognition",
                                    lhs == "bl_adni_mem"  & rhs == "bl_composite" ~ "Baseline amyloid to baseline cognition",
                                    lhs == "m24_composite" & rhs == "bl_adni_mem" ~ "Baseline cognition to month 24 amyloid",
                                    lhs == "m24_composite" & rhs == "bl_composite" ~ "Baseline amyloid to month 24 amyloid")) %>%
  mutate_at(c("Unstandardized", "SE", "Standardized"), round, digits = 2) %>%
  mutate_at(c("p"), round, digits = 3) %>%
  select(-lhs, -rhs) %>%
  relocate(Pathway, .before = Unstandardized)

# ADNI-EF

donanemab_adnief_table <- donanemab_adnief_results %>%
  filter(op == "~") %>%
  select(lhs, rhs, est, se, pvalue, all) %>%
  rename(Unstandardized = est,
         Standardized = all,
         SE = se,
         p = pvalue) %>%
  mutate(Pathway = dplyr::case_when(lhs == "m24_adni_ef" & rhs == "bl_adni_ef" ~ "Baseline cognition to month 24 cognition",
                                    lhs == "m24_adni_ef" & rhs == "bl_composite" ~ "Baseline amyloid to month 24 cognition",
                                    lhs == "m24_adni_ef" & rhs == "m24_composite" ~ "Month 24 amyloid to month 24 cognition",
                                    lhs == "bl_adni_ef"  & rhs == "bl_composite" ~ "Baseline amyloid to baseline cognition",
                                    lhs == "m24_composite" & rhs == "bl_adni_ef" ~ "Baseline cognition to month 24 amyloid",
                                    lhs == "m24_composite" & rhs == "bl_composite" ~ "Baseline amyloid to month 24 amyloid")) %>%
  mutate_at(c("Unstandardized", "SE", "Standardized"), round, digits = 2) %>%
  mutate_at(c("p"), round, digits = 3) %>%
  select(-lhs, -rhs) %>%
  relocate(Pathway, .before = Unstandardized)

## SHOULD BE CRITERIA

# ADAS13

shouldbe_adas13_results_table <- shouldbe_adas13_results %>%
  filter(op == "~") %>%
  select(lhs, rhs, est, se, pvalue, all) %>%
  rename(Unstandardized = est,
         Standardized = all,
         SE = se,
         p = pvalue) %>%
  mutate(Pathway = dplyr::case_when(lhs == "m24_adas13" & rhs == "bl_adas13" ~ "Baseline cognition to month 24 cognition",
                                    lhs == "m24_adas13" & rhs == "bl_composite" ~ "Baseline amyloid to month 24 cognition",
                                    lhs == "m24_adas13" & rhs == "m24_composite" ~ "Month 24 amyloid to month 24 cognition",
                                    lhs == "bl_adas13"  & rhs == "bl_composite" ~ "Baseline amyloid to baseline cognition",
                                    lhs == "m24_composite" & rhs == "bl_adas13" ~ "Baseline cognition to month 24 amyloid",
                                    lhs == "m24_composite" & rhs == "bl_composite" ~ "Baseline amyloid to month 24 amyloid")) %>%
  mutate_at(c("Unstandardized", "SE", "Standardized"), round, digits = 2) %>%
  mutate_at(c("p"), round, digits = 3) %>%
  select(-lhs, -rhs) %>%
  relocate(Pathway, .before = Unstandardized)

# ADNI-MEM

shouldbe_adnimem_table <- shouldbe_adnimem_results %>%
  filter(op == "~") %>%
  select(lhs, rhs, est, se, pvalue, all) %>%
  rename(Unstandardized = est,
         Standardized = all,
         SE = se,
         p = pvalue) %>%
  mutate(Pathway = dplyr::case_when(lhs == "m24_adni_mem" & rhs == "bl_adni_mem" ~ "Baseline cognition to month 24 cognition",
                                    lhs == "m24_adni_mem" & rhs == "bl_composite" ~ "Baseline amyloid to month 24 cognition",
                                    lhs == "m24_adni_mem" & rhs == "m24_composite" ~ "Month 24 amyloid to month 24 cognition",
                                    lhs == "bl_adni_mem"  & rhs == "bl_composite" ~ "Baseline amyloid to baseline cognition",
                                    lhs == "m24_composite" & rhs == "bl_adni_mem" ~ "Baseline cognition to month 24 amyloid",
                                    lhs == "m24_composite" & rhs == "bl_composite" ~ "Baseline amyloid to month 24 amyloid")) %>%
  mutate_at(c("Unstandardized", "SE", "Standardized"), round, digits = 2) %>%
  mutate_at(c("p"), round, digits = 3) %>%
  select(-lhs, -rhs) %>%
  relocate(Pathway, .before = Unstandardized)

# ADNI-EF

shouldbe_adnief_table <- shouldbe_adnief_results %>%
  filter(op == "~") %>%
  select(lhs, rhs, est, se, pvalue, all) %>%
  rename(Unstandardized = est,
         Standardized = all,
         SE = se,
         p = pvalue) %>%
  mutate(Pathway = dplyr::case_when(lhs == "m24_adni_ef" & rhs == "bl_adni_ef" ~ "Baseline cognition to month 24 cognition",
                                    lhs == "m24_adni_ef" & rhs == "bl_composite" ~ "Baseline amyloid to month 24 cognition",
                                    lhs == "m24_adni_ef" & rhs == "m24_composite" ~ "Month 24 amyloid to month 24 cognition",
                                    lhs == "bl_adni_ef"  & rhs == "bl_composite" ~ "Baseline amyloid to baseline cognition",
                                    lhs == "m24_composite" & rhs == "bl_adni_ef" ~ "Baseline cognition to month 24 amyloid",
                                    lhs == "m24_composite" & rhs == "bl_composite" ~ "Baseline amyloid to month 24 amyloid")) %>%
  mutate_at(c("Unstandardized", "SE", "Standardized"), round, digits = 2) %>%
  mutate_at(c("p"), round, digits = 3) %>%
  select(-lhs, -rhs) %>%
  relocate(Pathway, .before = Unstandardized)


# Merge results by criteria

engage_results_table <- rbind(
  "ADAS-13", engage_adas13_results_table,
  "ADNI-MEM", engage_adnimem_table,
  "ADNI-EF", engage_adnief_table
) %>%
  gt::gt(rownames_to_stub = TRUE) %>%
  gt::gtsave("engage_results_table.rtf")

donanemab_results_table <- rbind(
  "ADAS-13", donanemab_adas13_results_table,
  "ADNI-MEM", donanemab_adnimem_table,
  "ADNI-EF", donanemab_adnief_table
) %>%
  gt::gt(rownames_to_stub = TRUE) %>%
  gt::gtsave("donanemab_results_table.rtf")

shouldbe_results_table <- rbind(
  "ADAS-13", shouldbe_adas13_results_table,
  "ADNI-MEM", shouldbe_adnimem_table,
  "ADNI-EF", shouldbe_adnief_table
) %>%
  gt::gt(rownames_to_stub = TRUE) %>%
  gt::gtsave("shouldbe_results_table.rtf")
