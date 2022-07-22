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


# Pull results

# beta (standardized estimate)

## ENGAGE - ADNIMEM

engage_adnimembaseline_to_adnimemm24.beta <- engage_adnimem_results %>%
  filter(lhs == "m24_adni_mem", rhs == "bl_adni_mem") %>%
  select(all) %>%
  pull() %>%
  round(2)

engage_amyloidbaseline_to_adnimem24.beta <- engage_adnimem_results %>%
  filter(lhs == "m24_adni_mem", rhs == "bl_composite") %>%
  select(all) %>%
  pull() %>%
  round(2)

engage_adnimembaseline_to_amyloidm24.beta <- engage_adnimem_results %>%
  filter(lhs == "m24_composite", rhs == "bl_adni_mem") %>%
  select(all) %>%
  pull() %>%
  round(2)

engage_amyloidbaseline_to_amyloidm24.beta <- engage_adnimem_results %>%
  filter(lhs == "m24_composite", rhs == "bl_composite") %>%
  select(all) %>%
  pull %>%
  round(2)

engage_amyloidm24_to_adnimemm24.beta <- engage_adnimem_results %>%
  filter(lhs == "m24_adni_mem", rhs == "m24_composite") %>%
  select(all) %>%
  pull %>%
  round(2)

engage_amyloidbaseline_to_adnimembaseline.beta <- engage_adnimem_results %>%
  filter(lhs == "bl_adni_mem", rhs == "bl_composite") %>%
  select(all) %>%
  pull %>%
  round(2)

## DONANEMAB - ADNIMEM

donanemab_adnimembaseline_to_adnimemm24.beta <- donanemab_adnimem_results %>%
  filter(lhs == "m24_adni_mem", rhs == "bl_adni_mem") %>%
  select(all) %>%
  pull() %>%
  round(2)

donanemab_amyloidbaseline_to_adnimem24.beta <- donanemab_adnimem_results %>%
  filter(lhs == "m24_adni_mem", rhs == "bl_composite") %>%
  select(all) %>%
  pull() %>%
  round(2)

donanemab_adnimembaseline_to_amyloidm24.beta <- donanemab_adnimem_results %>%
  filter(lhs == "m24_composite", rhs == "bl_adni_mem") %>%
  select(all) %>%
  pull() %>%
  round(2)

donanemab_amyloidbaseline_to_amyloidm24.beta <- donanemab_adnimem_results %>%
  filter(lhs == "m24_composite", rhs == "bl_composite") %>%
  select(all) %>%
  pull %>%
  round(2)

donanemab_amyloidm24_to_adnimemm24.beta <- donanemab_adnimem_results %>%
  filter(lhs == "m24_adni_mem", rhs == "m24_composite") %>%
  select(all) %>%
  pull %>%
  round(2)

donanemab_amyloidbaseline_to_adnimembaseline.beta <- donanemab_adnimem_results %>%
  filter(lhs == "bl_adni_mem", rhs == "bl_composite") %>%
  select(all) %>%
  pull %>%
  round(2)

## shouldbe - ADNIMEM

shouldbe_adnimembaseline_to_adnimemm24.beta <- shouldbe_adnimem_results %>%
  filter(lhs == "m24_adni_mem", rhs == "bl_adni_mem") %>%
  select(all) %>%
  pull() %>%
  round(2)

shouldbe_amyloidbaseline_to_adnimem24.beta <- shouldbe_adnimem_results %>%
  filter(lhs == "m24_adni_mem", rhs == "bl_composite") %>%
  select(all) %>%
  pull() %>%
  round(2)

shouldbe_adnimembaseline_to_amyloidm24.beta <- shouldbe_adnimem_results %>%
  filter(lhs == "m24_composite", rhs == "bl_adni_mem") %>%
  select(all) %>%
  pull() %>%
  round(2)

shouldbe_amyloidbaseline_to_amyloidm24.beta <- shouldbe_adnimem_results %>%
  filter(lhs == "m24_composite", rhs == "bl_composite") %>%
  select(all) %>%
  pull %>%
  round(2)

shouldbe_amyloidm24_to_adnimemm24.beta <- shouldbe_adnimem_results %>%
  filter(lhs == "m24_adni_mem", rhs == "m24_composite") %>%
  select(all) %>%
  pull %>%
  round(2)

shouldbe_amyloidbaseline_to_adnimembaseline.beta <- shouldbe_adnimem_results %>%
  filter(lhs == "bl_adni_mem", rhs == "bl_composite") %>%
  select(all) %>%
  pull %>%
  round(2)

## ENGAGE - adnief

engage_adniefbaseline_to_adniefm24.beta <- engage_adnief_results %>%
  filter(lhs == "m24_adni_ef", rhs == "bl_adni_ef") %>%
  select(all) %>%
  pull() %>%
  round(2)

engage_amyloidbaseline_to_adnief24.beta <- engage_adnief_results %>%
  filter(lhs == "m24_adni_ef", rhs == "bl_composite") %>%
  select(all) %>%
  pull() %>%
  round(2)

engage_adniefbaseline_to_amyloidm24.beta <- engage_adnief_results %>%
  filter(lhs == "m24_composite", rhs == "bl_adni_ef") %>%
  select(all) %>%
  pull() %>%
  round(2)

engage_amyloidbaseline_to_amyloidm24.beta <- engage_adnief_results %>%
  filter(lhs == "m24_composite", rhs == "bl_composite") %>%
  select(all) %>%
  pull %>%
  round(2)

engage_amyloidm24_to_adniefm24.beta <- engage_adnief_results %>%
  filter(lhs == "m24_adni_ef", rhs == "m24_composite") %>%
  select(all) %>%
  pull %>%
  round(2)

engage_amyloidbaseline_to_adniefbaseline.beta <- engage_adnief_results %>%
  filter(lhs == "bl_adni_ef", rhs == "bl_composite") %>%
  select(all) %>%
  pull %>%
  round(2)

## DONANEMAB - adnief

donanemab_adniefbaseline_to_adniefm24.beta <- donanemab_adnief_results %>%
  filter(lhs == "m24_adni_ef", rhs == "bl_adni_ef") %>%
  select(all) %>%
  pull() %>%
  round(2)

donanemab_amyloidbaseline_to_adnief24.beta <- donanemab_adnief_results %>%
  filter(lhs == "m24_adni_ef", rhs == "bl_composite") %>%
  select(all) %>%
  pull() %>%
  round(2)

donanemab_adniefbaseline_to_amyloidm24.beta <- donanemab_adnief_results %>%
  filter(lhs == "m24_composite", rhs == "bl_adni_ef") %>%
  select(all) %>%
  pull() %>%
  round(2)

donanemab_amyloidbaseline_to_amyloidm24.beta <- donanemab_adnief_results %>%
  filter(lhs == "m24_composite", rhs == "bl_composite") %>%
  select(all) %>%
  pull %>%
  round(2)

donanemab_amyloidm24_to_adniefm24.beta <- donanemab_adnief_results %>%
  filter(lhs == "m24_adni_ef", rhs == "m24_composite") %>%
  select(all) %>%
  pull %>%
  round(2)

donanemab_amyloidbaseline_to_adniefbaseline.beta <- donanemab_adnief_results %>%
  filter(lhs == "bl_adni_ef", rhs == "bl_composite") %>%
  select(all) %>%
  pull %>%
  round(2)

## shouldbe - adnief

shouldbe_adniefbaseline_to_adniefm24.beta <- shouldbe_adnief_results %>%
  filter(lhs == "m24_adni_ef", rhs == "bl_adni_ef") %>%
  select(all) %>%
  pull() %>%
  round(2)

shouldbe_amyloidbaseline_to_adnief24.beta <- shouldbe_adnief_results %>%
  filter(lhs == "m24_adni_ef", rhs == "bl_composite") %>%
  select(all) %>%
  pull() %>%
  round(2)

shouldbe_adniefbaseline_to_amyloidm24.beta <- shouldbe_adnief_results %>%
  filter(lhs == "m24_composite", rhs == "bl_adni_ef") %>%
  select(all) %>%
  pull() %>%
  round(2)

shouldbe_amyloidbaseline_to_amyloidm24.beta <- shouldbe_adnief_results %>%
  filter(lhs == "m24_composite", rhs == "bl_composite") %>%
  select(all) %>%
  pull %>%
  round(2)

shouldbe_amyloidm24_to_adniefm24.beta <- shouldbe_adnief_results %>%
  filter(lhs == "m24_adni_ef", rhs == "m24_composite") %>%
  select(all) %>%
  pull %>%
  round(2)

shouldbe_amyloidbaseline_to_adniefbaseline.beta <- shouldbe_adnief_results %>%
  filter(lhs == "bl_adni_ef", rhs == "bl_composite") %>%
  select(all) %>%
  pull %>%
  round(2)

## ENGAGE - adas13

engage_adas13baseline_to_adas13m24.beta <- engage_adas13_results %>%
  filter(lhs == "m24_adas13", rhs == "bl_adas13") %>%
  select(all) %>%
  pull() %>%
  round(2)

engage_amyloidbaseline_to_adas1324.beta <- engage_adas13_results %>%
  filter(lhs == "m24_adas13", rhs == "bl_composite") %>%
  select(all) %>%
  pull() %>%
  round(2)

engage_adas13baseline_to_amyloidm24.beta <- engage_adas13_results %>%
  filter(lhs == "m24_composite", rhs == "bl_adas13") %>%
  select(all) %>%
  pull() %>%
  round(2)

engage_amyloidbaseline_to_amyloidm24.beta <- engage_adas13_results %>%
  filter(lhs == "m24_composite", rhs == "bl_composite") %>%
  select(all) %>%
  pull %>%
  round(2)

engage_amyloidm24_to_adas13m24.beta <- engage_adas13_results %>%
  filter(lhs == "m24_adas13", rhs == "m24_composite") %>%
  select(all) %>%
  pull %>%
  round(2)

engage_amyloidbaseline_to_adas13baseline.beta <- engage_adas13_results %>%
  filter(lhs == "bl_adas13", rhs == "bl_composite") %>%
  select(all) %>%
  pull %>%
  round(2)

## DONANEMAB - adas13

donanemab_adas13baseline_to_adas13m24.beta <- donanemab_adas13_results %>%
  filter(lhs == "m24_adas13", rhs == "bl_adas13") %>%
  select(all) %>%
  pull() %>%
  round(2)

donanemab_amyloidbaseline_to_adas1324.beta <- donanemab_adas13_results %>%
  filter(lhs == "m24_adas13", rhs == "bl_composite") %>%
  select(all) %>%
  pull() %>%
  round(2)

donanemab_adas13baseline_to_amyloidm24.beta <- donanemab_adas13_results %>%
  filter(lhs == "m24_composite", rhs == "bl_adas13") %>%
  select(all) %>%
  pull() %>%
  round(2)

donanemab_amyloidbaseline_to_amyloidm24.beta <- donanemab_adas13_results %>%
  filter(lhs == "m24_composite", rhs == "bl_composite") %>%
  select(all) %>%
  pull %>%
  round(2)

donanemab_amyloidm24_to_adas13m24.beta <- donanemab_adas13_results %>%
  filter(lhs == "m24_adas13", rhs == "m24_composite") %>%
  select(all) %>%
  pull %>%
  round(2)

donanemab_amyloidbaseline_to_adas13baseline.beta <- donanemab_adas13_results %>%
  filter(lhs == "bl_adas13", rhs == "bl_composite") %>%
  select(all) %>%
  pull %>%
  round(2)

## shouldbe - adas13

shouldbe_adas13baseline_to_adas13m24.beta <- shouldbe_adas13_results %>%
  filter(lhs == "m24_adas13", rhs == "bl_adas13") %>%
  select(all) %>%
  pull() %>%
  round(2)

shouldbe_amyloidbaseline_to_adas1324.beta <- shouldbe_adas13_results %>%
  filter(lhs == "m24_adas13", rhs == "bl_composite") %>%
  select(all) %>%
  pull() %>%
  round(2)

shouldbe_adas13baseline_to_amyloidm24.beta <- shouldbe_adas13_results %>%
  filter(lhs == "m24_composite", rhs == "bl_adas13") %>%
  select(all) %>%
  pull() %>%
  round(2)

shouldbe_amyloidbaseline_to_amyloidm24.beta <- shouldbe_adas13_results %>%
  filter(lhs == "m24_composite", rhs == "bl_composite") %>%
  select(all) %>%
  pull %>%
  round(2)

shouldbe_amyloidm24_to_adas13m24.beta <- shouldbe_adas13_results %>%
  filter(lhs == "m24_adas13", rhs == "m24_composite") %>%
  select(all) %>%
  pull %>%
  round(2)

shouldbe_amyloidbaseline_to_adas13baseline.beta <- shouldbe_adas13_results %>%
  filter(lhs == "bl_adas13", rhs == "bl_composite") %>%
  select(all) %>%
  pull %>%
  round(2)

## b (unstandardized estimate)

## ENGAGE - ADNIMEM

engage_adnimembaseline_to_adnimemm24.b <- engage_adnimem_results %>%
  filter(lhs == "m24_adni_mem", rhs == "bl_adni_mem") %>%
  select(est) %>%
  pull() %>%
  round(2)

engage_amyloidbaseline_to_adnimem24.b <- engage_adnimem_results %>%
  filter(lhs == "m24_adni_mem", rhs == "bl_composite") %>%
  select(est) %>%
  pull() %>%
  round(2)

engage_adnimembaseline_to_amyloidm24.b <- engage_adnimem_results %>%
  filter(lhs == "m24_composite", rhs == "bl_adni_mem") %>%
  select(est) %>%
  pull() %>%
  round(2)

engage_amyloidbaseline_to_amyloidm24.b <- engage_adnimem_results %>%
  filter(lhs == "m24_composite", rhs == "bl_composite") %>%
  select(est) %>%
  pull %>%
  round(2)

engage_amyloidm24_to_adnimemm24.b <- engage_adnimem_results %>%
  filter(lhs == "m24_adni_mem", rhs == "m24_composite") %>%
  select(est) %>%
  pull %>%
  round(2)

engage_amyloidbaseline_to_adnimembaseline.b <- engage_adnimem_results %>%
  filter(lhs == "bl_adni_mem", rhs == "bl_composite") %>%
  select(est) %>%
  pull %>%
  round(2)

## DONANEMAB - ADNIMEM

donanemab_adnimembaseline_to_adnimemm24.b <- donanemab_adnimem_results %>%
  filter(lhs == "m24_adni_mem", rhs == "bl_adni_mem") %>%
  select(est) %>%
  pull() %>%
  round(2)

donanemab_amyloidbaseline_to_adnimem24.b <- donanemab_adnimem_results %>%
  filter(lhs == "m24_adni_mem", rhs == "bl_composite") %>%
  select(est) %>%
  pull() %>%
  round(2)

donanemab_adnimembaseline_to_amyloidm24.b <- donanemab_adnimem_results %>%
  filter(lhs == "m24_composite", rhs == "bl_adni_mem") %>%
  select(est) %>%
  pull() %>%
  round(2)

donanemab_amyloidbaseline_to_amyloidm24.b <- donanemab_adnimem_results %>%
  filter(lhs == "m24_composite", rhs == "bl_composite") %>%
  select(est) %>%
  pull %>%
  round(2)

donanemab_amyloidm24_to_adnimemm24.b <- donanemab_adnimem_results %>%
  filter(lhs == "m24_adni_mem", rhs == "m24_composite") %>%
  select(est) %>%
  pull %>%
  round(2)

donanemab_amyloidbaseline_to_adnimembaseline.b <- donanemab_adnimem_results %>%
  filter(lhs == "bl_adni_mem", rhs == "bl_composite") %>%
  select(est) %>%
  pull %>%
  round(2)

## shouldbe - ADNIMEM

shouldbe_adnimembaseline_to_adnimemm24.b <- shouldbe_adnimem_results %>%
  filter(lhs == "m24_adni_mem", rhs == "bl_adni_mem") %>%
  select(est) %>%
  pull() %>%
  round(2)

shouldbe_amyloidbaseline_to_adnimem24.b <- shouldbe_adnimem_results %>%
  filter(lhs == "m24_adni_mem", rhs == "bl_composite") %>%
  select(est) %>%
  pull() %>%
  round(2)

shouldbe_adnimembaseline_to_amyloidm24.b <- shouldbe_adnimem_results %>%
  filter(lhs == "m24_composite", rhs == "bl_adni_mem") %>%
  select(est) %>%
  pull() %>%
  round(2)

shouldbe_amyloidbaseline_to_amyloidm24.b <- shouldbe_adnimem_results %>%
  filter(lhs == "m24_composite", rhs == "bl_composite") %>%
  select(est) %>%
  pull %>%
  round(2)

shouldbe_amyloidm24_to_adnimemm24.b <- shouldbe_adnimem_results %>%
  filter(lhs == "m24_adni_mem", rhs == "m24_composite") %>%
  select(est) %>%
  pull %>%
  round(2)

shouldbe_amyloidbaseline_to_adnimembaseline.b <- shouldbe_adnimem_results %>%
  filter(lhs == "bl_adni_mem", rhs == "bl_composite") %>%
  select(est) %>%
  pull %>%
  round(2)

## ENGAGE - adnief

engage_adniefbaseline_to_adniefm24.b <- engage_adnief_results %>%
  filter(lhs == "m24_adni_ef", rhs == "bl_adni_ef") %>%
  select(est) %>%
  pull() %>%
  round(2)

engage_amyloidbaseline_to_adnief24.b <- engage_adnief_results %>%
  filter(lhs == "m24_adni_ef", rhs == "bl_composite") %>%
  select(est) %>%
  pull() %>%
  round(2)

engage_adniefbaseline_to_amyloidm24.b <- engage_adnief_results %>%
  filter(lhs == "m24_composite", rhs == "bl_adni_ef") %>%
  select(est) %>%
  pull() %>%
  round(2)

engage_amyloidbaseline_to_amyloidm24.b <- engage_adnief_results %>%
  filter(lhs == "m24_composite", rhs == "bl_composite") %>%
  select(est) %>%
  pull %>%
  round(2)

engage_amyloidm24_to_adniefm24.b <- engage_adnief_results %>%
  filter(lhs == "m24_adni_ef", rhs == "m24_composite") %>%
  select(est) %>%
  pull %>%
  round(2)

engage_amyloidbaseline_to_adniefbaseline.b <- engage_adnief_results %>%
  filter(lhs == "bl_adni_ef", rhs == "bl_composite") %>%
  select(est) %>%
  pull %>%
  round(2)

## DONANEMAB - adnief

donanemab_adniefbaseline_to_adniefm24.b <- donanemab_adnief_results %>%
  filter(lhs == "m24_adni_ef", rhs == "bl_adni_ef") %>%
  select(est) %>%
  pull() %>%
  round(2)

donanemab_amyloidbaseline_to_adnief24.b <- donanemab_adnief_results %>%
  filter(lhs == "m24_adni_ef", rhs == "bl_composite") %>%
  select(est) %>%
  pull() %>%
  round(2)

donanemab_adniefbaseline_to_amyloidm24.b <- donanemab_adnief_results %>%
  filter(lhs == "m24_composite", rhs == "bl_adni_ef") %>%
  select(est) %>%
  pull() %>%
  round(2)

donanemab_amyloidbaseline_to_amyloidm24.b <- donanemab_adnief_results %>%
  filter(lhs == "m24_composite", rhs == "bl_composite") %>%
  select(est) %>%
  pull %>%
  round(2)

donanemab_amyloidm24_to_adniefm24.b <- donanemab_adnief_results %>%
  filter(lhs == "m24_adni_ef", rhs == "m24_composite") %>%
  select(est) %>%
  pull %>%
  round(2)

donanemab_amyloidbaseline_to_adniefbaseline.b <- donanemab_adnief_results %>%
  filter(lhs == "bl_adni_ef", rhs == "bl_composite") %>%
  select(est) %>%
  pull %>%
  round(2)

## shouldbe - adnief

shouldbe_adniefbaseline_to_adniefm24.b <- shouldbe_adnief_results %>%
  filter(lhs == "m24_adni_ef", rhs == "bl_adni_ef") %>%
  select(est) %>%
  pull() %>%
  round(2)

shouldbe_amyloidbaseline_to_adnief24.b <- shouldbe_adnief_results %>%
  filter(lhs == "m24_adni_ef", rhs == "bl_composite") %>%
  select(est) %>%
  pull() %>%
  round(2)

shouldbe_adniefbaseline_to_amyloidm24.b <- shouldbe_adnief_results %>%
  filter(lhs == "m24_composite", rhs == "bl_adni_ef") %>%
  select(est) %>%
  pull() %>%
  round(2)

shouldbe_amyloidbaseline_to_amyloidm24.b <- shouldbe_adnief_results %>%
  filter(lhs == "m24_composite", rhs == "bl_composite") %>%
  select(est) %>%
  pull %>%
  round(2)

shouldbe_amyloidm24_to_adniefm24.b <- shouldbe_adnief_results %>%
  filter(lhs == "m24_adni_ef", rhs == "m24_composite") %>%
  select(est) %>%
  pull %>%
  round(2)

shouldbe_amyloidbaseline_to_adniefbaseline.b <- shouldbe_adnief_results %>%
  filter(lhs == "bl_adni_ef", rhs == "bl_composite") %>%
  select(est) %>%
  pull %>%
  round(2)

## ENGAGE - adas13

engage_adas13baseline_to_adas13m24.b <- engage_adas13_results %>%
  filter(lhs == "m24_adas13", rhs == "bl_adas13") %>%
  select(est) %>%
  pull() %>%
  round(2)

engage_amyloidbaseline_to_adas1324.b <- engage_adas13_results %>%
  filter(lhs == "m24_adas13", rhs == "bl_composite") %>%
  select(est) %>%
  pull() %>%
  round(2)

engage_adas13baseline_to_amyloidm24.b <- engage_adas13_results %>%
  filter(lhs == "m24_composite", rhs == "bl_adas13") %>%
  select(est) %>%
  pull() %>%
  round(2)

engage_amyloidbaseline_to_amyloidm24.b <- engage_adas13_results %>%
  filter(lhs == "m24_composite", rhs == "bl_composite") %>%
  select(est) %>%
  pull %>%
  round(2)

engage_amyloidm24_to_adas13m24.b <- engage_adas13_results %>%
  filter(lhs == "m24_adas13", rhs == "m24_composite") %>%
  select(est) %>%
  pull %>%
  round(2)

engage_amyloidbaseline_to_adas13baseline.b <- engage_adas13_results %>%
  filter(lhs == "bl_adas13", rhs == "bl_composite") %>%
  select(est) %>%
  pull %>%
  round(2)

## DONANEMAB - adas13

donanemab_adas13baseline_to_adas13m24.b <- donanemab_adas13_results %>%
  filter(lhs == "m24_adas13", rhs == "bl_adas13") %>%
  select(est) %>%
  pull() %>%
  round(2)

donanemab_amyloidbaseline_to_adas1324.b <- donanemab_adas13_results %>%
  filter(lhs == "m24_adas13", rhs == "bl_composite") %>%
  select(est) %>%
  pull() %>%
  round(2)

donanemab_adas13baseline_to_amyloidm24.b <- donanemab_adas13_results %>%
  filter(lhs == "m24_composite", rhs == "bl_adas13") %>%
  select(est) %>%
  pull() %>%
  round(2)

donanemab_amyloidbaseline_to_amyloidm24.b <- donanemab_adas13_results %>%
  filter(lhs == "m24_composite", rhs == "bl_composite") %>%
  select(est) %>%
  pull %>%
  round(2)

donanemab_amyloidm24_to_adas13m24.b <- donanemab_adas13_results %>%
  filter(lhs == "m24_adas13", rhs == "m24_composite") %>%
  select(est) %>%
  pull %>%
  round(2)

donanemab_amyloidbaseline_to_adas13baseline.b <- donanemab_adas13_results %>%
  filter(lhs == "bl_adas13", rhs == "bl_composite") %>%
  select(est) %>%
  pull %>%
  round(2)

## shouldbe - adas13

shouldbe_adas13baseline_to_adas13m24.b <- shouldbe_adas13_results %>%
  filter(lhs == "m24_adas13", rhs == "bl_adas13") %>%
  select(est) %>%
  pull() %>%
  round(2)

shouldbe_amyloidbaseline_to_adas1324.b <- shouldbe_adas13_results %>%
  filter(lhs == "m24_adas13", rhs == "bl_composite") %>%
  select(est) %>%
  pull() %>%
  round(2)

shouldbe_adas13baseline_to_amyloidm24.b <- shouldbe_adas13_results %>%
  filter(lhs == "m24_composite", rhs == "bl_adas13") %>%
  select(est) %>%
  pull() %>%
  round(2)

shouldbe_amyloidbaseline_to_amyloidm24.b <- shouldbe_adas13_results %>%
  filter(lhs == "m24_composite", rhs == "bl_composite") %>%
  select(est) %>%
  pull %>%
  round(2)

shouldbe_amyloidm24_to_adas13m24.b <- shouldbe_adas13_results %>%
  filter(lhs == "m24_adas13", rhs == "m24_composite") %>%
  select(est) %>%
  pull %>%
  round(2)

shouldbe_amyloidbaseline_to_adas13baseline.b <- shouldbe_adas13_results %>%
  filter(lhs == "bl_adas13", rhs == "bl_composite") %>%
  select(est) %>%
  pull %>%
  round(2)


# SE

## ENGAGE - ADNIMEM

engage_adnimembaseline_to_adnimemm24.se <- engage_adnimem_results %>%
  filter(lhs == "m24_adni_mem", rhs == "bl_adni_mem") %>%
  select(se) %>%
  pull() %>%
  round(2)

engage_amyloidbaseline_to_adnimem24.se <- engage_adnimem_results %>%
  filter(lhs == "m24_adni_mem", rhs == "bl_composite") %>%
  select(se) %>%
  pull() %>%
  round(2)

engage_adnimembaseline_to_amyloidm24.se <- engage_adnimem_results %>%
  filter(lhs == "m24_composite", rhs == "bl_adni_mem") %>%
  select(se) %>%
  pull() %>%
  round(2)

engage_amyloidbaseline_to_amyloidm24.se <- engage_adnimem_results %>%
  filter(lhs == "m24_composite", rhs == "bl_composite") %>%
  select(se) %>%
  pull %>%
  round(2)

engage_amyloidm24_to_adnimemm24.se <- engage_adnimem_results %>%
  filter(lhs == "m24_adni_mem", rhs == "m24_composite") %>%
  select(se) %>%
  pull %>%
  round(2)

engage_amyloidbaseline_to_adnimembaseline.se <- engage_adnimem_results %>%
  filter(lhs == "bl_adni_mem", rhs == "bl_composite") %>%
  select(se) %>%
  pull %>%
  round(2)

## DONANEMAB - ADNIMEM

donanemab_adnimembaseline_to_adnimemm24.se <- donanemab_adnimem_results %>%
  filter(lhs == "m24_adni_mem", rhs == "bl_adni_mem") %>%
  select(se) %>%
  pull() %>%
  round(2)

donanemab_amyloidbaseline_to_adnimem24.se <- donanemab_adnimem_results %>%
  filter(lhs == "m24_adni_mem", rhs == "bl_composite") %>%
  select(se) %>%
  pull() %>%
  round(2)

donanemab_adnimembaseline_to_amyloidm24.se <- donanemab_adnimem_results %>%
  filter(lhs == "m24_composite", rhs == "bl_adni_mem") %>%
  select(se) %>%
  pull() %>%
  round(2)

donanemab_amyloidbaseline_to_amyloidm24.se <- donanemab_adnimem_results %>%
  filter(lhs == "m24_composite", rhs == "bl_composite") %>%
  select(se) %>%
  pull %>%
  round(2)

donanemab_amyloidm24_to_adnimemm24.se <- donanemab_adnimem_results %>%
  filter(lhs == "m24_adni_mem", rhs == "m24_composite") %>%
  select(se) %>%
  pull %>%
  round(2)

donanemab_amyloidbaseline_to_adnimembaseline.se <- donanemab_adnimem_results %>%
  filter(lhs == "bl_adni_mem", rhs == "bl_composite") %>%
  select(se) %>%
  pull %>%
  round(2)

## shouldbe - ADNIMEM

shouldbe_adnimembaseline_to_adnimemm24.se <- shouldbe_adnimem_results %>%
  filter(lhs == "m24_adni_mem", rhs == "bl_adni_mem") %>%
  select(se) %>%
  pull() %>%
  round(2)

shouldbe_amyloidbaseline_to_adnimem24.se <- shouldbe_adnimem_results %>%
  filter(lhs == "m24_adni_mem", rhs == "bl_composite") %>%
  select(se) %>%
  pull() %>%
  round(2)

shouldbe_adnimembaseline_to_amyloidm24.se <- shouldbe_adnimem_results %>%
  filter(lhs == "m24_composite", rhs == "bl_adni_mem") %>%
  select(se) %>%
  pull() %>%
  round(2)

shouldbe_amyloidbaseline_to_amyloidm24.se <- shouldbe_adnimem_results %>%
  filter(lhs == "m24_composite", rhs == "bl_composite") %>%
  select(se) %>%
  pull %>%
  round(2)

shouldbe_amyloidm24_to_adnimemm24.se <- shouldbe_adnimem_results %>%
  filter(lhs == "m24_adni_mem", rhs == "m24_composite") %>%
  select(se) %>%
  pull %>%
  round(2)

shouldbe_amyloidbaseline_to_adnimembaseline.se <- shouldbe_adnimem_results %>%
  filter(lhs == "bl_adni_mem", rhs == "bl_composite") %>%
  select(se) %>%
  pull %>%
  round(2)

## ENGAGE - adnief

engage_adniefbaseline_to_adniefm24.se <- engage_adnief_results %>%
  filter(lhs == "m24_adni_ef", rhs == "bl_adni_ef") %>%
  select(se) %>%
  pull() %>%
  round(2)

engage_amyloidbaseline_to_adnief24.se <- engage_adnief_results %>%
  filter(lhs == "m24_adni_ef", rhs == "bl_composite") %>%
  select(se) %>%
  pull() %>%
  round(2)

engage_adniefbaseline_to_amyloidm24.se <- engage_adnief_results %>%
  filter(lhs == "m24_composite", rhs == "bl_adni_ef") %>%
  select(se) %>%
  pull() %>%
  round(2)

engage_amyloidbaseline_to_amyloidm24.se <- engage_adnief_results %>%
  filter(lhs == "m24_composite", rhs == "bl_composite") %>%
  select(se) %>%
  pull %>%
  round(2)

engage_amyloidm24_to_adniefm24.se <- engage_adnief_results %>%
  filter(lhs == "m24_adni_ef", rhs == "m24_composite") %>%
  select(se) %>%
  pull %>%
  round(2)

engage_amyloidbaseline_to_adniefbaseline.se <- engage_adnief_results %>%
  filter(lhs == "bl_adni_ef", rhs == "bl_composite") %>%
  select(se) %>%
  pull %>%
  round(2)

## DONANEMAB - adnief

donanemab_adniefbaseline_to_adniefm24.se <- donanemab_adnief_results %>%
  filter(lhs == "m24_adni_ef", rhs == "bl_adni_ef") %>%
  select(se) %>%
  pull() %>%
  round(2)

donanemab_amyloidbaseline_to_adnief24.se <- donanemab_adnief_results %>%
  filter(lhs == "m24_adni_ef", rhs == "bl_composite") %>%
  select(se) %>%
  pull() %>%
  round(2)

donanemab_adniefbaseline_to_amyloidm24.se <- donanemab_adnief_results %>%
  filter(lhs == "m24_composite", rhs == "bl_adni_ef") %>%
  select(se) %>%
  pull() %>%
  round(2)

donanemab_amyloidbaseline_to_amyloidm24.se <- donanemab_adnief_results %>%
  filter(lhs == "m24_composite", rhs == "bl_composite") %>%
  select(se) %>%
  pull %>%
  round(2)

donanemab_amyloidm24_to_adniefm24.se <- donanemab_adnief_results %>%
  filter(lhs == "m24_adni_ef", rhs == "m24_composite") %>%
  select(se) %>%
  pull %>%
  round(2)

donanemab_amyloidbaseline_to_adniefbaseline.se <- donanemab_adnief_results %>%
  filter(lhs == "bl_adni_ef", rhs == "bl_composite") %>%
  select(se) %>%
  pull %>%
  round(2)

## shouldbe - adnief

shouldbe_adniefbaseline_to_adniefm24.se <- shouldbe_adnief_results %>%
  filter(lhs == "m24_adni_ef", rhs == "bl_adni_ef") %>%
  select(se) %>%
  pull() %>%
  round(2)

shouldbe_amyloidbaseline_to_adnief24.se <- shouldbe_adnief_results %>%
  filter(lhs == "m24_adni_ef", rhs == "bl_composite") %>%
  select(se) %>%
  pull() %>%
  round(2)

shouldbe_adniefbaseline_to_amyloidm24.se <- shouldbe_adnief_results %>%
  filter(lhs == "m24_composite", rhs == "bl_adni_ef") %>%
  select(se) %>%
  pull() %>%
  round(2)

shouldbe_amyloidbaseline_to_amyloidm24.se <- shouldbe_adnief_results %>%
  filter(lhs == "m24_composite", rhs == "bl_composite") %>%
  select(se) %>%
  pull %>%
  round(2)

shouldbe_amyloidm24_to_adniefm24.se <- shouldbe_adnief_results %>%
  filter(lhs == "m24_adni_ef", rhs == "m24_composite") %>%
  select(se) %>%
  pull %>%
  round(2)

shouldbe_amyloidbaseline_to_adniefbaseline.se <- shouldbe_adnief_results %>%
  filter(lhs == "bl_adni_ef", rhs == "bl_composite") %>%
  select(se) %>%
  pull %>%
  round(2)

## ENGAGE - adas13

engage_adas13baseline_to_adas13m24.se <- engage_adas13_results %>%
  filter(lhs == "m24_adas13", rhs == "bl_adas13") %>%
  select(se) %>%
  pull() %>%
  round(2)

engage_amyloidbaseline_to_adas1324.se <- engage_adas13_results %>%
  filter(lhs == "m24_adas13", rhs == "bl_composite") %>%
  select(se) %>%
  pull() %>%
  round(2)

engage_adas13baseline_to_amyloidm24.se <- engage_adas13_results %>%
  filter(lhs == "m24_composite", rhs == "bl_adas13") %>%
  select(se) %>%
  pull() %>%
  round(2)

engage_amyloidbaseline_to_amyloidm24.se <- engage_adas13_results %>%
  filter(lhs == "m24_composite", rhs == "bl_composite") %>%
  select(se) %>%
  pull %>%
  round(2)

engage_amyloidm24_to_adas13m24.se <- engage_adas13_results %>%
  filter(lhs == "m24_adas13", rhs == "m24_composite") %>%
  select(se) %>%
  pull %>%
  round(2)

engage_amyloidbaseline_to_adas13baseline.se <- engage_adas13_results %>%
  filter(lhs == "bl_adas13", rhs == "bl_composite") %>%
  select(se) %>%
  pull %>%
  round(2)

## DONANEMAB - adas13

donanemab_adas13baseline_to_adas13m24.se <- donanemab_adas13_results %>%
  filter(lhs == "m24_adas13", rhs == "bl_adas13") %>%
  select(se) %>%
  pull() %>%
  round(2)

donanemab_amyloidbaseline_to_adas1324.se <- donanemab_adas13_results %>%
  filter(lhs == "m24_adas13", rhs == "bl_composite") %>%
  select(se) %>%
  pull() %>%
  round(2)

donanemab_adas13baseline_to_amyloidm24.se <- donanemab_adas13_results %>%
  filter(lhs == "m24_composite", rhs == "bl_adas13") %>%
  select(se) %>%
  pull() %>%
  round(2)

donanemab_amyloidbaseline_to_amyloidm24.se <- donanemab_adas13_results %>%
  filter(lhs == "m24_composite", rhs == "bl_composite") %>%
  select(se) %>%
  pull %>%
  round(2)

donanemab_amyloidm24_to_adas13m24.se <- donanemab_adas13_results %>%
  filter(lhs == "m24_adas13", rhs == "m24_composite") %>%
  select(se) %>%
  pull %>%
  round(2)

donanemab_amyloidbaseline_to_adas13baseline.se <- donanemab_adas13_results %>%
  filter(lhs == "bl_adas13", rhs == "bl_composite") %>%
  select(se) %>%
  pull %>%
  round(2)

## shouldbe - adas13

shouldbe_adas13baseline_to_adas13m24.se <- shouldbe_adas13_results %>%
  filter(lhs == "m24_adas13", rhs == "bl_adas13") %>%
  select(se) %>%
  pull() %>%
  round(2)

shouldbe_amyloidbaseline_to_adas1324.se <- shouldbe_adas13_results %>%
  filter(lhs == "m24_adas13", rhs == "bl_composite") %>%
  select(se) %>%
  pull() %>%
  round(2)

shouldbe_adas13baseline_to_amyloidm24.se <- shouldbe_adas13_results %>%
  filter(lhs == "m24_composite", rhs == "bl_adas13") %>%
  select(se) %>%
  pull() %>%
  round(2)

shouldbe_amyloidbaseline_to_amyloidm24.se <- shouldbe_adas13_results %>%
  filter(lhs == "m24_composite", rhs == "bl_composite") %>%
  select(se) %>%
  pull %>%
  round(2)

shouldbe_amyloidm24_to_adas13m24.se <- shouldbe_adas13_results %>%
  filter(lhs == "m24_adas13", rhs == "m24_composite") %>%
  select(se) %>%
  pull %>%
  round(2)

shouldbe_amyloidbaseline_to_adas13baseline.se <- shouldbe_adas13_results %>%
  filter(lhs == "bl_adas13", rhs == "bl_composite") %>%
  select(se) %>%
  pull %>%
  round(2)

# pvalue

## ENGAGE - ADNIMEM

# p-value format (pvf)
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
# pvf(.046)

engage_adnimembaseline_to_adnimemm24.pvalue <- engage_adnimem_results %>%
  filter(lhs == "m24_adni_mem", rhs == "bl_adni_mem") %>%
  select(pvalue) %>%
  pull() %>%
  pvf()

engage_amyloidbaseline_to_adnimem24.pvalue <- engage_adnimem_results %>%
  filter(lhs == "m24_adni_mem", rhs == "bl_composite") %>%
  select(pvalue) %>%
  pull() %>%
  pvf()

engage_adnimembaseline_to_amyloidm24.pvalue <- engage_adnimem_results %>%
  filter(lhs == "m24_composite", rhs == "bl_adni_mem") %>%
  select(pvalue) %>%
  pull() %>%
  pvf()

engage_amyloidbaseline_to_amyloidm24.pvalue <- engage_adnimem_results %>%
  filter(lhs == "m24_composite", rhs == "bl_composite") %>%
  select(pvalue) %>%
  pull %>%
  pvf()

engage_amyloidm24_to_adnimemm24.pvalue <- engage_adnimem_results %>%
  filter(lhs == "m24_adni_mem", rhs == "m24_composite") %>%
  select(pvalue) %>%
  pull %>%
  pvf()

engage_amyloidbaseline_to_adnimembaseline.pvalue <- engage_adnimem_results %>%
  filter(lhs == "bl_adni_mem", rhs == "bl_composite") %>%
  select(pvalue) %>%
  pull %>%
  pvf()

## DONANEMAB - ADNIMEM

donanemab_adnimembaseline_to_adnimemm24.pvalue <- donanemab_adnimem_results %>%
  filter(lhs == "m24_adni_mem", rhs == "bl_adni_mem") %>%
  select(pvalue) %>%
  pull() %>%
  pvf()

donanemab_amyloidbaseline_to_adnimem24.pvalue <- donanemab_adnimem_results %>%
  filter(lhs == "m24_adni_mem", rhs == "bl_composite") %>%
  select(pvalue) %>%
  pull() %>%
  pvf()

donanemab_adnimembaseline_to_amyloidm24.pvalue <- donanemab_adnimem_results %>%
  filter(lhs == "m24_composite", rhs == "bl_adni_mem") %>%
  select(pvalue) %>%
  pull() %>%
  pvf()

donanemab_amyloidbaseline_to_amyloidm24.pvalue <- donanemab_adnimem_results %>%
  filter(lhs == "m24_composite", rhs == "bl_composite") %>%
  select(pvalue) %>%
  pull %>%
  pvf()

donanemab_amyloidm24_to_adnimemm24.pvalue <- donanemab_adnimem_results %>%
  filter(lhs == "m24_adni_mem", rhs == "m24_composite") %>%
  select(pvalue) %>%
  pull %>%
  pvf()

donanemab_amyloidbaseline_to_adnimembaseline.pvalue <- donanemab_adnimem_results %>%
  filter(lhs == "bl_adni_mem", rhs == "bl_composite") %>%
  select(pvalue) %>%
  pull %>%
  pvf()

## shouldbe - ADNIMEM

shouldbe_adnimembaseline_to_adnimemm24.pvalue <- shouldbe_adnimem_results %>%
  filter(lhs == "m24_adni_mem", rhs == "bl_adni_mem") %>%
  select(pvalue) %>%
  pull() %>%
  pvf()

shouldbe_amyloidbaseline_to_adnimem24.pvalue <- shouldbe_adnimem_results %>%
  filter(lhs == "m24_adni_mem", rhs == "bl_composite") %>%
  select(pvalue) %>%
  pull() %>%
  pvf()

shouldbe_adnimembaseline_to_amyloidm24.pvalue <- shouldbe_adnimem_results %>%
  filter(lhs == "m24_composite", rhs == "bl_adni_mem") %>%
  select(pvalue) %>%
  pull() %>%
  pvf()

shouldbe_amyloidbaseline_to_amyloidm24.pvalue <- shouldbe_adnimem_results %>%
  filter(lhs == "m24_composite", rhs == "bl_composite") %>%
  select(pvalue) %>%
  pull %>%
  pvf()

shouldbe_amyloidm24_to_adnimemm24.pvalue <- shouldbe_adnimem_results %>%
  filter(lhs == "m24_adni_mem", rhs == "m24_composite") %>%
  select(pvalue) %>%
  pull %>%
  pvf()

shouldbe_amyloidbaseline_to_adnimembaseline.pvalue <- shouldbe_adnimem_results %>%
  filter(lhs == "bl_adni_mem", rhs == "bl_composite") %>%
  select(pvalue) %>%
  pull %>%
  pvf()

## ENGAGE - adnief

engage_adniefbaseline_to_adniefm24.pvalue <- engage_adnief_results %>%
  filter(lhs == "m24_adni_ef", rhs == "bl_adni_ef") %>%
  select(pvalue) %>%
  pull() %>%
  pvf()

engage_amyloidbaseline_to_adnief24.pvalue <- engage_adnief_results %>%
  filter(lhs == "m24_adni_ef", rhs == "bl_composite") %>%
  select(pvalue) %>%
  pull() %>%
  pvf()

engage_adniefbaseline_to_amyloidm24.pvalue <- engage_adnief_results %>%
  filter(lhs == "m24_composite", rhs == "bl_adni_ef") %>%
  select(pvalue) %>%
  pull() %>%
  pvf()

engage_amyloidbaseline_to_amyloidm24.pvalue <- engage_adnief_results %>%
  filter(lhs == "m24_composite", rhs == "bl_composite") %>%
  select(pvalue) %>%
  pull %>%
  pvf()

engage_amyloidm24_to_adniefm24.pvalue <- engage_adnief_results %>%
  filter(lhs == "m24_adni_ef", rhs == "m24_composite") %>%
  select(pvalue) %>%
  pull %>%
  pvf()

engage_amyloidbaseline_to_adniefbaseline.pvalue <- engage_adnief_results %>%
  filter(lhs == "bl_adni_ef", rhs == "bl_composite") %>%
  select(pvalue) %>%
  pull %>%
  pvf()

## DONANEMAB - adnief

donanemab_adniefbaseline_to_adniefm24.pvalue <- donanemab_adnief_results %>%
  filter(lhs == "m24_adni_ef", rhs == "bl_adni_ef") %>%
  select(pvalue) %>%
  pull() %>%
  pvf()

donanemab_amyloidbaseline_to_adnief24.pvalue <- donanemab_adnief_results %>%
  filter(lhs == "m24_adni_ef", rhs == "bl_composite") %>%
  select(pvalue) %>%
  pull() %>%
  pvf()

donanemab_adniefbaseline_to_amyloidm24.pvalue <- donanemab_adnief_results %>%
  filter(lhs == "m24_composite", rhs == "bl_adni_ef") %>%
  select(pvalue) %>%
  pull() %>%
  pvf()

donanemab_amyloidbaseline_to_amyloidm24.pvalue <- donanemab_adnief_results %>%
  filter(lhs == "m24_composite", rhs == "bl_composite") %>%
  select(pvalue) %>%
  pull %>%
  pvf()

donanemab_amyloidm24_to_adniefm24.pvalue <- donanemab_adnief_results %>%
  filter(lhs == "m24_adni_ef", rhs == "m24_composite") %>%
  select(pvalue) %>%
  pull %>%
  pvf()

donanemab_amyloidbaseline_to_adniefbaseline.pvalue <- donanemab_adnief_results %>%
  filter(lhs == "bl_adni_ef", rhs == "bl_composite") %>%
  select(pvalue) %>%
  pull %>%
  pvf()

## shouldbe - adnief

shouldbe_adniefbaseline_to_adniefm24.pvalue <- shouldbe_adnief_results %>%
  filter(lhs == "m24_adni_ef", rhs == "bl_adni_ef") %>%
  select(pvalue) %>%
  pull() %>%
  pvf()

shouldbe_amyloidbaseline_to_adnief24.pvalue <- shouldbe_adnief_results %>%
  filter(lhs == "m24_adni_ef", rhs == "bl_composite") %>%
  select(pvalue) %>%
  pull() %>%
  pvf()

shouldbe_adniefbaseline_to_amyloidm24.pvalue <- shouldbe_adnief_results %>%
  filter(lhs == "m24_composite", rhs == "bl_adni_ef") %>%
  select(pvalue) %>%
  pull() %>%
  pvf()

shouldbe_amyloidbaseline_to_amyloidm24.pvalue <- shouldbe_adnief_results %>%
  filter(lhs == "m24_composite", rhs == "bl_composite") %>%
  select(pvalue) %>%
  pull %>%
  pvf()

shouldbe_amyloidm24_to_adniefm24.pvalue <- shouldbe_adnief_results %>%
  filter(lhs == "m24_adni_ef", rhs == "m24_composite") %>%
  select(pvalue) %>%
  pull %>%
  pvf()

shouldbe_amyloidbaseline_to_adniefbaseline.pvalue <- shouldbe_adnief_results %>%
  filter(lhs == "bl_adni_ef", rhs == "bl_composite") %>%
  select(pvalue) %>%
  pull %>%
  pvf()

## ENGAGE - adas13

engage_adas13baseline_to_adas13m24.pvalue <- engage_adas13_results %>%
  filter(lhs == "m24_adas13", rhs == "bl_adas13") %>%
  select(pvalue) %>%
  pull() %>%
  pvf()

engage_amyloidbaseline_to_adas1324.pvalue <- engage_adas13_results %>%
  filter(lhs == "m24_adas13", rhs == "bl_composite") %>%
  select(pvalue) %>%
  pull() %>%
  pvf()

engage_adas13baseline_to_amyloidm24.pvalue <- engage_adas13_results %>%
  filter(lhs == "m24_composite", rhs == "bl_adas13") %>%
  select(pvalue) %>%
  pull() %>%
  pvf()

engage_amyloidbaseline_to_amyloidm24.pvalue <- engage_adas13_results %>%
  filter(lhs == "m24_composite", rhs == "bl_composite") %>%
  select(pvalue) %>%
  pull %>%
  pvf()

engage_amyloidm24_to_adas13m24.pvalue <- engage_adas13_results %>%
  filter(lhs == "m24_adas13", rhs == "m24_composite") %>%
  select(pvalue) %>%
  pull %>%
  pvf()

engage_amyloidbaseline_to_adas13baseline.pvalue <- engage_adas13_results %>%
  filter(lhs == "bl_adas13", rhs == "bl_composite") %>%
  select(pvalue) %>%
  pull %>%
  pvf()

## DONANEMAB - adas13

donanemab_adas13baseline_to_adas13m24.pvalue <- donanemab_adas13_results %>%
  filter(lhs == "m24_adas13", rhs == "bl_adas13") %>%
  select(pvalue) %>%
  pull() %>%
  pvf()

donanemab_amyloidbaseline_to_adas1324.pvalue <- donanemab_adas13_results %>%
  filter(lhs == "m24_adas13", rhs == "bl_composite") %>%
  select(pvalue) %>%
  pull() %>%
  pvf()

donanemab_adas13baseline_to_amyloidm24.pvalue <- donanemab_adas13_results %>%
  filter(lhs == "m24_composite", rhs == "bl_adas13") %>%
  select(pvalue) %>%
  pull() %>%
  pvf()

donanemab_amyloidbaseline_to_amyloidm24.pvalue <- donanemab_adas13_results %>%
  filter(lhs == "m24_composite", rhs == "bl_composite") %>%
  select(pvalue) %>%
  pull %>%
  pvf()

donanemab_amyloidm24_to_adas13m24.pvalue <- donanemab_adas13_results %>%
  filter(lhs == "m24_adas13", rhs == "m24_composite") %>%
  select(pvalue) %>%
  pull %>%
  pvf()

donanemab_amyloidbaseline_to_adas13baseline.pvalue <- donanemab_adas13_results %>%
  filter(lhs == "bl_adas13", rhs == "bl_composite") %>%
  select(pvalue) %>%
  pull %>%
  pvf()

## shouldbe - adas13

shouldbe_adas13baseline_to_adas13m24.pvalue <- shouldbe_adas13_results %>%
  filter(lhs == "m24_adas13", rhs == "bl_adas13") %>%
  select(pvalue) %>%
  pull() %>%
  pvf()

shouldbe_amyloidbaseline_to_adas1324.pvalue <- shouldbe_adas13_results %>%
  filter(lhs == "m24_adas13", rhs == "bl_composite") %>%
  select(pvalue) %>%
  pull() %>%
  pvf()

shouldbe_adas13baseline_to_amyloidm24.pvalue <- shouldbe_adas13_results %>%
  filter(lhs == "m24_composite", rhs == "bl_adas13") %>%
  select(pvalue) %>%
  pull() %>%
  pvf()

shouldbe_amyloidbaseline_to_amyloidm24.pvalue <- shouldbe_adas13_results %>%
  filter(lhs == "m24_composite", rhs == "bl_composite") %>%
  select(pvalue) %>%
  pull %>%
  pvf()

shouldbe_amyloidm24_to_adas13m24.pvalue <- shouldbe_adas13_results %>%
  filter(lhs == "m24_adas13", rhs == "m24_composite") %>%
  select(pvalue) %>%
  pull %>%
  pvf()

shouldbe_amyloidbaseline_to_adas13baseline.pvalue <- shouldbe_adas13_results %>%
  filter(lhs == "bl_adas13", rhs == "bl_composite") %>%
  select(pvalue) %>%
  pull %>%
  pvf()

## Generate figures

generate_clpm_figure <- function(bl_amyloid_to_bl_cog, m24_amyloid_to_m24_cog, bl_amyloid_to_m24_amyloid,
                                 bl_cog_to_m24_cog, bl_amyloid_to_m24_cog, bl_cog_to_m24_amyloid){

data <- tibble(x= 1:100, y= 1:100)

data %>%
  ggplot(aes(x, y)) +
  theme_void() +
  scale_x_continuous(minor_breaks = seq(10, 100, 10)) +
  scale_y_continuous(minor_breaks = seq(10, 100, 10)) +
  # Draw nodes
  geom_rect(xmin = 20, xmax=30, ymin=70, ymax=80, color='black',
            fill='white', size=0.25, size=0.25) +
  annotate('text', x= 25, y=75,label= 'Amyloid \n (Baseline)', size=2.5) +
  geom_rect(xmin = 70, xmax=80, ymin=70, ymax=80, color='black',
            fill='white', size=0.25, size=0.25) +
  annotate('text', x= 75, y=75,label= 'Amyloid \n (M24)', size=2.5) +
  geom_rect(xmin = 20, xmax=30, ymin=20, ymax=30, color='black',
            fill='white', size=0.25, size=0.25) +
  annotate('text', x= 25, y=25,label= 'Cognition \n (Baseline)', size=2.5) +
  geom_rect(xmin = 70, xmax=80, ymin=20, ymax=30, color='black',
            fill='white', size=0.25, size=0.25) +
  annotate('text', x= 75, y=25,label= 'Cognition \n (M24)', size=2.5) +
  # Draw arrows
  geom_segment(x = 25, xend = 25, y = 70, yend = 30, # amyloid baseline -> cog baseline
               size = 1, linejoin = "mitre", lineend = "butt",
               arrow = arrow(length = unit(1, "mm"), type= "closed")) +
  geom_segment(x = 75, xend = 75, y = 70, yend = 30, # amyloid m24 -> cog m24
               size = 1, linejoin = "mitre", lineend = "butt",
               arrow = arrow(length = unit(1, "mm"), type= "closed")) +
  geom_segment(x = 30, xend = 70, y = 25, yend = 25, # cog baseline -> cog m24
               size = 1, linejoin = "mitre", lineend = "butt",
               arrow = arrow(length = unit(1, "mm"), type= "closed")) +
  geom_segment(x = 30, xend = 70, y = 75, yend = 75, # amyloid baseline -> amyloid m24
               size = 1, linejoin = "mitre", lineend = "butt",
               arrow = arrow(length = unit(1, "mm"), type= "closed")) +
  geom_segment(x = 30, xend = 70, y = 30, yend = 70, # cog baseline -> amyloid m24
               size = 1, linejoin = "mitre", lineend = "butt",
               arrow = arrow(length = unit(1, "mm"), type= "closed")) +
  geom_segment(x = 30, xend = 70, y = 70, yend = 30, # amyloid baseline -> cog m24
               size = 1, linejoin = "mitre", lineend = "butt",
               arrow = arrow(length = unit(1, "mm"), type= "closed")) +
  # Add results
  annotate('text', x = 18, y = 50, label = bl_amyloid_to_bl_cog, size = 2.5) +
  annotate('text', x = 82, y = 50, label = m24_amyloid_to_m24_cog, size = 2.5) +
  annotate('text', x = 50, y = 82, label = bl_amyloid_to_m24_amyloid, size = 2.5) +
  annotate('text', x = 50, y = 18, label = bl_cog_to_m24_cog, size = 2.5) +
  annotate('text', x = 45, y = 65, label = bl_amyloid_to_m24_cog, size = 2.5) +
  annotate('text', x = 45, y = 34, label = bl_cog_to_m24_amyloid, size = 2.5)

}

### ADNI MEM FIGURES

## Get estimates

engage_adnimem_bl_amyloid_bl_cog <- paste0("Est. = ", engage_amyloidbaseline_to_adnimembaseline.beta)
                         #                   , "\nSE = ",
                         # engage_amyloidbaseline_to_adnimembaseline.se, "\np ", engage_amyloidbaseline_to_adnimembaseline.pvalue)

engage_adnimem_m24_amyloid_m24_cog <- paste0("Est. = ", engage_amyloidm24_to_adnimemm24.beta)
# , "\nSE = ",
#                                              engage_amyloidm24_to_adnimemm24.se, "\np = ", engage_amyloidm24_to_adnimemm24.pvalue)

engage_adnimem_bl_amyloid_m24_amyloid <- paste0("Est. = ", engage_amyloidbaseline_to_amyloidm24.beta)
# , "\nSE = ",
#                                                 engage_amyloidbaseline_to_amyloidm24.se, "\np ", engage_amyloidbaseline_to_amyloidm24.pvalue)

engage_adnimem_bl_cog_m24_cog <- paste0("Est. = ", engage_adnimembaseline_to_adnimemm24.beta)
# , "\nSE = ",
#                                         engage_adnimembaseline_to_adnimemm24.se, "\np ", engage_adnimembaseline_to_adnimemm24.pvalue)

engage_adnimem_bl_amyloid_m24_cog <- paste0("Est. = ", engage_amyloidbaseline_to_adnimem24.beta)
# , "\nSE = ",
#                                             engage_amyloidbaseline_to_adnimem24.se, "\np = ", engage_amyloidbaseline_to_adnimem24.pvalue)

engage_adnimem_bl_cog_m24_amyloid <- paste0("Est. = ", engage_adnimembaseline_to_amyloidm24.beta)
# , "\nFixed")
                                            # , "\nSE = ",
                                            # engage_adnimembaseline_to_amyloidm24.se, "\np ", engage_adnimembaseline_to_amyloidm24.pvalue)


engage_adnimem_figure <- generate_clpm_figure(bl_amyloid_to_bl_cog      = engage_adnimem_bl_amyloid_bl_cog,
                                              m24_amyloid_to_m24_cog    = engage_adnimem_m24_amyloid_m24_cog,
                                              bl_amyloid_to_m24_amyloid = engage_adnimem_bl_amyloid_m24_amyloid,
                                              bl_cog_to_m24_cog         = engage_adnimem_bl_cog_m24_cog,
                                              bl_amyloid_to_m24_cog     = engage_adnimem_bl_amyloid_m24_cog,
                                              bl_cog_to_m24_amyloid     = engage_adnimem_bl_cog_m24_amyloid)
engage_adnimem_figure

donanemab_adnimem_bl_amyloid_bl_cog <- paste0("Est. = ", donanemab_amyloidbaseline_to_adnimembaseline.beta)
                                           #    , "\nSE = ",
                                           # donanemab_amyloidbaseline_to_adnimembaseline.se, "\np ", donanemab_amyloidbaseline_to_adnimembaseline.pvalue)

donanemab_adnimem_m24_amyloid_m24_cog <- paste0("Est. = ", donanemab_amyloidm24_to_adnimemm24.beta)
# , "\nSE = ",
#                                              donanemab_amyloidm24_to_adnimemm24.se, "\np = ", donanemab_amyloidm24_to_adnimemm24.pvalue)

donanemab_adnimem_bl_amyloid_m24_amyloid <- paste0("Est. = ", donanemab_amyloidbaseline_to_amyloidm24.beta)
# , "\nSE = ",
#                                                 donanemab_amyloidbaseline_to_amyloidm24.se, "\np ", donanemab_amyloidbaseline_to_amyloidm24.pvalue)

donanemab_adnimem_bl_cog_m24_cog <- paste0("Est. = ", donanemab_adnimembaseline_to_adnimemm24.beta)
# , "\nSE = ",
#                                         donanemab_adnimembaseline_to_adnimemm24.se, "\np ", donanemab_adnimembaseline_to_adnimemm24.pvalue)

donanemab_adnimem_bl_amyloid_m24_cog <- paste0("Est. = ", donanemab_amyloidbaseline_to_adnimem24.beta)
# , "\nSE = ",
#                                             donanemab_amyloidbaseline_to_adnimem24.se, "\np = ", donanemab_amyloidbaseline_to_adnimem24.pvalue)

donanemab_adnimem_bl_cog_m24_amyloid <- paste0("Est. = ", donanemab_adnimembaseline_to_amyloidm24.beta)
# , "\n Fixed")
# \nSE = ",
#                                             donanemab_adnimembaseline_to_amyloidm24.se, "\np ", donanemab_adnimembaseline_to_amyloidm24.pvalue)


donanemab_adnimem_figure <- generate_clpm_figure(bl_amyloid_to_bl_cog      = donanemab_adnimem_bl_amyloid_bl_cog,
                                              m24_amyloid_to_m24_cog    = donanemab_adnimem_m24_amyloid_m24_cog,
                                              bl_amyloid_to_m24_amyloid = donanemab_adnimem_bl_amyloid_m24_amyloid,
                                              bl_cog_to_m24_cog         = donanemab_adnimem_bl_cog_m24_cog,
                                              bl_amyloid_to_m24_cog     = donanemab_adnimem_bl_amyloid_m24_cog,
                                              bl_cog_to_m24_amyloid     = donanemab_adnimem_bl_cog_m24_amyloid)
donanemab_adnimem_figure

shouldbe_adnimem_bl_amyloid_bl_cog <- paste0("Est. = ", shouldbe_amyloidbaseline_to_adnimembaseline.beta)
# , "\nSE = ",
#                                            shouldbe_amyloidbaseline_to_adnimembaseline.se, "\np = ", shouldbe_amyloidbaseline_to_adnimembaseline.pvalue)

shouldbe_adnimem_m24_amyloid_m24_cog <- paste0("Est. = ", shouldbe_amyloidm24_to_adnimemm24.beta)
# , "\nSE = ",
#                                              shouldbe_amyloidm24_to_adnimemm24.se, "\np = ", shouldbe_amyloidm24_to_adnimemm24.pvalue)

shouldbe_adnimem_bl_amyloid_m24_amyloid <- paste0("Est. = ", shouldbe_amyloidbaseline_to_amyloidm24.beta)
# , "\nSE = ",
#                                                 shouldbe_amyloidbaseline_to_amyloidm24.se, "\np ", shouldbe_amyloidbaseline_to_amyloidm24.pvalue)

shouldbe_adnimem_bl_cog_m24_cog <- paste0("Est. = ", shouldbe_adnimembaseline_to_adnimemm24.beta)
# , "\nSE = ",
#                                         shouldbe_adnimembaseline_to_adnimemm24.se, "\np ", shouldbe_adnimembaseline_to_adnimemm24.pvalue)

shouldbe_adnimem_bl_amyloid_m24_cog <- paste0("Est. = ", shouldbe_amyloidbaseline_to_adnimem24.beta)
# , "\nSE = ",
#                                             shouldbe_amyloidbaseline_to_adnimem24.se, "\np = ", shouldbe_amyloidbaseline_to_adnimem24.pvalue)

shouldbe_adnimem_bl_cog_m24_amyloid <- paste0("Est. = ", shouldbe_adnimembaseline_to_amyloidm24.beta)
# , "\n Fixed")
                                            #   ,
                                            # shouldbe_adnimembaseline_to_amyloidm24.se, "\np ", shouldbe_adnimembaseline_to_amyloidm24.pvalue)


shouldbe_adnimem_figure <- generate_clpm_figure(bl_amyloid_to_bl_cog      = shouldbe_adnimem_bl_amyloid_bl_cog,
                                              m24_amyloid_to_m24_cog    = shouldbe_adnimem_m24_amyloid_m24_cog,
                                              bl_amyloid_to_m24_amyloid = shouldbe_adnimem_bl_amyloid_m24_amyloid,
                                              bl_cog_to_m24_cog         = shouldbe_adnimem_bl_cog_m24_cog,
                                              bl_amyloid_to_m24_cog     = shouldbe_adnimem_bl_amyloid_m24_cog,
                                              bl_cog_to_m24_amyloid     = shouldbe_adnimem_bl_cog_m24_amyloid)
shouldbe_adnimem_figure

### ADNI EF FIGURES

engage_adnief_bl_amyloid_bl_cog <- paste0("Est. = ", engage_amyloidbaseline_to_adniefbaseline.beta)
# , "\nSE = ",
#                                            engage_amyloidbaseline_to_adniefbaseline.se, "\np ", engage_amyloidbaseline_to_adniefbaseline.pvalue)

engage_adnief_m24_amyloid_m24_cog <- paste0("Est. = ", engage_amyloidm24_to_adniefm24.beta)
# , "\nSE = ",
#                                              engage_amyloidm24_to_adniefm24.se, "\np = ", engage_amyloidm24_to_adniefm24.pvalue)

engage_adnief_bl_amyloid_m24_amyloid <- paste0("Est. = ", engage_amyloidbaseline_to_amyloidm24.beta)
# , "\nSE = ",
#                                                 engage_amyloidbaseline_to_amyloidm24.se, "\np ", engage_amyloidbaseline_to_amyloidm24.pvalue)

engage_adnief_bl_cog_m24_cog <- paste0("Est. = ", engage_adniefbaseline_to_adniefm24.beta)
# , "\nSE = ",
#                                         engage_adniefbaseline_to_adniefm24.se, "\np ", engage_adniefbaseline_to_adniefm24.pvalue)

engage_adnief_bl_amyloid_m24_cog <- paste0("Est. = ", engage_amyloidbaseline_to_adnief24.beta)
# , "\nSE = ",
#                                             engage_amyloidbaseline_to_adnief24.se, "\np = ", engage_amyloidbaseline_to_adnief24.pvalue)

engage_adnief_bl_cog_m24_amyloid <- paste0("Est. = ", engage_adniefbaseline_to_amyloidm24.beta)
# , "\n Fixed")
#                                            # "\nSE = ",
                                           #  engage_adniefbaseline_to_amyloidm24.se, "\np ", engage_adniefbaseline_to_amyloidm24.pvalue)


engage_adnief_figure <- generate_clpm_figure(bl_amyloid_to_bl_cog      = engage_adnief_bl_amyloid_bl_cog,
                                              m24_amyloid_to_m24_cog    = engage_adnief_m24_amyloid_m24_cog,
                                              bl_amyloid_to_m24_amyloid = engage_adnief_bl_amyloid_m24_amyloid,
                                              bl_cog_to_m24_cog         = engage_adnief_bl_cog_m24_cog,
                                              bl_amyloid_to_m24_cog     = engage_adnief_bl_amyloid_m24_cog,
                                              bl_cog_to_m24_amyloid     = engage_adnief_bl_cog_m24_amyloid)
engage_adnief_figure

donanemab_adnief_bl_amyloid_bl_cog <- paste0("Est. = ", donanemab_amyloidbaseline_to_adniefbaseline.beta)
# , "\nSE = ",
#                                           donanemab_amyloidbaseline_to_adniefbaseline.se, "\np ", donanemab_amyloidbaseline_to_adniefbaseline.pvalue)

donanemab_adnief_m24_amyloid_m24_cog <- paste0("Est. = ", donanemab_amyloidm24_to_adniefm24.beta)
# , "\nSE = ",
#                                             donanemab_amyloidm24_to_adniefm24.se, "\np = ", donanemab_amyloidm24_to_adniefm24.pvalue)

donanemab_adnief_bl_amyloid_m24_amyloid <- paste0("Est. = ", donanemab_amyloidbaseline_to_amyloidm24.beta)
# , "\nSE = ",
#                                                donanemab_amyloidbaseline_to_amyloidm24.se, "\np ", donanemab_amyloidbaseline_to_amyloidm24.pvalue)

donanemab_adnief_bl_cog_m24_cog <- paste0("Est. = ", donanemab_adniefbaseline_to_adniefm24.beta)
# , "\nSE = ",
#                                        donanemab_adniefbaseline_to_adniefm24.se, "\np ", donanemab_adniefbaseline_to_adniefm24.pvalue)

donanemab_adnief_bl_amyloid_m24_cog <- paste0("Est. = ", donanemab_amyloidbaseline_to_adnief24.beta)
# , "\nSE = ",
#                                            donanemab_amyloidbaseline_to_adnief24.se, "\np = ", donanemab_amyloidbaseline_to_adnief24.pvalue)

donanemab_adnief_bl_cog_m24_amyloid <- paste0("Est. = ", donanemab_adniefbaseline_to_amyloidm24.beta)
# , "\n Fixed")
                                           #    "\nSE = ",
                                           # donanemab_adniefbaseline_to_amyloidm24.se, "\np ", donanemab_adniefbaseline_to_amyloidm24.pvalue)

donanemab_adnief_figure <- generate_clpm_figure(bl_amyloid_to_bl_cog      = donanemab_adnief_bl_amyloid_bl_cog,
                                             m24_amyloid_to_m24_cog    = donanemab_adnief_m24_amyloid_m24_cog,
                                             bl_amyloid_to_m24_amyloid = donanemab_adnief_bl_amyloid_m24_amyloid,
                                             bl_cog_to_m24_cog         = donanemab_adnief_bl_cog_m24_cog,
                                             bl_amyloid_to_m24_cog     = donanemab_adnief_bl_amyloid_m24_cog,
                                             bl_cog_to_m24_amyloid     = donanemab_adnief_bl_cog_m24_amyloid)
donanemab_adnief_figure


shouldbe_adnief_bl_amyloid_bl_cog <- paste0("Est. = ", shouldbe_amyloidbaseline_to_adniefbaseline.beta)
# , "\nSE = ",
#                                              shouldbe_amyloidbaseline_to_adniefbaseline.se, "\np ", shouldbe_amyloidbaseline_to_adniefbaseline.pvalue)

shouldbe_adnief_m24_amyloid_m24_cog <- paste0("Est. = ", shouldbe_amyloidm24_to_adniefm24.beta)
# , "\nSE = ",
#                                                shouldbe_amyloidm24_to_adniefm24.se, "\np = ", shouldbe_amyloidm24_to_adniefm24.pvalue)

shouldbe_adnief_bl_amyloid_m24_amyloid <- paste0("Est. = ", shouldbe_amyloidbaseline_to_amyloidm24.beta)
# , "\nSE = ",
#                                                   shouldbe_amyloidbaseline_to_amyloidm24.se, "\np ", shouldbe_amyloidbaseline_to_amyloidm24.pvalue)

shouldbe_adnief_bl_cog_m24_cog <- paste0("Est. = ", shouldbe_adniefbaseline_to_adniefm24.beta)
# , "\nSE = ",
#                                           shouldbe_adniefbaseline_to_adniefm24.se, "\np ", shouldbe_adniefbaseline_to_adniefm24.pvalue)

shouldbe_adnief_bl_amyloid_m24_cog <- paste0("Est. = ", shouldbe_amyloidbaseline_to_adnief24.beta)
# , "\nSE = ",
#                                               shouldbe_amyloidbaseline_to_adnief24.se, "\np = ", shouldbe_amyloidbaseline_to_adnief24.pvalue)

shouldbe_adnief_bl_cog_m24_amyloid <- paste0("Est. = ", shouldbe_adniefbaseline_to_amyloidm24.beta)
# , "\n Fixed")

shouldbe_adnief_figure <- generate_clpm_figure(bl_amyloid_to_bl_cog      = shouldbe_adnief_bl_amyloid_bl_cog,
                                                m24_amyloid_to_m24_cog    = shouldbe_adnief_m24_amyloid_m24_cog,
                                                bl_amyloid_to_m24_amyloid = shouldbe_adnief_bl_amyloid_m24_amyloid,
                                                bl_cog_to_m24_cog         = shouldbe_adnief_bl_cog_m24_cog,
                                                bl_amyloid_to_m24_cog     = shouldbe_adnief_bl_amyloid_m24_cog,
                                                bl_cog_to_m24_amyloid     = shouldbe_adnief_bl_cog_m24_amyloid)

### ADAS13 FIGURES

engage_adas13_bl_amyloid_bl_cog <- paste0("Est. = ", engage_amyloidbaseline_to_adas13baseline.beta)
# , "\nSE = ",
#                                             engage_amyloidbaseline_to_adas13baseline.se, "\np ", engage_amyloidbaseline_to_adas13baseline.pvalue)

engage_adas13_m24_amyloid_m24_cog <- paste0("Est. = ", engage_amyloidm24_to_adas13m24.beta)
# , "\nSE = ",
#                                               engage_amyloidm24_to_adas13m24.se, "\np = ", engage_amyloidm24_to_adas13m24.pvalue)

engage_adas13_bl_amyloid_m24_amyloid <- paste0("Est. = ", engage_amyloidbaseline_to_amyloidm24.beta)
# , "\nSE = ",
#                                                  engage_amyloidbaseline_to_amyloidm24.se, "\np ", engage_amyloidbaseline_to_amyloidm24.pvalue)

engage_adas13_bl_cog_m24_cog <- paste0("Est. = ", engage_adas13baseline_to_adas13m24.beta)
# , "\nSE = ",
#                                          engage_adas13baseline_to_adas13m24.se, "\np ", engage_adas13baseline_to_adas13m24.pvalue)

engage_adas13_bl_amyloid_m24_cog <- paste0("Est. = ", engage_amyloidbaseline_to_adas1324.beta)
# , "\nSE = ",
#                                              engage_amyloidbaseline_to_adas1324.se, "\np = ", engage_amyloidbaseline_to_adas1324.pvalue)

engage_adas13_bl_cog_m24_amyloid <- paste0("Est. = ", engage_adas13baseline_to_amyloidm24.beta)
# , "\n Fixed")

engage_adas13_figure <- generate_clpm_figure(bl_amyloid_to_bl_cog       = engage_adas13_bl_amyloid_bl_cog,
                                              m24_amyloid_to_m24_cog    = engage_adas13_m24_amyloid_m24_cog,
                                              bl_amyloid_to_m24_amyloid = engage_adas13_bl_amyloid_m24_amyloid,
                                              bl_cog_to_m24_cog         = engage_adas13_bl_cog_m24_cog,
                                              bl_amyloid_to_m24_cog     = engage_adas13_bl_amyloid_m24_cog,
                                              bl_cog_to_m24_amyloid     = engage_adas13_bl_cog_m24_amyloid)

donanemab_adas13_bl_amyloid_bl_cog <- paste0("Est. = ", donanemab_amyloidbaseline_to_adas13baseline.beta)
# , "\nSE = ",
#                                           donanemab_amyloidbaseline_to_adas13baseline.se, "\np ", donanemab_amyloidbaseline_to_adas13baseline.pvalue)

donanemab_adas13_m24_amyloid_m24_cog <- paste0("Est. = ", donanemab_amyloidm24_to_adas13m24.beta)
# , "\nSE = ",
#                                             donanemab_amyloidm24_to_adas13m24.se, "\np = ", donanemab_amyloidm24_to_adas13m24.pvalue)

donanemab_adas13_bl_amyloid_m24_amyloid <- paste0("Est. = ", donanemab_amyloidbaseline_to_amyloidm24.beta)
# , "\nSE = ",
#                                                donanemab_amyloidbaseline_to_amyloidm24.se, "\np ", donanemab_amyloidbaseline_to_amyloidm24.pvalue)

donanemab_adas13_bl_cog_m24_cog <- paste0("Est. = ", donanemab_adas13baseline_to_adas13m24.beta)
# , "\nSE = ",
#                                        donanemab_adas13baseline_to_adas13m24.se, "\np ", donanemab_adas13baseline_to_adas13m24.pvalue)

donanemab_adas13_bl_amyloid_m24_cog <- paste0("Est. = ", donanemab_amyloidbaseline_to_adas1324.beta)
# , "\nSE = ",
#                                            donanemab_amyloidbaseline_to_adas1324.se, "\np = ", donanemab_amyloidbaseline_to_adas1324.pvalue)

donanemab_adas13_bl_cog_m24_amyloid <- paste0("Est. = ", donanemab_adas13baseline_to_amyloidm24.beta)
# , "\n Fixed")

donanemab_adas13_figure <- generate_clpm_figure(bl_amyloid_to_bl_cog       = donanemab_adas13_bl_amyloid_bl_cog,
                                             m24_amyloid_to_m24_cog    = donanemab_adas13_m24_amyloid_m24_cog,
                                             bl_amyloid_to_m24_amyloid = donanemab_adas13_bl_amyloid_m24_amyloid,
                                             bl_cog_to_m24_cog         = donanemab_adas13_bl_cog_m24_cog,
                                             bl_amyloid_to_m24_cog     = donanemab_adas13_bl_amyloid_m24_cog,
                                             bl_cog_to_m24_amyloid     = donanemab_adas13_bl_cog_m24_amyloid)

shouldbe_adas13_bl_amyloid_bl_cog <- paste0("Est. = ", shouldbe_amyloidbaseline_to_adas13baseline.beta)
# , "\nSE = ",
#                                              shouldbe_amyloidbaseline_to_adas13baseline.se, "\np ", shouldbe_amyloidbaseline_to_adas13baseline.pvalue)

shouldbe_adas13_m24_amyloid_m24_cog <- paste0("Est. = ", shouldbe_amyloidm24_to_adas13m24.beta)
# , "\nSE = ",
#                                                shouldbe_amyloidm24_to_adas13m24.se, "\np = ", shouldbe_amyloidm24_to_adas13m24.pvalue)

shouldbe_adas13_bl_amyloid_m24_amyloid <- paste0("Est. = ", shouldbe_amyloidbaseline_to_amyloidm24.beta)
# , "\nSE = ",
#                                                   shouldbe_amyloidbaseline_to_amyloidm24.se, "\np ", shouldbe_amyloidbaseline_to_amyloidm24.pvalue)

shouldbe_adas13_bl_cog_m24_cog <- paste0("Est. = ", shouldbe_adas13baseline_to_adas13m24.beta)
# , "\nSE = ",
#                                           shouldbe_adas13baseline_to_adas13m24.se, "\np ", shouldbe_adas13baseline_to_adas13m24.pvalue)

shouldbe_adas13_bl_amyloid_m24_cog <- paste0("Est. = ", shouldbe_amyloidbaseline_to_adas1324.beta)
# , "\nSE = ",
#                                               shouldbe_amyloidbaseline_to_adas1324.se, "\np = ", shouldbe_amyloidbaseline_to_adas1324.pvalue)

shouldbe_adas13_bl_cog_m24_amyloid <- paste0("Est. = ", shouldbe_adas13baseline_to_amyloidm24.beta)
# , "\n Fixed")

shouldbe_adas13_figure <- generate_clpm_figure(bl_amyloid_to_bl_cog       = shouldbe_adas13_bl_amyloid_bl_cog,
                                                m24_amyloid_to_m24_cog    = shouldbe_adas13_m24_amyloid_m24_cog,
                                                bl_amyloid_to_m24_amyloid = shouldbe_adas13_bl_amyloid_m24_amyloid,
                                                bl_cog_to_m24_cog         = shouldbe_adas13_bl_cog_m24_cog,
                                                bl_amyloid_to_m24_cog     = shouldbe_adas13_bl_amyloid_m24_cog,
                                                bl_cog_to_m24_amyloid     = shouldbe_adas13_bl_cog_m24_amyloid)

