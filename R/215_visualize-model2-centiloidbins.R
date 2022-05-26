rm(list = setdiff(ls(), lsf.str())[!(setdiff(ls(), lsf.str()) %in% 'params')])
source(here::here('R', '002_folder-paths-and-options.R'))
source(here::here('R', '005_libraries.R'))

# Import data

engage_adnimem_results <-    readRDS(here::here("R_objects", "210_engage_adnimem.RDS"))
engage_adnief_results <-     readRDS(here::here("R_objects", "210_engage_adnief.RDS"))
engage_adas13_results <-   readRDS(here::here("R_objects", "210_engage_adas13.RDS"))
donanemab_adnimem_results <- readRDS(here::here("R_objects", "210_donanemab_adnimem.RDS"))
donanemab_adnief_results <- readRDS(here::here("R_objects", "210_donanemab_adnief.RDS"))
donanemab_adas13_results <-  readRDS(here::here("R_objects", "210_donanemab_adas13.RDS"))
shouldbe_adnimem_results <-  readRDS(here::here("R_objects", "210_shouldbe_adnimem.RDS"))
shouldbe_adnief_results <-   readRDS(here::here("R_objects", "210_shouldbe_adnief.RDS"))
shouldbe_adas13_results <-   readRDS(here::here("R_objects", "210_shouldbe_adas13.RDS"))

# Pull results

## ENGAGE - ADNIMEM

engage_adnimembaseline_to_adnimemm24 <- engage_adnimem_results %>%
  filter(lhs == "m24_adni_mem", rhs == "bl_adni_mem") %>%
  select(est.std) %>%
  pull() %>%
  round(2)

engage_amyloidbaseline_to_adnimem24 <- engage_adnimem_results %>%
  filter(lhs == "m24_adni_mem", rhs == "bl_composite") %>%
  select(est.std) %>%
  pull() %>%
  round(2)

engage_adnimembaseline_to_amyloidm24 <- engage_adnimem_results %>%
  filter(lhs == "m24_composite", rhs == "bl_adni_mem") %>%
  select(est.std) %>%
  pull() %>%
  round(2)

engage_amyloidbaseline_to_amyloidm24 <- engage_adnimem_results %>%
  filter(lhs == "m24_composite", rhs == "bl_composite") %>%
  select(est.std) %>%
  pull %>%
  round(2)

engage_amyloidm24_to_adnimemm24 <- engage_adnimem_results %>%
  filter(lhs == "m24_adni_mem", rhs == "m24_composite") %>%
  select(est.std) %>%
  pull %>%
  round(2)

engage_amyloidbaseline_to_adnimembaseline <- engage_adnimem_results %>%
  filter(lhs == "bl_adni_mem", rhs == "bl_composite") %>%
  select(est.std) %>%
  pull %>%
  round(2)

## DONANEMAB - ADNIMEM

donanemab_adnimembaseline_to_adnimemm24 <- donanemab_adnimem_results %>%
  filter(lhs == "m24_adni_mem", rhs == "bl_adni_mem") %>%
  select(est.std) %>%
  pull() %>%
  round(2)

donanemab_amyloidbaseline_to_adnimem24 <- donanemab_adnimem_results %>%
  filter(lhs == "m24_adni_mem", rhs == "bl_composite") %>%
  select(est.std) %>%
  pull() %>%
  round(2)

donanemab_adnimembaseline_to_amyloidm24 <- donanemab_adnimem_results %>%
  filter(lhs == "m24_composite", rhs == "bl_adni_mem") %>%
  select(est.std) %>%
  pull() %>%
  round(2)

donanemab_amyloidbaseline_to_amyloidm24 <- donanemab_adnimem_results %>%
  filter(lhs == "m24_composite", rhs == "bl_composite") %>%
  select(est.std) %>%
  pull %>%
  round(2)

donanemab_amyloidm24_to_adnimemm24 <- donanemab_adnimem_results %>%
  filter(lhs == "m24_adni_mem", rhs == "m24_composite") %>%
  select(est.std) %>%
  pull %>%
  round(2)

donanemab_amyloidbaseline_to_adnimembaseline <- donanemab_adnimem_results %>%
  filter(lhs == "bl_adni_mem", rhs == "bl_composite") %>%
  select(est.std) %>%
  pull %>%
  round(2)

## shouldbe - ADNIMEM

shouldbe_adnimembaseline_to_adnimemm24 <- shouldbe_adnimem_results %>%
  filter(lhs == "m24_adni_mem", rhs == "bl_adni_mem") %>%
  select(est.std) %>%
  pull() %>%
  round(2)

shouldbe_amyloidbaseline_to_adnimem24 <- shouldbe_adnimem_results %>%
  filter(lhs == "m24_adni_mem", rhs == "bl_composite") %>%
  select(est.std) %>%
  pull() %>%
  round(2)

shouldbe_adnimembaseline_to_amyloidm24 <- shouldbe_adnimem_results %>%
  filter(lhs == "m24_composite", rhs == "bl_adni_mem") %>%
  select(est.std) %>%
  pull() %>%
  round(2)

shouldbe_amyloidbaseline_to_amyloidm24 <- shouldbe_adnimem_results %>%
  filter(lhs == "m24_composite", rhs == "bl_composite") %>%
  select(est.std) %>%
  pull %>%
  round(2)

shouldbe_amyloidm24_to_adnimemm24 <- shouldbe_adnimem_results %>%
  filter(lhs == "m24_adni_mem", rhs == "m24_composite") %>%
  select(est.std) %>%
  pull %>%
  round(2)

shouldbe_amyloidbaseline_to_adnimembaseline <- shouldbe_adnimem_results %>%
  filter(lhs == "bl_adni_mem", rhs == "bl_composite") %>%
  select(est.std) %>%
  pull %>%
  round(2)

## ENGAGE - adnief

engage_adniefbaseline_to_adniefm24 <- engage_adnief_results %>%
  filter(lhs == "m24_adni_ef", rhs == "bl_adni_ef") %>%
  select(est.std) %>%
  pull() %>%
  round(2)

engage_amyloidbaseline_to_adnief24 <- engage_adnief_results %>%
  filter(lhs == "m24_adni_ef", rhs == "bl_composite") %>%
  select(est.std) %>%
  pull() %>%
  round(2)

engage_adniefbaseline_to_amyloidm24 <- engage_adnief_results %>%
  filter(lhs == "m24_composite", rhs == "bl_adni_ef") %>%
  select(est.std) %>%
  pull() %>%
  round(2)

engage_amyloidbaseline_to_amyloidm24 <- engage_adnief_results %>%
  filter(lhs == "m24_composite", rhs == "bl_composite") %>%
  select(est.std) %>%
  pull %>%
  round(2)

engage_amyloidm24_to_adniefm24 <- engage_adnief_results %>%
  filter(lhs == "m24_adni_ef", rhs == "m24_composite") %>%
  select(est.std) %>%
  pull %>%
  round(2)

engage_amyloidbaseline_to_adniefbaseline <- engage_adnief_results %>%
  filter(lhs == "bl_adni_ef", rhs == "bl_composite") %>%
  select(est.std) %>%
  pull %>%
  round(2)

## DONANEMAB - adnief

donanemab_adniefbaseline_to_adniefm24 <- donanemab_adnief_results %>%
  filter(lhs == "m24_adni_ef", rhs == "bl_adni_ef") %>%
  select(est.std) %>%
  pull() %>%
  round(2)

donanemab_amyloidbaseline_to_adnief24 <- donanemab_adnief_results %>%
  filter(lhs == "m24_adni_ef", rhs == "bl_composite") %>%
  select(est.std) %>%
  pull() %>%
  round(2)

donanemab_adniefbaseline_to_amyloidm24 <- donanemab_adnief_results %>%
  filter(lhs == "m24_composite", rhs == "bl_adni_ef") %>%
  select(est.std) %>%
  pull() %>%
  round(2)

donanemab_amyloidbaseline_to_amyloidm24 <- donanemab_adnief_results %>%
  filter(lhs == "m24_composite", rhs == "bl_composite") %>%
  select(est.std) %>%
  pull %>%
  round(2)

donanemab_amyloidm24_to_adniefm24 <- donanemab_adnief_results %>%
  filter(lhs == "m24_adni_ef", rhs == "m24_composite") %>%
  select(est.std) %>%
  pull %>%
  round(2)

donanemab_amyloidbaseline_to_adniefbaseline <- donanemab_adnief_results %>%
  filter(lhs == "bl_adni_ef", rhs == "bl_composite") %>%
  select(est.std) %>%
  pull %>%
  round(2)

## shouldbe - adnief

shouldbe_adniefbaseline_to_adniefm24 <- shouldbe_adnief_results %>%
  filter(lhs == "m24_adni_ef", rhs == "bl_adni_ef") %>%
  select(est.std) %>%
  pull() %>%
  round(2)

shouldbe_amyloidbaseline_to_adnief24 <- shouldbe_adnief_results %>%
  filter(lhs == "m24_adni_ef", rhs == "bl_composite") %>%
  select(est.std) %>%
  pull() %>%
  round(2)

shouldbe_adniefbaseline_to_amyloidm24 <- shouldbe_adnief_results %>%
  filter(lhs == "m24_composite", rhs == "bl_adni_ef") %>%
  select(est.std) %>%
  pull() %>%
  round(2)

shouldbe_amyloidbaseline_to_amyloidm24 <- shouldbe_adnief_results %>%
  filter(lhs == "m24_composite", rhs == "bl_composite") %>%
  select(est.std) %>%
  pull %>%
  round(2)

shouldbe_amyloidm24_to_adniefm24 <- shouldbe_adnief_results %>%
  filter(lhs == "m24_adni_ef", rhs == "m24_composite") %>%
  select(est.std) %>%
  pull %>%
  round(2)

shouldbe_amyloidbaseline_to_adniefbaseline <- shouldbe_adnief_results %>%
  filter(lhs == "bl_adni_ef", rhs == "bl_composite") %>%
  select(est.std) %>%
  pull %>%
  round(2)

## ENGAGE - adas13

engage_adas13baseline_to_adas13m24 <- engage_adas13_results %>%
  filter(lhs == "m24_adas13", rhs == "bl_adas13") %>%
  select(est.std) %>%
  pull() %>%
  round(2)

engage_amyloidbaseline_to_adas1324 <- engage_adas13_results %>%
  filter(lhs == "m24_adas13", rhs == "bl_composite") %>%
  select(est.std) %>%
  pull() %>%
  round(2)

engage_adas13baseline_to_amyloidm24 <- engage_adas13_results %>%
  filter(lhs == "m24_composite", rhs == "bl_adas13") %>%
  select(est.std) %>%
  pull() %>%
  round(2)

engage_amyloidbaseline_to_amyloidm24 <- engage_adas13_results %>%
  filter(lhs == "m24_composite", rhs == "bl_composite") %>%
  select(est.std) %>%
  pull %>%
  round(2)

engage_amyloidm24_to_adas13m24 <- engage_adas13_results %>%
  filter(lhs == "m24_adas13", rhs == "m24_composite") %>%
  select(est.std) %>%
  pull %>%
  round(2)

engage_amyloidbaseline_to_adas13baseline <- engage_adas13_results %>%
  filter(lhs == "bl_adas13", rhs == "bl_composite") %>%
  select(est.std) %>%
  pull %>%
  round(2)

## DONANEMAB - adas13

donanemab_adas13baseline_to_adas13m24 <- donanemab_adas13_results %>%
  filter(lhs == "m24_adas13", rhs == "bl_adas13") %>%
  select(est.std) %>%
  pull() %>%
  round(2)

donanemab_amyloidbaseline_to_adas1324 <- donanemab_adas13_results %>%
  filter(lhs == "m24_adas13", rhs == "bl_composite") %>%
  select(est.std) %>%
  pull() %>%
  round(2)

donanemab_adas13baseline_to_amyloidm24 <- donanemab_adas13_results %>%
  filter(lhs == "m24_composite", rhs == "bl_adas13") %>%
  select(est.std) %>%
  pull() %>%
  round(2)

donanemab_amyloidbaseline_to_amyloidm24 <- donanemab_adas13_results %>%
  filter(lhs == "m24_composite", rhs == "bl_composite") %>%
  select(est.std) %>%
  pull %>%
  round(2)

donanemab_amyloidm24_to_adas13m24 <- donanemab_adas13_results %>%
  filter(lhs == "m24_adas13", rhs == "m24_composite") %>%
  select(est.std) %>%
  pull %>%
  round(2)

donanemab_amyloidbaseline_to_adas13baseline <- donanemab_adas13_results %>%
  filter(lhs == "bl_adas13", rhs == "bl_composite") %>%
  select(est.std) %>%
  pull %>%
  round(2)

## shouldbe - adas13

shouldbe_adas13baseline_to_adas13m24 <- shouldbe_adas13_results %>%
  filter(lhs == "m24_adas13", rhs == "bl_adas13") %>%
  select(est.std) %>%
  pull() %>%
  round(2)

shouldbe_amyloidbaseline_to_adas1324 <- shouldbe_adas13_results %>%
  filter(lhs == "m24_adas13", rhs == "bl_composite") %>%
  select(est.std) %>%
  pull() %>%
  round(2)

shouldbe_adas13baseline_to_amyloidm24 <- shouldbe_adas13_results %>%
  filter(lhs == "m24_composite", rhs == "bl_adas13") %>%
  select(est.std) %>%
  pull() %>%
  round(2)

shouldbe_amyloidbaseline_to_amyloidm24 <- shouldbe_adas13_results %>%
  filter(lhs == "m24_composite", rhs == "bl_composite") %>%
  select(est.std) %>%
  pull %>%
  round(2)

shouldbe_amyloidm24_to_adas13m24 <- shouldbe_adas13_results %>%
  filter(lhs == "m24_adas13", rhs == "m24_composite") %>%
  select(est.std) %>%
  pull %>%
  round(2)

shouldbe_amyloidbaseline_to_adas13baseline <- shouldbe_adas13_results %>%
  filter(lhs == "bl_adas13", rhs == "bl_composite") %>%
  select(est.std) %>%
  pull %>%
  round(2)

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
               size = .15, linejoin = "mitre", lineend = "butt",
               arrow = arrow(length = unit(1, "mm"), type= "closed")) +
  geom_segment(x = 75, xend = 75, y = 70, yend = 30, # amyloid m24 -> cog m24
               size = .15, linejoin = "mitre", lineend = "butt",
               arrow = arrow(length = unit(1, "mm"), type= "closed")) +
  geom_segment(x = 30, xend = 70, y = 25, yend = 25, # cog baseline -> cog m24
               size = .15, linejoin = "mitre", lineend = "butt",
               arrow = arrow(length = unit(1, "mm"), type= "closed")) +
  geom_segment(x = 30, xend = 70, y = 75, yend = 75, # amyloid baseline -> amyloid m24
               size = .15, linejoin = "mitre", lineend = "butt",
               arrow = arrow(length = unit(1, "mm"), type= "closed")) +
  geom_segment(x = 30, xend = 70, y = 30, yend = 70, # cog baseline -> amyloid m24
               size = .15, linejoin = "mitre", lineend = "butt",
               arrow = arrow(length = unit(1, "mm"), type= "closed")) +
  geom_segment(x = 30, xend = 70, y = 70, yend = 30, # amyloid baseline -> cog m24
               size = .15, linejoin = "mitre", lineend = "butt",
               arrow = arrow(length = unit(1, "mm"), type= "closed")) +
  # Add results
  annotate('text', x = 20, y = 50, label = bl_amyloid_to_bl_cog, size = 2.5) +
  annotate('text', x = 80, y = 50, label = m24_amyloid_to_m24_cog, size = 2.5) +
  annotate('text', x = 50, y = 80, label = bl_amyloid_to_m24_amyloid, size = 2.5) +
  annotate('text', x = 50, y = 20, label = bl_cog_to_m24_cog, size = 2.5) +
  annotate('text', x = 35, y = 70, label = bl_amyloid_to_m24_cog, size = 2.5) +
  annotate('text', x = 35, y = 30, label = bl_cog_to_m24_amyloid, size = 2.5)

}

### ADNI MEM FIGURES

engage_adnimem_figure <- generate_clpm_figure(bl_amyloid_to_bl_cog      = engage_amyloidbaseline_to_adnimembaseline,
                                              m24_amyloid_to_m24_cog    = engage_amyloidm24_to_adnimemm24,
                                              bl_amyloid_to_m24_amyloid = engage_amyloidbaseline_to_amyloidm24,
                                              bl_cog_to_m24_cog         = engage_adnimembaseline_to_adnimemm24,
                                              bl_amyloid_to_m24_cog     = engage_amyloidbaseline_to_adnimem24,
                                              bl_cog_to_m24_amyloid     = engage_adnimembaseline_to_amyloidm24)

donanemab_adnimem_figure <- generate_clpm_figure(bl_amyloid_to_bl_cog      = donanemab_amyloidbaseline_to_adnimembaseline,
                                              m24_amyloid_to_m24_cog    = donanemab_amyloidm24_to_adnimemm24,
                                              bl_amyloid_to_m24_amyloid = donanemab_amyloidbaseline_to_amyloidm24,
                                              bl_cog_to_m24_cog         = donanemab_adnimembaseline_to_adnimemm24,
                                              bl_amyloid_to_m24_cog     = donanemab_amyloidbaseline_to_adnimem24,
                                              bl_cog_to_m24_amyloid     = donanemab_adnimembaseline_to_amyloidm24)

shouldbe_adnimem_figure <- generate_clpm_figure(bl_amyloid_to_bl_cog      = shouldbe_amyloidbaseline_to_adnimembaseline,
                                              m24_amyloid_to_m24_cog    = shouldbe_amyloidm24_to_adnimemm24,
                                              bl_amyloid_to_m24_amyloid = shouldbe_amyloidbaseline_to_amyloidm24,
                                              bl_cog_to_m24_cog         = shouldbe_adnimembaseline_to_adnimemm24,
                                              bl_amyloid_to_m24_cog     = shouldbe_amyloidbaseline_to_adnimem24,
                                              bl_cog_to_m24_amyloid     = shouldbe_adnimembaseline_to_amyloidm24)

### ADNI EF FIGURES

engage_adnief_figure <- generate_clpm_figure(bl_amyloid_to_bl_cog      = engage_amyloidbaseline_to_adniefbaseline,
                                              m24_amyloid_to_m24_cog    = engage_amyloidm24_to_adniefm24,
                                              bl_amyloid_to_m24_amyloid = engage_amyloidbaseline_to_amyloidm24,
                                              bl_cog_to_m24_cog         = engage_adniefbaseline_to_adniefm24,
                                              bl_amyloid_to_m24_cog     = engage_amyloidbaseline_to_adnief24,
                                              bl_cog_to_m24_amyloid     = engage_adniefbaseline_to_amyloidm24)

donanemab_adnief_figure <- generate_clpm_figure(bl_amyloid_to_bl_cog      = donanemab_amyloidbaseline_to_adniefbaseline,
                                                 m24_amyloid_to_m24_cog    = donanemab_amyloidm24_to_adniefm24,
                                                 bl_amyloid_to_m24_amyloid = donanemab_amyloidbaseline_to_amyloidm24,
                                                 bl_cog_to_m24_cog         = donanemab_adniefbaseline_to_adniefm24,
                                                 bl_amyloid_to_m24_cog     = donanemab_amyloidbaseline_to_adnief24,
                                                 bl_cog_to_m24_amyloid     = donanemab_adniefbaseline_to_amyloidm24)

shouldbe_adnief_figure <- generate_clpm_figure(bl_amyloid_to_bl_cog      = shouldbe_amyloidbaseline_to_adniefbaseline,
                                                m24_amyloid_to_m24_cog    = shouldbe_amyloidm24_to_adniefm24,
                                                bl_amyloid_to_m24_amyloid = shouldbe_amyloidbaseline_to_amyloidm24,
                                                bl_cog_to_m24_cog         = shouldbe_adniefbaseline_to_adniefm24,
                                                bl_amyloid_to_m24_cog     = shouldbe_amyloidbaseline_to_adnief24,
                                                bl_cog_to_m24_amyloid     = shouldbe_adniefbaseline_to_amyloidm24)

### ADAS13 FIGURES

engage_adas13_figure <- generate_clpm_figure(bl_amyloid_to_bl_cog      = engage_amyloidbaseline_to_adas13baseline,
                                              m24_amyloid_to_m24_cog    = engage_amyloidm24_to_adas13m24,
                                              bl_amyloid_to_m24_amyloid = engage_amyloidbaseline_to_amyloidm24,
                                              bl_cog_to_m24_cog         = engage_adas13baseline_to_adas13m24,
                                              bl_amyloid_to_m24_cog     = engage_amyloidbaseline_to_adas1324,
                                              bl_cog_to_m24_amyloid     = engage_adas13baseline_to_amyloidm24)

donanemab_adas13_figure <- generate_clpm_figure(bl_amyloid_to_bl_cog      = donanemab_amyloidbaseline_to_adas13baseline,
                                                 m24_amyloid_to_m24_cog    = donanemab_amyloidm24_to_adas13m24,
                                                 bl_amyloid_to_m24_amyloid = donanemab_amyloidbaseline_to_amyloidm24,
                                                 bl_cog_to_m24_cog         = donanemab_adas13baseline_to_adas13m24,
                                                 bl_amyloid_to_m24_cog     = donanemab_amyloidbaseline_to_adas1324,
                                                 bl_cog_to_m24_amyloid     = donanemab_adas13baseline_to_amyloidm24)

shouldbe_adas13_figure <- generate_clpm_figure(bl_amyloid_to_bl_cog      = shouldbe_amyloidbaseline_to_adas13baseline,
                                                m24_amyloid_to_m24_cog    = shouldbe_amyloidm24_to_adas13m24,
                                                bl_amyloid_to_m24_amyloid = shouldbe_amyloidbaseline_to_amyloidm24,
                                                bl_cog_to_m24_cog         = shouldbe_adas13baseline_to_adas13m24,
                                                bl_amyloid_to_m24_cog     = shouldbe_amyloidbaseline_to_adas1324,
                                                bl_cog_to_m24_amyloid     = shouldbe_adas13baseline_to_amyloidm24)


brain_jpg <- image_read("C:/Users/zkunicki/Documents/Research/In Progress/YMCA 2021/ADNI_amyloid_generalizability/Images/brain.jpg")

test_plot_with_image <- shouldbe_adas13_figure +
  annotation_raster(brain_jpg, xmin = 70, xmax=80, ymin=70, ymax=80)

