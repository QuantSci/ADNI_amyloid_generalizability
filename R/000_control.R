

rmarkdown::render(here::here('R', '000_master-report.Rmd'))
fs::file_move(here::here('R', '000_master-report.html'), here::here('Reports', stringr::str_c("Amyloid_generalizability_report_", Sys.Date(),".html")))

rmarkdown::render(here::here('R', '000_master-presentation.Rmd'))
fs::file_move(here::here('R', '000_master-presentation.html'), here::here('Reports', stringr::str_c("Amyloid_generalizability_presentation_", Sys.Date(),".html")))

