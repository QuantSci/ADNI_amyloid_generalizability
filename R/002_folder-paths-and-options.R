knitr::opts_chunk$set(echo=FALSE, warning=FALSE, message=FALSE)

r.objects.folder <- here::here("R_objects")

if (!fs::dir_exists(r.objects.folder)) {
  fs::dir_create(r.objects.folder)
}

# This is the path to the Box directory on my laptop with the default location
box_path <- fs::path_home("Box")

box_adni_path <- fs::path(box_path, "ADNI")
box_hrs_path <- fs::path(box_path, "HRS")
