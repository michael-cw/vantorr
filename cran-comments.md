## Resubmission
This is a resubmission addressing the feedback from Leonore Hochhauser:

* Ensured that all `graphics::par()` calls in `R/vantor_fieldwork.R` (`vantor_field_atlas()` and `.write_rgb_array_png()`) are immediately preceded by saving the current graphical parameters (`oldpar <- graphics::par(no.readonly = TRUE)`) and registering an immediate `on.exit(graphics::par(oldpar), add = TRUE, after = FALSE)` handler to restore them when the function exits.
* Replaced `tryCatch(..., finally = setwd(old_wd))` around `setwd()` calls in `R/vantor_fieldwork.R` (`vantor_offline_package()` and `.write_single_tpk()`) with an internal helper `.zip_from_dir()` that saves the working directory (`oldwd <- getwd()`) and immediately registers `on.exit(setwd(oldwd), add = TRUE)` on the following line before calling `setwd()`.

## Test environments
* local macOS (aarch64, Apple Silicon), R 4.3.1
* win-builder (R-release and R-devel)
* GitHub Actions (ubuntu-latest, macOS-latest, windows-latest)

## R CMD check results

0 errors | 0 warnings | 1 note

* Note: "New submission". This is a new submission to CRAN.
