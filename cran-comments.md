## Resubmission

This is a resubmission. In this version I have:

* Ensured that all `graphics::par()` and `setwd()` calls are immediately preceded by saving the current settings (`oldpar <- graphics::par(no.readonly = TRUE)` and `oldwd <- getwd()`) and registering an immediate `on.exit()` handler to restore them upon function exit.

## R CMD check results

0 errors | 0 warnings | 1 note

* Note: "New submission". This is a new submission to CRAN.

## Test environments
* local macOS (aarch64, Apple Silicon), R 4.3.1
* win-builder (R-release and R-devel)
* GitHub Actions (ubuntu-latest, macOS-latest, windows-latest)
