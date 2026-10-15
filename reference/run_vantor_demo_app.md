# Launch Interactive Vantor Explorer Demo Shiny App

Launches a standalone Shiny application combining
[`vantor_auth_ui()`](https://michael-cw.github.io/vantorr/reference/vantor_auth_ui.md)
and
[`vantor_explorer_ui()`](https://michael-cw.github.io/vantorr/reference/vantor_explorer_ui.md)
for interactively browsing Vantor Vivid basemaps, inspecting seamline
acquisition dates, monitoring account usage, and exporting GeoTIFFs.

## Usage

``` r
run_vantor_demo_app(
  initial_bbox = c(xmin = -105.01, ymin = 39.73, xmax = -104.98, ymax = 39.75),
  verbose = getOption("vantorr.verbose", TRUE)
)
```

## Arguments

- initial_bbox:

  Numeric vector of length 4 (`c(xmin, ymin, xmax, ymax)` in
  `EPSG:4326`) or any spatial object supported by
  [`vantor_as_bbox()`](https://michael-cw.github.io/vantorr/reference/vantor_as_bbox.md).
  Defaults to downtown Denver, CO.

- verbose:

  Logical. Print status messages. Default is
  `getOption("vantorr.verbose", TRUE)`.

## Value

A `shiny.appobj` object.

## Examples

``` r
if (interactive()) {
  run_vantor_demo_app()
}
```
