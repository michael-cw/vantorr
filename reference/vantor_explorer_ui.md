# Vantor Interactive Imagery Explorer Shiny Module UI

Creates a drop-in, namespace-safe Shiny UI component featuring an
interactive `leaflet` map with Vantor Vivid Mosaic basemap switching
(`VIVID_STANDARD_30` and `VIVID_ADVANCED_15`), seamline cutline overlay,
WFS seamline vintage inspection, live AOI bounding box metrics, and
one-click GeoTIFF export.

## Usage

``` r
vantor_explorer_ui(id, height = "550px")
```

## Arguments

- id:

  Character string. The Shiny module namespace ID.

- height:

  Character string. CSS height of the `leaflet` map widget. Default is
  `"550px"`.

## Value

A `shiny.tag.list` UI definition.

## Examples

``` r
if (requireNamespace("shiny", quietly = TRUE) &&
    requireNamespace("leaflet", quietly = TRUE)) {
  ui <- shiny::fluidPage(
    vantor_explorer_ui("explorer")
  )
}
```
