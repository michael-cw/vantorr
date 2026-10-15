# Vantor Interactive Imagery Explorer Shiny Module Server

Server logic for the
[`vantor_explorer_ui()`](https://michael-cw.github.io/vantorr/reference/vantor_explorer_ui.md)
Shiny module. Renders an interactive `leaflet` map with Vantor Vivid
basemaps, updates layers via
[`leaflet::leafletProxy()`](https://rstudio.github.io/leaflet/reference/leafletProxy.html),
queries WFS seamlines for the current viewport, and provides a
[`shiny::downloadHandler()`](https://rdrr.io/pkg/shiny/man/downloadHandler.html)
with progress bar integration for exporting GeoTIFFs via
[`get_maxar_wms_basemap()`](https://michael-cw.github.io/vantorr/reference/get_maxar_wms_basemap.md)
or
[`get_maxar_wms_tiles()`](https://michael-cw.github.io/vantorr/reference/get_maxar_wms_tiles.md).

## Usage

``` r
vantor_explorer_server(
  id,
  initial_bbox = c(xmin = -105.01, ymin = 39.73, xmax = -104.98, ymax = 39.75),
  auth_reactive = NULL,
  verbose = getOption("vantorr.verbose", TRUE)
)
```

## Arguments

- id:

  Character string. The Shiny module namespace ID.

- initial_bbox:

  Numeric vector of length 4 (`c(xmin, ymin, xmax, ymax)` in
  `EPSG:4326`) or any spatial object supported by
  [`vantor_as_bbox()`](https://michael-cw.github.io/vantorr/reference/vantor_as_bbox.md)
  defining the initial map view. Defaults to downtown Denver, CO.

- auth_reactive:

  Optional
  [`shiny::reactive`](https://rdrr.io/pkg/shiny/man/reactive.html)
  returned by
  [`vantor_auth_server()`](https://michael-cw.github.io/vantorr/reference/vantor_auth_server.md)
  to automatically refresh map layers when credentials change.

- verbose:

  Logical. Print status messages to the R console. Default is
  `getOption("vantorr.verbose", TRUE)`.

## Value

A list of
[`shiny::reactive`](https://rdrr.io/pkg/shiny/man/reactive.html)
expressions:

- `bbox`: Reactive `sf::bbox` (`EPSG:4326`) of the current map viewport

- `seamlines`: Reactive `sf` object of WFS seamlines when inspected

- `product`: Reactive selected Vivid product name

- `resolution`: Reactive selected ground resolution in meters

## Examples

``` r
if (requireNamespace("shiny", quietly = TRUE) &&
    requireNamespace("leaflet", quietly = TRUE)) {
  server <- function(input, output, session) {
    explorer <- vantor_explorer_server("explorer", verbose = FALSE)
  }
}
```
