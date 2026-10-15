# Add a Vantor Vivid Mosaic Basemap to a Leaflet Map

Adds a high-resolution Vantor / Maxar Vivid Mosaic basemap
(`VIVID_STANDARD_30` or `VIVID_ADVANCED_15`) to a `leaflet` or
`leafletProxy` map widget using either pre-cached OGC WMTS tiles
(recommended for fast interactive panning) or dynamic OGC WMS rendering.

## Usage

``` r
add_vantor_basemap(
  map,
  product = "VIVID_STANDARD_30",
  service = c("WMTS", "WMS"),
  api_key = NULL,
  group = "Vantor Vivid",
  layerId = NULL,
  show_seamlines = FALSE,
  cql_filter = NULL,
  shoreline_masking = FALSE,
  max_zoom = 21L,
  attribution = "Imagery © Vantor / Maxar Technologies",
  options = NULL,
  verbose = getOption("vantorr.verbose", TRUE)
)

addVantorBasemap(
  map,
  product = "VIVID_STANDARD_30",
  service = c("WMTS", "WMS"),
  api_key = NULL,
  group = "Vantor Vivid",
  layerId = NULL,
  show_seamlines = FALSE,
  cql_filter = NULL,
  shoreline_masking = FALSE,
  max_zoom = 21L,
  attribution = "Imagery © Vantor / Maxar Technologies",
  options = NULL,
  verbose = getOption("vantorr.verbose", TRUE)
)
```

## Arguments

- map:

  A `leaflet` or `leaflet_proxy` map widget object.

- product:

  Character string. Vivid product name. Default is
  `"VIVID_STANDARD_30"`. Options include `"VIVID_STANDARD_30"` (30 cm)
  and `"VIVID_ADVANCED_15"` (15 cm).

- service:

  Character string. OGC service protocol: `"WMTS"` (default, fastest in
  browser) or `"WMS"`.

- api_key:

  Character string. Optional Vantor/Maxar API key. If `NULL`, resolved
  from session authentication
  ([`vantor_auth()`](https://michael-cw.github.io/vantorr/reference/vantor_auth.md))
  or environment variables (`VANTOR_API_KEY` / `MAXAR_API_KEY`).

- group:

  Character string. Name of the leaflet layer group. Default is
  `"Vantor Vivid"`.

- layerId:

  Character string. Optional leaflet layer ID.

- show_seamlines:

  Logical. If `TRUE`, also overlays the corresponding cutline/seamline
  layer via
  [`add_vantor_seamlines()`](https://michael-cw.github.io/vantorr/reference/add_vantor_seamlines.md).
  Default is `FALSE`.

- cql_filter:

  Character string. Optional custom CQL filter.

- shoreline_masking:

  Logical. Apply shoreline masking (used when `service = "WMS"`).
  Default is `FALSE`.

- max_zoom:

  Integer. Maximum zoom level for the tile layer. Default is `21L`.

- attribution:

  Character string. Map attribution text.

- options:

  A list of tile options passed to
  [`leaflet::tileOptions()`](https://rstudio.github.io/leaflet/reference/map-options.html)
  or
  [`leaflet::WMSTileOptions()`](https://rstudio.github.io/leaflet/reference/map-options.html).

- verbose:

  Logical. Print status messages. Default is
  `getOption("vantorr.verbose", TRUE)`.

## Value

The modified `leaflet` or `leaflet_proxy` map object.

## Examples

``` r
if (requireNamespace("leaflet", quietly = TRUE)) {
  m <- leaflet::leaflet() |>
    add_vantor_basemap(
      product = "VIVID_STANDARD_30",
      api_key = "example_api_key",
      verbose = FALSE
    )
}
```
