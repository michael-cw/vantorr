# Add Vantor Basemap Seamlines to a Leaflet Map

Overlays Vantor / Maxar Vivid Mosaic seamlines onto a `leaflet` or
`leafletProxy` map. Supports either fast OGC WMS raster cutline
rendering (`mode = "wms"`, default) or interactive OGC WFS vector
polygons (`mode = "wfs"`) with clickable popups displaying acquisition
date (`acq_time`), resolution (`gsd`), sensor (`sensor`), and catalog ID
(`catid`).

## Usage

``` r
add_vantor_seamlines(
  map,
  product = "VIVID_STANDARD_30",
  mode = c("wms", "wfs"),
  bbox = NULL,
  api_key = NULL,
  group = "Vantor Seamlines",
  layerId = NULL,
  cql_filter = NULL,
  color = "#00E5FF",
  weight = 2,
  options = NULL,
  verbose = getOption("vantorr.verbose", TRUE)
)

addVantorSeamlines(
  map,
  product = "VIVID_STANDARD_30",
  mode = c("wms", "wfs"),
  bbox = NULL,
  api_key = NULL,
  group = "Vantor Seamlines",
  layerId = NULL,
  cql_filter = NULL,
  color = "#00E5FF",
  weight = 2,
  options = NULL,
  verbose = getOption("vantorr.verbose", TRUE)
)
```

## Arguments

- map:

  A `leaflet` or `leaflet_proxy` map widget object.

- product:

  Character string. Vivid product name. Default is
  `"VIVID_STANDARD_30"`.

- mode:

  Character string. Either `"wms"` (default, transparent raster overlay
  across all zoom levels) or `"wfs"` (fetches vector polygons via
  [`vantor_get_seamlines()`](https://michael-cw.github.io/vantorr/reference/vantor_get_seamlines.md)
  for `bbox` with interactive metadata popups).

- bbox:

  Optional bounding box or spatial object required when `mode = "wfs"`.

- api_key:

  Character string. Optional Vantor/Maxar API key.

- group:

  Character string. Leaflet layer group name. Default is
  `"Vantor Seamlines"`.

- layerId:

  Character string. Optional leaflet layer ID.

- cql_filter:

  Character string. Optional custom CQL filter.

- color:

  Character string. Stroke color for vector polygons when
  `mode = "wfs"`. Default is `"#00E5FF"`.

- weight:

  Numeric. Stroke weight when `mode = "wfs"`. Default is `2`.

- options:

  Optional
  [`leaflet::WMSTileOptions()`](https://rstudio.github.io/leaflet/reference/map-options.html)
  list when `mode = "wms"`.

- verbose:

  Logical. Print status messages. Default is
  `getOption("vantorr.verbose", TRUE)`.

## Value

The modified `leaflet` or `leaflet_proxy` map object.

## Examples

``` r
if (requireNamespace("leaflet", quietly = TRUE)) {
  m <- leaflet::leaflet() |>
    add_vantor_seamlines(
      product = "VIVID_STANDARD_30",
      api_key = "example_api_key",
      verbose = FALSE
    )
}
```
