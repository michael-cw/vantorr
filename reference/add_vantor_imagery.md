# Add Vantor Archive Streaming Imagery to a Leaflet Map

Streams individual archive satellite imagery strips or filtered
collections from the Vantor / Maxar Streaming Imagery OGC WMS endpoint
(`https://api.maxar.com/streaming/v1/ogc/wms`) onto a `leaflet` or
`leafletProxy` map.

## Usage

``` r
add_vantor_imagery(
  map,
  feature_id = NULL,
  cql_filter = NULL,
  api_key = NULL,
  group = "Vantor Archive Imagery",
  layerId = NULL,
  format = "image/vnd.jpeg-png",
  transparent = TRUE,
  max_zoom = 21L,
  attribution = "Imagery © Vantor / Maxar Technologies",
  options = NULL,
  verbose = getOption("vantorr.verbose", TRUE)
)

addVantorImagery(
  map,
  feature_id = NULL,
  cql_filter = NULL,
  api_key = NULL,
  group = "Vantor Archive Imagery",
  layerId = NULL,
  format = "image/vnd.jpeg-png",
  transparent = TRUE,
  max_zoom = 21L,
  attribution = "Imagery © Vantor / Maxar Technologies",
  options = NULL,
  verbose = getOption("vantorr.verbose", TRUE)
)
```

## Arguments

- map:

  A `leaflet` or `leaflet_proxy` map widget object.

- feature_id:

  Character string. Optional catalog / feature ID of a specific
  satellite strip (e.g., `"10300100EBEDF600"`). Used to construct
  `cql_filter = "featureId='<feature_id>'"` if `cql_filter` is `NULL`.

- cql_filter:

  Character string. Optional custom CQL filter, such as
  `"(acquisitionDate>='2023-01-01')AND(cloudCover<0.20)"`.

- api_key:

  Character string. Optional Vantor/Maxar API key.

- group:

  Character string. Leaflet layer group name. Default is
  `"Vantor Archive Imagery"`.

- layerId:

  Character string. Optional leaflet layer ID.

- format:

  Character string. WMS tile image format. Default is
  `"image/vnd.jpeg-png"`.

- transparent:

  Logical. Request transparent background for no-data pixels. Default is
  `TRUE`.

- max_zoom:

  Integer. Maximum zoom level. Default is `21L`.

- attribution:

  Character string. Map attribution text.

- options:

  Optional
  [`leaflet::WMSTileOptions()`](https://rstudio.github.io/leaflet/reference/map-options.html)
  list.

- verbose:

  Logical. Print status messages. Default is
  `getOption("vantorr.verbose", TRUE)`.

## Value

The modified `leaflet` or `leaflet_proxy` map object.

## Examples

``` r
if (requireNamespace("leaflet", quietly = TRUE)) {
  m <- leaflet::leaflet() |>
    add_vantor_imagery(
      feature_id = "10300100EBEDF600",
      api_key = "example_api_key",
      verbose = FALSE
    )
}
```
