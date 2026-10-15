# Get Maxar / Vantor WMS Basemap as GeoTIFF or Spatial Raster Object

Retrieves a Maxar/Vantor Vivid Mosaic basemap (30 cm or 15 cm) via OGC
WMS for a specified area of interest. Supports spatial inputs from `sf`,
`terra`, and `stars`, as well as numeric bounding boxes, and can return
a file path,
[`terra::SpatRaster`](https://rspatial.github.io/terra/reference/SpatRaster-class.html),
or `stars` object.

## Usage

``` r
get_maxar_wms_basemap(
  bbox,
  token = NULL,
  output_file = tempfile(pattern = "vantor_basemap_", fileext = ".tif"),
  width = NULL,
  height = NULL,
  resolution = 0.3,
  product = "VIVID_STANDARD_30",
  crs = c("EPSG:3857", "EPSG:4326"),
  cql_filter = NULL,
  shoreline_masking = FALSE,
  api_key = NULL,
  return_type = c("file", "SpatRaster", "stars", "stars_proxy"),
  verbose = getOption("vantorr.verbose", TRUE)
)
```

## Arguments

- bbox:

  A spatial object (`sf`, `sfc`, `bbox`, `SpatRaster`, `SpatVector`,
  `SpatExtent`, `stars`, `stars_proxy`) or a numeric vector of length 4:
  `c(xmin, ymin, xmax, ymax)`. Automatically transformed to the target
  `crs`.

- token:

  Character string. Optional OAuth2 bearer token from
  [`get_maxar_token()`](https://michael-cw.github.io/vantorr/reference/get_maxar_token.md).
  If `NULL`, resolved via `api_key`, session cache
  ([`vantor_auth()`](https://michael-cw.github.io/vantorr/reference/vantor_auth.md)),
  or environment variables.

- output_file:

  Character string. Path where the GeoTIFF file will be saved. Defaults
  to `tempfile(pattern = "vantor_basemap_", fileext = ".tif")` in
  compliance with CRAN policies.

- width:

  Integer. Width of the output image in pixels. If `NULL`, calculated
  from `resolution`.

- height:

  Integer. Height of the output image in pixels. If `NULL`, calculated
  from `resolution`.

- resolution:

  Numeric. Target ground resolution in meters. Default is `0.3` (30 cm).
  Used to calculate `width` and `height` if not explicitly provided.

- product:

  Character string. Vivid product name. Default is
  `"VIVID_STANDARD_30"`. Options include `"VIVID_STANDARD_30"` and
  `"VIVID_ADVANCED_15"`.

- crs:

  Character string. Output CRS requested from WMS (`"EPSG:3857"` or
  `"EPSG:4326"`). Default is `"EPSG:3857"`.

- cql_filter:

  Character string. Optional custom CQL filter. If `NULL`, constructed
  automatically from `product` and `shoreline_masking`.

- shoreline_masking:

  Logical. Apply shoreline masking. Default is `FALSE`.

- api_key:

  Character string. Optional Maxar/Vantor API key (`maxar-api-key`).

- return_type:

  Character string. Return format: `"file"` (default, returns the file
  path invisibly), `"SpatRaster"` (returns a
  [`terra::SpatRaster`](https://rspatial.github.io/terra/reference/SpatRaster-class.html)),
  `"stars"` (returns an in-memory `stars` object), or `"stars_proxy"`.

- verbose:

  Logical. Print progress messages. Default is
  `getOption("vantorr.verbose", TRUE)`.

## Value

Depending on `return_type`, the path to the saved GeoTIFF file
(invisibly when `return_type = "file"`), a `SpatRaster`, or a `stars`
object. Returns `NULL` invisibly (with an informative message) if the
API request fails.

## Examples

``` r
# \donttest{
if (vantor_has_auth()) {
  # Define area of interest using an sf bounding box (WGS84)
  aoi <- sf::st_bbox(
    c(xmin = -105.005, ymin = 39.735, xmax = -105.000, ymax = 39.740),
    crs = sf::st_crs(4326)
  )

  # Retrieve basemap to a temporary GeoTIFF file
  tif_file <- get_maxar_wms_basemap(
    bbox = aoi,
    resolution = 0.5,
    verbose = FALSE
  )
}
# }
```
