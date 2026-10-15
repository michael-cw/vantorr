# Get Multiple WMS Basemap Tiles for Large Areas

Splits a large area of interest into a regular grid of tiles, downloads
each tile via
[`get_maxar_wms_basemap()`](https://michael-cw.github.io/vantorr/reference/get_maxar_wms_basemap.md),
and optionally merges them into a single GeoTIFF,
[`terra::SpatRaster`](https://rspatial.github.io/terra/reference/SpatRaster-class.html),
or `stars` object.

## Usage

``` r
get_maxar_wms_tiles(
  bbox,
  token = NULL,
  api_key = NULL,
  tile_size = 2500,
  output_dir = tempfile(pattern = "vantor_tiles_"),
  merge_tiles = TRUE,
  final_output = tempfile(pattern = "vantor_merged_", fileext = ".tif"),
  return_type = c("file", "SpatRaster", "stars", "stars_proxy"),
  progress_callback = NULL,
  verbose = getOption("vantorr.verbose", TRUE),
  ...
)
```

## Arguments

- bbox:

  A spatial object (`sf`, `sfc`, `bbox`, `SpatRaster`, `SpatVector`,
  `SpatExtent`, `stars`, `stars_proxy`) or a numeric vector of length 4.

- token:

  Character string. Optional OAuth2 bearer token.

- api_key:

  Character string. Optional Maxar/Vantor API key.

- tile_size:

  Numeric. Size of each tile side in meters. Default is `2500` (2.5 km).

- output_dir:

  Character string. Directory for individual tile files. Defaults to a
  new directory inside
  [`tempdir()`](https://rdrr.io/r/base/tempfile.html) in compliance with
  CRAN policy.

- merge_tiles:

  Logical. Merge downloaded tiles into a single mosaic. Default is
  `TRUE` (requires the `terra` package).

- final_output:

  Character string. File path for the merged GeoTIFF. Defaults to
  `tempfile(pattern = "vantor_merged_", fileext = ".tif")`.

- return_type:

  Character string. One of `"file"`, `"SpatRaster"`, `"stars"`, or
  `"stars_proxy"`.

- progress_callback:

  Optional function taking `(i, n_tiles)` called after each tile
  download (useful for integrating with
  [`shiny::incProgress()`](https://rdrr.io/pkg/shiny/man/withProgress.html)).

- verbose:

  Logical. Print progress messages. Default is
  `getOption("vantorr.verbose", TRUE)`.

- ...:

  Additional arguments passed to
  [`get_maxar_wms_basemap()`](https://michael-cw.github.io/vantorr/reference/get_maxar_wms_basemap.md).

## Value

If `merge_tiles = TRUE`, returns the merged output formatted according
to `return_type`. If `merge_tiles = FALSE`, returns a character vector
of downloaded tile file paths.

## Examples

``` r
# \donttest{
if (vantor_has_auth()) {
  large_aoi <- sf::st_bbox(
    c(xmin = -105.02, ymin = 39.72, xmax = -104.98, ymax = 39.75),
    crs = sf::st_crs(4326)
  )
  merged_path <- get_maxar_wms_tiles(
    bbox = large_aoi,
    tile_size = 2000,
    resolution = 1.0,
    verbose = FALSE
  )
}
# }
```
