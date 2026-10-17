# Write an Esri Tile Package (`.tpk` or `.tpkx`) for Offline Field Basemaps

Generates a multi-zoom Web Mercator (`EPSG:3857`) Esri Tile Package
(`.tpk` Compact Cache V1 or `.tpkx` Compact Cache V2) in pure R without
requiring proprietary ArcGIS software or ArcGIS Online credits. Designed
for zoomable offline satellite basemaps in **World Bank Survey
Solutions** (including v24.09+ built on the **ArcGIS Maps SDK for .NET
v200.x**), **ArcGIS Field Maps**, and **ArcGIS Survey123**.

Tiles can either be sliced offline from a local
[`terra::SpatRaster`](https://rspatial.github.io/terra/reference/SpatRaster-class.html)
/ GeoTIFF (`raster`), or fetched directly across zoom levels from the
Vantor WMS/WMTS imagery service (`raster = NULL`).

## Usage

``` r
vantor_write_tpk(
  x = NULL,
  output_file = tempfile(fileext = ".tpk"),
  zoom = 15:17,
  format = c("auto", "tpk", "tpkx"),
  raster = NULL,
  product = "VIVID_STANDARD_30",
  max_tiles = 2500L,
  title = "Vantor Satellite Basemap",
  zip_for_suso = FALSE,
  aoi = NULL,
  verbose = getOption("vantorr.verbose", TRUE)
)
```

## Arguments

- x:

  Optional spatial object (`sf`, `sfc`, `bbox`, `SpatRaster`,
  `SpatVector`, `SpatExtent`, `stars`, or numeric
  `c(xmin, ymin, xmax, ymax)`) defining the Area of Interest. If `NULL`,
  the extent of `raster` is used.

- output_file:

  Character string. Destination file path ending in `.tpk` or `.tpkx`.
  Defaults to `tempfile(fileext = ".tpk")` in compliance with CRAN
  policy.

- zoom:

  Integer vector of Web Mercator (`EPSG:3857`) zoom levels (`0` to `22`)
  to render into the tile pyramid. Default is `15:17`.

- format:

  Character string: `"auto"` (inferred from `output_file` extension),
  `"tpk"` (Compact Cache V1 with `.bundle` + `.bundlx` and XML
  metadata), or `"tpkx"` (Apache-2.0 open specification Compact Cache V2
  with `.bundle` and JSON metadata, supported by ArcGIS Maps SDK
  v200.x).

- raster:

  Optional
  [`terra::SpatRaster`](https://rspatial.github.io/terra/reference/SpatRaster-class.html)
  or file path to a GeoTIFF raster. When supplied, tiles are resampled
  offline from `raster`. When `NULL`, tiles are downloaded from the
  Vantor WMS service using active credentials
  ([`vantor_auth()`](https://michael-cw.github.io/vantorr/reference/vantor_auth.md)).

- product:

  Character string. Vantor Vivid mosaic product identifier when fetching
  online tiles (`raster = NULL`). Default is `"VIVID_STANDARD_30"`.

- max_tiles:

  Integer. Safety cap on total tiles across all requested zoom levels to
  prevent accidental massive downloads. Default is `2500L`.

- title:

  Character string. Package title embedded in `iteminfo`. Default is
  `"Vantor Satellite Basemap"`.

- zip_for_suso:

  Logical. If `TRUE`, also creates a clean outer `.zip` archive
  (`<stem>_tpk.zip` or `<stem>_tpkx.zip`) using `zip -q -X -j`
  (stripping macOS `__MACOSX/._*` resource forks that Survey Solutions
  Headquarters rejects when users compress files via macOS Finder).
  Default is `FALSE`.

- aoi:

  Optional alias for `x` (Area of Interest).

- verbose:

  Logical. Print progress messages. Default is
  `getOption("vantorr.verbose", TRUE)`.

## Value

Invisibly returns the normalized `output_file` path, with attributes
`n_tiles`, `zoom`, `format`, and `suso_zip_path` (when
`zip_for_suso = TRUE`).

## Examples

``` r
if (requireNamespace("terra", quietly = TRUE)) {
  # Create a synthetic RGB SpatRaster in EPSG:3857
  r <- terra::rast(
    xmin = -11689500, xmax = -11688500,
    ymin = 4826500, ymax = 4827500,
    nrows = 128, ncols = 128, nlyrs = 3,
    crs = "EPSG:3857"
  )
  terra::values(r) <- sample(50:220, terra::ncell(r) * 3, replace = TRUE)

  # Export both .tpk (Compact Cache V1) and .tpkx (Compact Cache V2)
  tpk_file <- vantor_write_tpk(
    raster = r,
    output_file = tempfile(fileext = ".tpk"),
    zoom = 14:15,
    verbose = FALSE
  )
  tpkx_file <- vantor_write_tpk(
    raster = r,
    output_file = tempfile(fileext = ".tpkx"),
    zoom = 14:15,
    verbose = FALSE
  )
  unlink(c(tpk_file, tpkx_file))
}
```
