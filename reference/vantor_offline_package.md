# Package Offline Fieldwork Vector Layers and Satellite Basemaps

Bundles survey grid cells, sample points, seamline vintage polygons, and
high-resolution Vantor satellite imagery into a self-contained directory
ready to copy onto mobile tablets for offline use in **QField**,
**Mergin Maps**, **ArcGIS Field Maps**, **World Bank Survey Solutions**,
or **ODK Collect**:

- Writes all vector layers (`grid`, `samples`, `seamlines`) into a
  single multi-layer OGC **GeoPackage (`.gpkg`)**.

- Writes a CSV preload table (`<package_name>_samples.csv`) and a
  **World Bank Survey Solutions** tab-delimited assignment preload file
  (`<package_name>_suso_preload.tab`) when `samples` is provided.

- Saves or downloads the satellite basemap as a Web Mercator
  (`EPSG:3857`) georeferenced **GeoTIFF (`.tif`)**.

- Optionally builds a multi-zoom Esri **Tile Package (`.tpk` or
  `.tpkx`)** via
  [`vantor_write_tpk()`](https://michael-cw.github.io/vantorr/reference/vantor_write_tpk.md)
  for zoomable offline basemaps in **Survey Solutions** (ArcGIS Maps SDK
  for .NET v200.x) and **ArcGIS Field Maps**.

- Optionally exports zipped **Esri Shapefiles (`_shapes.zip`)** for
  uploading vector Enumeration Area boundaries to Survey Solutions
  Headquarters.

## Usage

``` r
vantor_offline_package(
  output_dir = tempfile(pattern = "vantor_offline_pkg_"),
  grid = NULL,
  samples = NULL,
  seamlines = NULL,
  raster = NULL,
  fetch_basemap = FALSE,
  create_tpk = FALSE,
  tpk_zoom = 15:16,
  create_shapefiles = FALSE,
  suso_gps_var = "hh_gps",
  resolution = 0.5,
  product = "VIVID_STANDARD_30",
  package_name = "vantor_fieldwork",
  verbose = getOption("vantorr.verbose", TRUE)
)
```

## Arguments

- output_dir:

  Character string. Target directory for the offline bundle. Defaults to
  `tempfile(pattern = "vantor_offline_pkg_")` in compliance with CRAN
  policy.

- grid:

  Optional `sf` polygon layer (e.g., from
  [`vantor_survey_grid()`](https://michael-cw.github.io/vantorr/reference/vantor_survey_grid.md)).

- samples:

  Optional `sf` point layer (e.g., from
  [`vantor_export_capi()`](https://michael-cw.github.io/vantorr/reference/vantor_export_capi.md)).

- seamlines:

  Optional `sf` seamline vintage layer (e.g., from
  [`vantor_get_seamlines()`](https://michael-cw.github.io/vantorr/reference/vantor_get_seamlines.md)).

- raster:

  Optional
  [`terra::SpatRaster`](https://rspatial.github.io/terra/reference/SpatRaster-class.html)
  or GeoTIFF file path to include in the offline bundle. Automatically
  reprojected to Web Mercator (`EPSG:3857`) for Survey Solutions / Esri
  compatibility.

- fetch_basemap:

  Logical. If `TRUE` and `raster` is `NULL`, downloads the Vantor WMS
  basemap for the extent of `grid` or `samples`. Default is `FALSE`.

- create_tpk:

  Logical or character (`"tpk"` or `"tpkx"`). If `TRUE` or `"tpk"`,
  generates an Esri `.tpk` (Compact Cache V1) tile package; if `"tpkx"`,
  generates an open-specification `.tpkx` (Compact Cache V2) tile
  package supported by Survey Solutions v24.09+ (ArcGIS Maps SDK
  v200.x). Default is `FALSE`.

- tpk_zoom:

  Integer vector. Web Mercator zoom levels (`0` to `22`) to include when
  `create_tpk` is enabled. Default is `15:16`.

- create_shapefiles:

  Logical. If `TRUE`, also writes `grid` and `samples` as `EPSG:4326`
  Esri Shapefiles bundled into `<package_name>_shapes.zip` for Survey
  Solutions Headquarters upload. Default is `FALSE`.

- suso_gps_var:

  Character string. Variable name of the identifying GPS question in
  your Survey Solutions Designer questionnaire used to format
  `<suso_gps_var>__Latitude` and `<suso_gps_var>__Longitude` columns in
  `<package_name>_suso_preload.tab`. Default is `"hh_gps"`.

- resolution:

  Numeric. Ground resolution in meters when `fetch_basemap = TRUE`.
  Default is `0.5`.

- product:

  Character string. Vivid product name when `fetch_basemap = TRUE`.
  Default is `"VIVID_STANDARD_30"`.

- package_name:

  Character string. Base filename prefix for bundled files. Default is
  `"vantor_fieldwork"`.

- verbose:

  Logical. Print progress messages. Default is
  `getOption("vantorr.verbose", TRUE)`.

## Value

A list containing the paths to the generated offline bundle files:
`output_dir`, `gpkg_path`, `layers`, `csv_path`, `suso_tab_path`,
`raster_path`, `tpk_path`, `tpk_zip_path`, and `shapefile_zip_path`.

## Examples

``` r
aoi <- sf::st_bbox(
  c(xmin = -105.01, ymin = 39.73, xmax = -105.00, ymax = 39.74),
  crs = sf::st_crs(4326)
)
grid_sf <- vantor_survey_grid(aoi, cell_size = 600, verbose = FALSE)
bundle <- vantor_offline_package(
  grid = grid_sf,
  create_shapefiles = TRUE,
  verbose = FALSE
)
unlink(bundle$output_dir, recursive = TRUE)
```
