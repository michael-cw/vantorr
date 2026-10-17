# Generate a Printable Multi-Page Field Atlas (PDF Map Book)

Creates a multi-page printable PDF Field Atlas (Map Book) for field
supervisors and survey enumerators:

- **Page 1 (Overview Index Map)**: Displays the full Area of Interest,
  all Enumeration Area / survey grid boundaries labeled by `id_col`
  (e.g., `"A1"`, `"A2"`), sample points, scale bar, and north arrow.

- **Subsequent Pages (Detail Enumeration Sheets)**: One page per grid
  cell or Enumeration Area polygon, zoomed in with a metric margin
  (`buffer_m`), optional high-resolution Vantor RGB satellite imagery
  background, labeled sample points, scale bar, north arrow, and a
  metadata header/footer with coordinates and imagery vintage.

## Usage

``` r
vantor_field_atlas(
  grid,
  points = NULL,
  raster = NULL,
  output_file = tempfile(pattern = "vantor_field_atlas_", fileext = ".pdf"),
  id_col = "cell_id",
  point_label_col = "sample_id",
  title = "Vantor Field Enumeration Atlas",
  buffer_m = 50,
  fetch_basemap = FALSE,
  resolution = 0.5,
  product = "VIVID_STANDARD_30",
  paper_width = 11,
  paper_height = 8.5,
  verbose = getOption("vantorr.verbose", TRUE)
)
```

## Arguments

- grid:

  An `sf` polygon object representing survey grid cells (e.g. from
  [`vantor_survey_grid()`](https://michael-cw.github.io/vantorr/reference/vantor_survey_grid.md))
  or Enumeration Area boundaries.

- points:

  Optional `sf` point object of sample locations (e.g. from
  [`vantor_export_capi()`](https://michael-cw.github.io/vantorr/reference/vantor_export_capi.md))
  to plot and label on each map sheet.

- raster:

  Optional
  [`terra::SpatRaster`](https://rspatial.github.io/terra/reference/SpatRaster-class.html)
  or GeoTIFF file path to use as the RGB satellite imagery background.

- output_file:

  Character string. Output PDF file path. Defaults to
  `tempfile(pattern = "vantor_field_atlas_", fileext = ".pdf")` in
  compliance with CRAN policy.

- id_col:

  Character string. Column name in `grid` used to label each enumeration
  sheet. Default is `"cell_id"`.

- point_label_col:

  Character string. Optional column name in `points` used to label
  sample markers on detail sheets. Default is `"sample_id"`.

- title:

  Character string. Title printed on the overview cover map. Default is
  `"Vantor Field Enumeration Atlas"`.

- buffer_m:

  Numeric. Buffer distance in meters around each detail cell. Default is
  `50`.

- fetch_basemap:

  Logical. If `TRUE` and `raster` is `NULL`, downloads Vantor WMS
  satellite imagery for the atlas extent via
  [`get_maxar_wms_basemap()`](https://michael-cw.github.io/vantorr/reference/get_maxar_wms_basemap.md).
  Default is `FALSE`.

- resolution:

  Numeric. Ground resolution in meters when `fetch_basemap = TRUE`.
  Default is `0.5`.

- product:

  Character string. Vivid product name when `fetch_basemap = TRUE`.
  Default is `"VIVID_STANDARD_30"`.

- paper_width:

  Numeric. PDF page width in inches. Default is `11` (landscape).

- paper_height:

  Numeric. PDF page height in inches. Default is `8.5`.

- verbose:

  Logical. Print progress messages. Default is
  `getOption("vantorr.verbose", TRUE)`.

## Value

Invisibly returns the file path to the generated PDF Field Atlas.

## Details

Works both **offline** (using a pre-downloaded `raster` `SpatRaster` or
vector-only mode when `fetch_basemap = FALSE`) and **online** (fetching
Vantor WMS basemap imagery automatically when `fetch_basemap = TRUE`).

## Examples

``` r
aoi <- sf::st_bbox(
  c(xmin = -105.01, ymin = 39.73, xmax = -105.00, ymax = 39.74),
  crs = sf::st_crs(4326)
)
grid_sf <- vantor_survey_grid(aoi, cell_size = 600, verbose = FALSE)
pdf_path <- vantor_field_atlas(grid_sf, verbose = FALSE)
unlink(pdf_path)
```
