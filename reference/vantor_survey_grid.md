# Generate an Alphanumeric Survey Index Grid over an Area of Interest

Partitions an Area of Interest (AOI) into a regular grid of square or
hexagonal enumeration cells with human-readable alphanumeric index
labels (`"A1"`, `"A2"`, `"B1"`, etc.), cell centroids (`centroid_lon`,
`centroid_lat`), and area in square meters (`area_m2`). Designed for
planning field enumeration areas, printing map book index sheets with
[`vantor_field_atlas()`](https://michael-cw.github.io/vantorr/reference/vantor_field_atlas.md),
and stratified spatial sampling.

## Usage

``` r
vantor_survey_grid(
  aoi,
  cell_size = 500,
  shape = c("square", "hexagonal"),
  clip_to_aoi = TRUE,
  crs = 4326,
  verbose = getOption("vantorr.verbose", TRUE)
)
```

## Arguments

- aoi:

  A spatial object (`sf`, `sfc`, `bbox`, `SpatRaster`, `SpatVector`,
  `SpatExtent`, `stars`, `stars_proxy`) or a numeric bounding box vector
  of length 4 (`c(xmin, ymin, xmax, ymax)` in `EPSG:4326`).

- cell_size:

  Numeric. Target width/diameter of each grid cell in meters (computed
  in Web Mercator `EPSG:3857`). Default is `500` (500 m).

- shape:

  Character string. Grid cell geometry: `"square"` (default) or
  `"hexagonal"`.

- clip_to_aoi:

  Logical. If `TRUE` (default) and `aoi` is an `sf`, `sfc`, or
  `SpatVector` polygon, retains only grid cells that spatially intersect
  the `aoi` geometry.

- crs:

  Numeric or character. Output Coordinate Reference System for the
  returned `sf` grid. Default is `4326` (`EPSG:4326`).

- verbose:

  Logical. Print status messages. Default is
  `getOption("vantorr.verbose", TRUE)`.

## Value

An `sf` polygon object with columns:

- `cell_id`: Alphanumeric grid label (`"A1"`, `"A2"`, ..., `"B1"`, ...)

- `row_idx`: Row index (top-to-bottom, 1-based)

- `col_idx`: Column index (left-to-right, 1-based)

- `centroid_lon`, `centroid_lat`: Cell centroid coordinates in
  `EPSG:4326`

- `area_m2`: Approximate planar area in square meters

- `geometry`: Polygon geometry in `crs`

## Examples

``` r
aoi <- sf::st_bbox(
  c(xmin = -105.01, ymin = 39.73, xmax = -104.99, ymax = 39.75),
  crs = sf::st_crs(4326)
)
grid_sf <- vantor_survey_grid(aoi, cell_size = 500, verbose = FALSE)
head(grid_sf)
#> Simple feature collection with 6 features and 6 fields
#> Geometry type: POLYGON
#> Dimension:     XY
#> Bounding box:  xmin: -105.01 ymin: 39.74382 xmax: -104.9875 ymax: 39.75072
#> Geodetic CRS:  WGS 84
#>   cell_id row_idx col_idx centroid_lon centroid_lat area_m2
#> 1      A1       1       1    -105.0078     39.74900  250000
#> 2      A2       1       2    -105.0033     39.74900  250000
#> 3      A3       1       3    -104.9988     39.74900  250000
#> 4      A4       1       4    -104.9943     39.74900  250000
#> 5      A5       1       5    -104.9898     39.74900  250000
#> 6      B1       2       1    -105.0078     39.74554  250000
#>                         geometry
#> 1 POLYGON ((-105.01 39.74727,...
#> 2 POLYGON ((-105.0055 39.7472...
#> 3 POLYGON ((-105.001 39.74727...
#> 4 POLYGON ((-104.9965 39.7472...
#> 5 POLYGON ((-104.992 39.74727...
#> 6 POLYGON ((-105.01 39.74382,...
```
