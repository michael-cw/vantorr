# Prepare and Export Survey Sample Points for CAPI Fieldwork Apps

Enriches a set of survey sample locations (`sf` points or polygons, such
as household structures, enumeration clusters, or ground-truth plots)
with navigation and preload fields ready for **ODK Collect**,
**KoboToolbox**, **ArcGIS Survey123**, **World Bank Survey Solutions**,
and **QField**:

- `sample_id`: Unique formatted sample identifier

- `longitude`, `latitude`: WGS84 decimal degree coordinates

- `odk_geopoint`: Preload string (`"<lat> <lon> 0 0"`) used by ODK /
  Kobo `geopoint` questions

- `plus_code`: Offline-capable 10-character Google Plus Code via
  [`vantor_plus_code()`](https://michael-cw.github.io/vantorr/reference/vantor_plus_code.md)

- `google_maps_url`: One-click navigation link for mobile enumerators

- Optional grid cell assignment (`cell_id`) if a `grid` from
  [`vantor_survey_grid()`](https://michael-cw.github.io/vantorr/reference/vantor_survey_grid.md)
  is supplied

- Optional imagery vintage metadata (`acq_date`, `sensor`, `gsd_m`) if
  `seamlines` are supplied

## Usage

``` r
vantor_export_capi(
  samples,
  id_col = NULL,
  grid = NULL,
  seamlines = NULL,
  output_csv = NULL,
  output_geojson = NULL,
  output_suso_tab = NULL,
  suso_gps_var = NULL,
  verbose = getOption("vantorr.verbose", TRUE)
)
```

## Arguments

- samples:

  An `sf` object (`POINT` or `POLYGON`; polygons are converted to
  representative points via
  [`sf::st_point_on_surface()`](https://r-spatial.github.io/sf/reference/geos_unary.html)).

- id_col:

  Character string. Optional existing column name in `samples` to use as
  `sample_id`. If `NULL`, generates sequential IDs (`"S001"`, `"S002"`,
  ...).

- grid:

  Optional `sf` grid from
  [`vantor_survey_grid()`](https://michael-cw.github.io/vantorr/reference/vantor_survey_grid.md)
  used to assign each sample to its Enumeration Area / grid `cell_id`.

- seamlines:

  Optional `sf` seamline polygons from
  [`vantor_get_seamlines()`](https://michael-cw.github.io/vantorr/reference/vantor_get_seamlines.md)
  used to attach imagery vintage (`imagery_acq_date`, `imagery_sensor`)
  to each sample point.

- output_csv:

  Optional file path to write a CSV preload table (for ODK Collect,
  KoboToolbox, and ArcGIS Survey123). Default is `NULL`.

- output_geojson:

  Optional file path to write a GeoJSON file for mobile GIS map overlays
  (QField, Mergin Maps). Default is `NULL`.

- output_suso_tab:

  Optional file path to write a tab-delimited (`.tab`) assignment
  preload file formatted for **World Bank Survey Solutions**
  Headquarters (`Upload Assignments`). Includes composite GPS columns
  (`<suso_gps_var>__Latitude` and `<suso_gps_var>__Longitude`) so
  sampled structures appear as interactive pins on the Interviewer
  tablet Map Dashboard over the `.tpk` basemap, plus `_target_area`
  (from `cell_id`), `plus_code`, and `imagery_acq_date`. Default is
  `NULL`.

- suso_gps_var:

  Optional character string. Variable name of the identifying GPS
  question in Survey Solutions Designer (e.g., `"hh_gps"`). When
  provided (or when `output_suso_tab` is set), the coordinate columns on
  both the returned `sf` object and the exported `.tab` file are named
  `<suso_gps_var>__Latitude` and `<suso_gps_var>__Longitude` instead of
  `latitude` and `longitude`. Default is `NULL` (uses `"hh_gps"`
  automatically when `output_suso_tab` is supplied).

- verbose:

  Logical. Print progress messages. Default is
  `getOption("vantorr.verbose", TRUE)`.

## Value

An enriched `sf` point object in `EPSG:4326`.

## Details

Optionally writes the enriched dataset to a CSV preload table
(`output_csv`) and/or a GeoJSON file (`output_geojson`).

## Examples

``` r
pts <- sf::st_as_sf(
  data.frame(
    hh_id = c("HH_01", "HH_02"),
    lon = c(-105.005, -105.002),
    lat = c(39.735, 39.738)
  ),
  coords = c("lon", "lat"),
  crs = 4326
)

capi_sf <- vantor_export_capi(pts, id_col = "hh_id", suso_gps_var = "hh_gps", verbose = FALSE)
head(capi_sf)
#> Simple feature collection with 2 features and 7 fields
#> Geometry type: POINT
#> Dimension:     XY
#> Bounding box:  xmin: -105.005 ymin: 39.735 xmax: -105.002 ymax: 39.738
#> Geodetic CRS:  WGS 84
#>   hh_id                geometry sample_id hh_gps__Latitude hh_gps__Longitude
#> 1 HH_01 POINT (-105.005 39.735)     HH_01           39.735          -105.005
#> 2 HH_02 POINT (-105.002 39.738)     HH_02           39.738          -105.002
#>                odk_geopoint   plus_code
#> 1 39.735000 -105.005000 0 0 85FPPXPW+22
#> 2 39.738000 -105.002000 0 0 85FPPXQX+66
#>                                                         google_maps_url
#> 1 https://www.google.com/maps/search/?api=1&query=39.735000,-105.005000
#> 2 https://www.google.com/maps/search/?api=1&query=39.738000,-105.002000
```
