# Producing Fieldwork Resources for Survey Data Collection

## Overview

Household surveys, agricultural censuses, and ground-truth campaigns
rely on high-resolution satellite imagery to delineate Enumeration Areas
(EAs), verify building footprints, and guide field enumerators to
sampled structures.

`vantorr` provides a dedicated suite of fieldwork functions that turn
Vantor / Maxar satellite basemaps and seamline metadata into
ready-to-use survey resources:

1.  **[`vantor_survey_grid()`](https://michael-cw.github.io/vantorr/reference/vantor_survey_grid.md)**:
    Partition an Area of Interest (AOI) into square or hexagonal survey
    index cells labeled `"A1"`, `"A2"`, `"B1"`, etc.
2.  **[`vantor_vintage_report()`](https://michael-cw.github.io/vantorr/reference/vantor_vintage_report.md)**:
    Audit the acquisition dates (`acq_time`), resolution (`gsd`), and
    sensor quality of Vantor basemap strips across each Enumeration Area
    to flag outdated imagery before deploying field teams.
3.  **[`vantor_plus_code()`](https://michael-cw.github.io/vantorr/reference/vantor_plus_code.md)
    &
    [`vantor_export_capi()`](https://michael-cw.github.io/vantorr/reference/vantor_export_capi.md)**:
    Enrich sample points with offline-capable **Open Location Codes
    (Google Plus Codes)**, grid cell assignments (`cell_id`), and
    imagery acquisition dates (`imagery_acq_date`), and export
    platform-specific preload files:
    - **CSV (`.csv`) & GeoJSON (`.geojson`)** with `odk_geopoint`
      (`"lat lon 0 0"`) for **ODK Collect**, **KoboToolbox**, and
      **ArcGIS Survey123**.
    - **Tab-delimited (`.tab`)** assignment preload tables
      (`output_suso_tab`) with `<gps_var>__Latitude`,
      `<gps_var>__Longitude`, and `_target_area` for **World Bank Survey
      Solutions** (or direct programmatic assignment creation via the
      [**`SurveySolutionsAPIv2`**](https://github.com/michael-cw/SurveySolutionsAPIv2)
      R package).
4.  **[`vantor_field_atlas()`](https://michael-cw.github.io/vantorr/reference/vantor_field_atlas.md)**:
    Generate a multi-page printable **PDF Field Atlas (Map Book)**
    featuring an Overview Index sheet and zoomed-in Enumeration Area
    detail sheets with Vantor satellite imagery, scale bars, north
    arrows, and enumerator sign-off footers.
5.  **[`vantor_offline_package()`](https://michael-cw.github.io/vantorr/reference/vantor_offline_package.md)
    &
    [`vantor_write_tpk()`](https://michael-cw.github.io/vantorr/reference/vantor_write_tpk.md)**:
    Bundle vector layers (`.gpkg`, zipped `.shp`), CAPI preload tables
    (`.csv` and Survey Solutions `.tab`), georeferenced Web Mercator
    basemaps (`.tif`), and multi-zoom Esri Tile Packages (`.tpk` /
    `.tpkx` and clean `_tpk.zip` upload archives) into a portable
    directory for offline tablet apps (**World Bank Survey Solutions**,
    **QField**, **Mergin Maps**, **ArcGIS Field Maps**). See
    [`vignette("survey-solutions-workflow", package = "vantorr")`](https://michael-cw.github.io/vantorr/articles/survey-solutions-workflow.md)
    for the dedicated Survey Solutions & `SurveySolutionsAPIv2` guide.

------------------------------------------------------------------------

## 1. Generating an Alphanumeric Survey Index Grid

Start by defining your survey Area of Interest (using any `sf`, `terra`,
or `stars` object, or a WGS84 bounding box) and partitioning it into a
metric enumeration grid with
[`vantor_survey_grid()`](https://michael-cw.github.io/vantorr/reference/vantor_survey_grid.md):

``` r

library(vantorr)
library(sf)
#> Linking to GEOS 3.12.1, GDAL 3.8.4, PROJ 9.4.0; sf_use_s2() is TRUE

# Define a 1.5 km x 1.5 km survey Area of Interest in WGS84 (EPSG:4326)
aoi <- st_bbox(
  c(xmin = -105.010, ymin = 39.730, xmax = -104.995, ymax = 39.742),
  crs = st_crs(4326)
)

# Generate a 500 m square survey grid with alphanumeric labels (A1, A2, B1, ...)
survey_grid <- vantor_survey_grid(
  aoi = aoi,
  cell_size = 500,
  shape = "square",
  verbose = FALSE
)

survey_grid[, c("cell_id", "row_idx", "col_idx", "centroid_lon", "centroid_lat", "area_m2")]
#> Simple feature collection with 16 features and 6 fields
#> Geometry type: POLYGON
#> Dimension:     XY
#> Bounding box:  xmin: -105.01 ymin: 39.73 xmax: -104.992 ymax: 39.74382
#> Geodetic CRS:  WGS 84
#> First 10 features:
#>    cell_id row_idx col_idx centroid_lon centroid_lat area_m2
#> 1       A1       1       1    -105.0078     39.74209  250000
#> 2       A2       1       2    -105.0033     39.74209  250000
#> 3       A3       1       3    -104.9988     39.74209  250000
#> 4       A4       1       4    -104.9943     39.74209  250000
#> 5       B1       2       1    -105.0078     39.73864  250000
#> 6       B2       2       2    -105.0033     39.73864  250000
#> 7       B3       2       3    -104.9988     39.73864  250000
#> 8       B4       2       4    -104.9943     39.73864  250000
#> 9       C1       3       1    -105.0078     39.73518  250000
#> 10      C2       3       2    -105.0033     39.73518  250000
#>                          geometry
#> 1  POLYGON ((-105.01 39.74036,...
#> 2  POLYGON ((-105.0055 39.7403...
#> 3  POLYGON ((-105.001 39.74036...
#> 4  POLYGON ((-104.9965 39.7403...
#> 5  POLYGON ((-105.01 39.73691,...
#> 6  POLYGON ((-105.0055 39.7369...
#> 7  POLYGON ((-105.001 39.73691...
#> 8  POLYGON ((-104.9965 39.7369...
#> 9  POLYGON ((-105.01 39.73345,...
#> 10 POLYGON ((-105.0055 39.7334...
```

You can also generate hexagonal tessellations for spatial cluster
sampling by setting `shape = "hexagonal"`:

``` r

hex_grid <- vantor_survey_grid(
  aoi = aoi,
  cell_size = 500,
  shape = "hexagonal",
  verbose = FALSE
)
```

------------------------------------------------------------------------

## 2. Auditing Imagery Vintage Across Enumeration Areas

When using satellite imagery to list households or sample structures,
survey managers need to know whether the mosaic over each Enumeration
Area is recent or several years old.

[`vantor_vintage_report()`](https://michael-cw.github.io/vantorr/reference/vantor_vintage_report.md)
intersects your `survey_grid` (or Enumeration Area polygons) with
Vantor’s OGC WFS Seamlines metadata
([`vantor_get_seamlines()`](https://michael-cw.github.io/vantorr/reference/vantor_get_seamlines.md))
and calculates per-cell vintage metrics, flagging any cells where the
imagery is older than `max_age_days` (default `730` days / 2 years):

``` r

# In an online session with Vantor credentials, seamlines are fetched automatically:
# vintage_grid <- vantor_vintage_report(survey_grid, product = "VIVID_STANDARD_30")

# Offline demonstration using synthetic seamline polygons:
bb_mid <- -105.0025
strip_west <- st_bbox(c(xmin = -105.012, ymin = 39.728, xmax = bb_mid, ymax = 39.745), crs = st_crs(4326))
strip_east <- st_bbox(c(xmin = bb_mid, ymin = 39.728, xmax = -104.990, ymax = 39.745), crs = st_crs(4326))

mock_seamlines <- st_sf(
  acq_time = c("2025-08-14T17:45:00Z", "2021-03-20T18:10:00Z"),
  gsd = c(0.30, 0.31),
  ona = c(10.2, 18.5),
  sensor = c("WV03", "WV02"),
  catid = c("10400100AA11", "10300100BB22"),
  geometry = c(st_as_sfc(strip_west), st_as_sfc(strip_east))
)

vintage_grid <- vantor_vintage_report(
  aoi = survey_grid,
  seamlines = mock_seamlines,
  max_age_days = 730,
  reference_date = as.Date("2026-10-15"),
  verbose = FALSE
)

st_drop_geometry(vintage_grid)[, c(
  "cell_id", "n_strips", "earliest_acq_date", "latest_acq_date",
  "max_age_days", "sensors", "vintage_warning"
)]
#>    cell_id n_strips earliest_acq_date latest_acq_date max_age_days    sensors
#> 1       A1        1        2025-08-14      2025-08-14          427       WV03
#> 2       A2        2        2021-03-20      2025-08-14         2035 WV03, WV02
#> 3       A3        1        2021-03-20      2021-03-20         2035       WV02
#> 4       A4        1        2021-03-20      2021-03-20         2035       WV02
#> 5       B1        1        2025-08-14      2025-08-14          427       WV03
#> 6       B2        2        2021-03-20      2025-08-14         2035 WV03, WV02
#> 7       B3        1        2021-03-20      2021-03-20         2035       WV02
#> 8       B4        1        2021-03-20      2021-03-20         2035       WV02
#> 9       C1        1        2025-08-14      2025-08-14          427       WV03
#> 10      C2        2        2021-03-20      2025-08-14         2035 WV03, WV02
#> 11      C3        1        2021-03-20      2021-03-20         2035       WV02
#> 12      C4        1        2021-03-20      2021-03-20         2035       WV02
#> 13      D1        1        2025-08-14      2025-08-14          427       WV03
#> 14      D2        2        2021-03-20      2025-08-14         2035 WV03, WV02
#> 15      D3        1        2021-03-20      2021-03-20         2035       WV02
#> 16      D4        1        2021-03-20      2021-03-20         2035       WV02
#>    vintage_warning
#> 1            FALSE
#> 2             TRUE
#> 3             TRUE
#> 4             TRUE
#> 5            FALSE
#> 6             TRUE
#> 7             TRUE
#> 8             TRUE
#> 9            FALSE
#> 10            TRUE
#> 11            TRUE
#> 12            TRUE
#> 13           FALSE
#> 14            TRUE
#> 15            TRUE
#> 16            TRUE
```

------------------------------------------------------------------------

## 3. Preparing CAPI Sample Preloads & Offline Plus Codes

Once household or cluster sample points are selected within your
Enumeration Areas,
[`vantor_export_capi()`](https://michael-cw.github.io/vantorr/reference/vantor_export_capi.md)
enriches them with coordinates, **ODK / Kobo `geopoint` strings**,
**offline Google Plus Codes**
([`vantor_plus_code()`](https://michael-cw.github.io/vantorr/reference/vantor_plus_code.md)),
one-click Google Maps navigation links, grid cell assignments
(`cell_id`), and imagery acquisition dates (`imagery_acq_date`).

Because different CAPI platforms expect different preload schemas,
[`vantor_export_capi()`](https://michael-cw.github.io/vantorr/reference/vantor_export_capi.md)
supports three simultaneous export targets:

- **`output_csv` & `output_geojson`**: Standard CSV and GeoJSON files
  containing `odk_geopoint` (`"lat lon 0 0"`) for `pulldata()` in **ODK
  Collect**, **KoboToolbox**, and **ArcGIS Survey123**.
- **`output_suso_tab` (`suso_gps_var = "hh_gps"`)**: A tab-delimited
  (`.tab`) assignment preload file formatted specifically for **World
  Bank Survey Solutions Headquarters**
  (`Survey Setup -> Questionnaires -> Upload Assignments`). It
  automatically splits coordinates into `<suso_gps_var>__Latitude` and
  `<suso_gps_var>__Longitude` so preloaded households appear as
  interactive pins on the Interviewer tablet’s **Map Dashboard**, sets
  `_target_area` from `cell_id`, and includes `plus_code` and
  `imagery_acq_date` for cover-page display.

``` r

# Create sample household locations across the AOI
sample_pts <- st_as_sf(
  data.frame(
    hh_id = c("HH-101", "HH-102", "HH-103", "HH-104"),
    head_name = c("Household A", "Household B", "Household C", "Household D"),
    lon = c(-105.008, -105.004, -105.001, -104.997),
    lat = c(39.739, 39.738, 39.734, 39.733)
  ),
  coords = c("lon", "lat"),
  crs = 4326
)

# Enrich samples and export to ODK CSV, GeoJSON, and Survey Solutions .tab
csv_file <- tempfile(pattern = "odk_preload_", fileext = ".csv")
geojson_file <- tempfile(pattern = "field_samples_", fileext = ".geojson")
suso_tab_file <- tempfile(pattern = "suso_preload_", fileext = ".tab")

capi_samples <- vantor_export_capi(
  samples = sample_pts,
  id_col = "hh_id",
  grid = vintage_grid,
  seamlines = mock_seamlines,
  output_csv = csv_file,
  output_geojson = geojson_file,
  output_suso_tab = suso_tab_file,
  suso_gps_var = "hh_gps",
  verbose = FALSE
)

st_drop_geometry(capi_samples)[, c(
  "sample_id", "cell_id", "plus_code",
  "hh_gps__Latitude", "hh_gps__Longitude",
  "imagery_acq_date", "imagery_sensor"
)]
#>   sample_id cell_id   plus_code hh_gps__Latitude hh_gps__Longitude
#> 1    HH-101      B1 85FPPXQR+JR           39.739          -105.008
#> 2    HH-102      B2 85FPPXQW+6C           39.738          -105.004
#> 3    HH-103      C3 85FPPXMX+JJ           39.734          -105.001
#> 4    HH-104      D3 85FQP2M3+66           39.733          -104.997
#>   imagery_acq_date imagery_sensor
#> 1       2025-08-14           WV03
#> 2       2025-08-14           WV03
#> 3       2021-03-20           WV02
#> 4       2021-03-20           WV02
```

Inspecting the generated Survey Solutions `.tab` file confirms that it
contains the exact `<gps_var>__Latitude`, `<gps_var>__Longitude`, and
`_target_area` columns required by Survey Solutions Headquarters:

``` r

read.delim(suso_tab_file, check.names = FALSE)[, c(
  "sample_id", "cell_id", "plus_code",
  "hh_gps__Latitude", "hh_gps__Longitude",
  "_target_area"
)]
#>   sample_id cell_id   plus_code hh_gps__Latitude hh_gps__Longitude _target_area
#> 1    HH-101      B1 85FPPXQR+JR           39.739          -105.008           B1
#> 2    HH-102      B2 85FPPXQW+6C           39.738          -105.004           B2
#> 3    HH-103      C3 85FPPXMX+JJ           39.734          -105.001           C3
#> 4    HH-104      D3 85FQP2M3+66           39.733          -104.997           D3
```

Enumerators can type the preloaded `plus_code` (e.g., `85FPPXQR+JR`)
from the interview cover page directly into Google Maps or OSM on a
mobile tablet to get turn-by-turn routing to the sampled structure
**completely offline**, then switch to the offline Vantor `.tpk` basemap
for the final 100 meters.

------------------------------------------------------------------------

## 4. Generating a Multi-Page Printable Field Atlas (PDF Map Book)

For paper back-up maps and field supervisor binders,
[`vantor_field_atlas()`](https://michael-cw.github.io/vantorr/reference/vantor_field_atlas.md)
generates a multi-page PDF Map Book containing:

- **Page 1 (Overview Index Sheet)**: Shows the entire AOI, all grid cell
  boundaries and labels (`A1`, `A2`, …), and all sample locations.
- **Pages 2..N (Detail Enumeration Sheets)**: One page per grid cell,
  zoomed in with a configurable buffer (`buffer_m = 50`), metric scale
  bar, north arrow, sample point labels, imagery vintage date, and a
  field notes sign-off line.

``` r

atlas_pdf <- tempfile(pattern = "vantor_atlas_", fileext = ".pdf")

vantor_field_atlas(
  grid = vintage_grid,
  points = capi_samples,
  output_file = atlas_pdf,
  title = "Denver Survey Enumeration Atlas",
  buffer_m = 50,
  fetch_basemap = FALSE, # Set fetch_basemap = TRUE when authenticated to embed Vantor RGB imagery
  verbose = FALSE
)

file.exists(atlas_pdf)
#> [1] TRUE
```

When authenticated with Vantor
([`vantor_has_auth()`](https://michael-cw.github.io/vantorr/reference/vantor_has_auth.md)),
simply set `fetch_basemap = TRUE` (or pass a pre-downloaded `SpatRaster`
via `raster = ...`) to render the 30 cm or 15 cm Vantor Vivid RGB
satellite basemap underneath every page of the PDF atlas:

``` r

vantor_field_atlas(
  grid = vintage_grid,
  points = capi_samples,
  output_file = "denver_field_atlas.pdf",
  fetch_basemap = TRUE,
  resolution = 0.3,
  product = "VIVID_STANDARD_30"
)
```

------------------------------------------------------------------------

## 5. Packaging Offline Mobile GIS & Survey Solutions Bundles (`vantor_offline_package()` & `vantor_write_tpk()`)

To deploy your survey grid, CAPI sample points, seamline vintage
polygons, and georeferenced Vantor satellite imagery to mobile CAPI and
GIS apps—including **World Bank Survey Solutions** (v24.09+ built on the
**ArcGIS Maps SDK for .NET v200.x**), **QField**, **Mergin Maps**, and
**ArcGIS Field Maps**—use
[`vantor_offline_package()`](https://michael-cw.github.io/vantorr/reference/vantor_offline_package.md)
and
[`vantor_write_tpk()`](https://michael-cw.github.io/vantorr/reference/vantor_write_tpk.md):

- **Esri Tile Packages (`.tpk` / `.tpkx`) and Clean Upload Archives
  (`_tpk.zip`)**:
  [`vantor_write_tpk()`](https://michael-cw.github.io/vantorr/reference/vantor_write_tpk.md)
  builds multi-zoom Web Mercator (`EPSG:3857` / Esri `WKID: 102100`)
  tile pyramids in pure R without requiring proprietary ArcGIS software
  or ArcGIS Online credits. Setting `zip_for_suso = TRUE` also creates a
  `<stem>_tpk.zip` archive stripped of macOS `__MACOSX/._*` resource
  forks for direct upload to **Survey Solutions Headquarters**
  (`Survey Setup -> Maps`).
- **Zipped Esri Shapefiles (`_shapes.zip`)**: Setting
  `create_shapefiles = TRUE` exports Enumeration Area grid cells and
  sample points as `EPSG:4326` Shapefiles (including a `label` attribute
  column for automatic feature labeling in Survey Solutions) zipped at
  the archive root.
- **Survey Solutions `.tab` Preload (`_suso_preload.tab`) & ODK `.csv`
  (`_samples.csv`)**: Automatically generated whenever `samples` is
  supplied.
- **Web Mercator GeoTIFFs (`.tif`)**: Any included raster basemap is
  automatically reprojected to `EPSG:3857` so Survey Solutions’ Esri
  `RasterLayer` engine renders it accurately.

``` r

bundle <- vantor_offline_package(
  grid = vintage_grid,
  samples = capi_samples,
  seamlines = mock_seamlines,
  create_shapefiles = TRUE, # Zipped Shapefiles for Survey Solutions HQ
  fetch_basemap = FALSE,    # Set TRUE to download and include the GeoTIFF basemap
  package_name = "denver_survey_ea",
  verbose = FALSE
)

# Inspect the generated multi-layer GeoPackage, ODK CSV, Survey Solutions .tab, and Shapefile ZIP
bundle$layers
#> [1] "survey_grid"       "survey_samples"    "imagery_seamlines"
basename(c(bundle$gpkg_path, bundle$csv_path, bundle$suso_tab_path, bundle$shapefile_zip_path))
#> [1] "denver_survey_ea.gpkg"             "denver_survey_ea_samples.csv"     
#> [3] "denver_survey_ea_suso_preload.tab" "denver_survey_ea_shapes.zip"
st_layers(bundle$gpkg_path)
#> Driver: GPKG 
#> Available layers:
#>          layer_name geometry_type features fields crs_name
#> 1       survey_grid       Polygon       16     15   WGS 84
#> 2    survey_samples         Point        4     11   WGS 84
#> 3 imagery_seamlines       Polygon        2      5   WGS 84
```

To generate a standalone multi-zoom `.tpk` (and clean `_tpk.zip` upload
archive) directly from Vantor WMS imagery or an existing
[`terra::SpatRaster`](https://rspatial.github.io/terra/reference/SpatRaster-class.html):

``` r

# Online: fetch zoom levels 15-17 directly from Vantor WMS into a .tpk + _tpk.zip
vantor_write_tpk(
  x = vintage_grid,
  output_file = "denver_ea_basemap.tpk",
  zoom = 15:17,
  product = "VIVID_STANDARD_30",
  zip_for_suso = TRUE
)
```

For a step-by-step guide covering Survey Solutions Headquarters upload
requirements, `WKID: 102100` coordinate conventions, `.tab` preloading,
and automated server upload & assignment creation using the
[**`SurveySolutionsAPIv2`**](https://github.com/michael-cw/SurveySolutionsAPIv2)
R package, see
[`vignette("survey-solutions-workflow", package = "vantorr")`](https://michael-cw.github.io/vantorr/articles/survey-solutions-workflow.md).
