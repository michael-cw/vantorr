
<!-- README.md is generated from README.Rmd. Please edit that file -->

# vantorr

<!-- badges: start -->

[![Lifecycle:
stable](https://img.shields.io/badge/lifecycle-stable-brightgreen.svg)](https://lifecycle.r-lib.org/articles/stages.html#stable)
[![CRAN
status](https://www.r-pkg.org/badges/version/vantorr)](https://CRAN.R-project.org/package=vantorr)
<!-- badges: end -->

The `vantorr` package provides an R interface to the [Maxar / Vantor Hub
APIs](https://developers.maxar.com/docs/), enabling programmatic access
to high-resolution satellite imagery (such as Vivid Mosaic basemaps,
WorldView archives, and analysis-ready data). It natively integrates
with `sf`, `terra`, `stars`, `leaflet`, and `shiny`.

## Key Features

- **Flexible Authentication**: Session credential management via API
  Keys (`maxar-api-key`) or OAuth 2.0 Bearer Tokens (`vantor_auth()`)
  with automatic token caching and refresh.
- **Multi-Package Spatial Dispatch**: Pass `sf`, `terra` (`SpatRaster`,
  `SpatVector`, `SpatExtent`), or `stars` objects to any spatial query
  via `vantor_as_bbox()`, and receive outputs as GeoTIFF files,
  `SpatRaster`, or `stars` objects.
- **Basemaps, Tiling & Seamline Metadata**: Download Vivid 30 cm / 15 cm
  GeoTIFFs (`get_maxar_wms_basemap()`), automatically tile and mosaic
  large areas (`get_maxar_wms_tiles()`), and retrieve vector seamline
  polygons with acquisition dates and sensor metadata via OGC WFS
  (`vantor_get_seamlines()`).
- **Account Quota Monitoring**: Query credit limits and consumption
  breakdowns via the Usage API v2 (`vantor_get_usage()`).
- **Survey Fieldwork & World Bank Survey Solutions Resources**: Create
  alphanumeric survey index grids (`vantor_survey_grid()`), audit
  imagery vintage across Enumeration Areas (`vantor_vintage_report()`),
  encode offline Google Plus Codes (`vantor_plus_code()`), export CAPI &
  Survey Solutions `.tab` preload tables (`vantor_export_capi()`),
  generate multi-page printable PDF Field Atlases
  (`vantor_field_atlas()`), build multi-zoom Esri Tile Packages (`.tpk`
  / `.tpkx` and `_tpk.zip` via `vantor_write_tpk()`), and bundle offline
  GeoPackages & Shapefiles (`vantor_offline_package()`).
- **Interactive `leaflet` & `shiny` Integration**: Stream WMTS/WMS
  basemaps, seamlines, and archive strips in `leaflet`
  (`add_vantor_basemap()`, `add_vantor_seamlines()`,
  `add_vantor_imagery()`), or embed drop-in Shiny modules
  (`vantor_auth_ui()`, `vantor_explorer_ui()`, `run_vantor_demo_app()`).

## Installation

Once available on CRAN, you can install `vantorr` with:

``` r
install.packages("vantorr")
```

You can also install the development version of `vantorr` from
[GitHub](https://github.com/michael-cw/vantorr) with:

``` r
# install.packages("devtools")
devtools::install_github("michael-cw/vantorr", build_vignettes = TRUE)
```

## Authentication

The Vantor API supports both **API Keys** (long-lived, up to 180 days)
and **OAuth 2.0 Bearer Tokens** (session-based, with automatic refresh).
Note that different Vantor API services accept different credential
types:

- **OGC & Non-Administrative APIs** (`get_maxar_wms_basemap()`,
  `get_maxar_wms_tiles()`, `vantor_get_seamlines()`): Accept **either**
  an API Key or an OAuth2 Bearer Token.
- **Browser-Based `leaflet` Tile Layers** (`add_vantor_basemap()`,
  `add_vantor_seamlines()`, `add_vantor_imagery()`): Require an **API
  Key** (`maxar_api_key`).
- **Account Services / Admin APIs** (`vantor_get_usage()`): Require an
  **OAuth2 Bearer Token** (`username` and `password`), as Vantor’s Admin
  APIs do not accept API keys.

You can configure credentials in your `.Renviron` file
(`VANTOR_API_KEY`, or `VANTOR_USERNAME` and `VANTOR_PASSWORD`) or set
them interactively for your R session with `vantor_auth()`:

``` r
library(vantorr)

# Option 1: Set an API Key for the session (for OGC & Leaflet functions)
vantor_auth(api_key = "your_api_key")

# Option 2: Authenticate via OAuth2 (automatically caches and refreshes tokens;
# required for Admin endpoints such as vantor_get_usage())
vantor_auth(username = "your_email@example.com", password = "your_password")

# Check account credit usage and quota (requires OAuth2 Bearer token & Admin permissions)
usage <- vantor_get_usage(category = "PRODUCT", unit = "CREDIT")
```

## Retrieving WMS Basemaps & Seamlines

`vantorr` accepts spatial objects from `sf`, `terra`, and `stars` (as
well as numeric bounding boxes in `EPSG:4326` or `EPSG:3857`) and
automatically transforms coordinates to the required projection.

``` r
library(vantorr)
library(sf)
library(terra)

# Define an Area of Interest (AOI) in WGS84 (EPSG:4326)
aoi <- st_bbox(
  c(xmin = -77.040, ymin = 38.888, xmax = -77.030, ymax = 38.892),
  crs = st_crs(4326)
)

# Retrieve a 30cm Vivid Basemap directly as a terra SpatRaster
basemap_rast <- get_maxar_wms_basemap(
  bbox = aoi,
  resolution = 0.3,
  product = "VIVID_STANDARD_30",
  return_type = "SpatRaster"
)

terra::plotRGB(basemap_rast, r = 1, g = 2, b = 3, stretch = "lin")

# Fetch vector seamline polygons and acquisition timestamps (OGC WFS)
seamlines_sf <- vantor_get_seamlines(bbox = aoi, product = "VIVID_STANDARD_30")
```

For larger areas of interest that exceed single-request pixel limits,
use `get_maxar_wms_tiles()` to automatically grid, download, and mosaic
tiles:

``` r
large_mosaic <- get_maxar_wms_tiles(
  bbox = aoi,
  tile_size = 2500,
  resolution = 0.3,
  merge_tiles = TRUE,
  return_type = "SpatRaster"
)
```

## Fieldwork Resources for Survey Data Collection

Generate alphanumeric survey index grids, audit imagery vintage across
Enumeration Areas, prepare CAPI sample preload tables (with offline
Google Plus Codes, ODK `geopoint` strings, and **World Bank Survey
Solutions** `.tab` assignment files), print multi-page PDF Field
Atlases, and export offline Esri Tile Packages (`.tpk` / `.tpkx` and
clean `_tpk.zip` upload archives) for **Survey Solutions** and **ArcGIS
Field Maps**:

``` r
# 1. Create a 500 m alphanumeric survey grid (A1, A2, B1, ...)
grid_sf <- vantor_survey_grid(aoi, cell_size = 500)

# 2. Audit imagery acquisition dates and flag outdated cells (> 2 years old)
vintage_grid <- vantor_vintage_report(grid_sf, seamlines = seamlines_sf, max_age_days = 730)

# 3. Export CAPI & Survey Solutions preload table and multi-page PDF Atlas
capi_pts <- vantor_export_capi(
  sample_pts,
  grid = vintage_grid,
  seamlines = seamlines_sf,
  output_suso_tab = "suso_assignments.tab",
  suso_gps_var = "hh_gps"
)
vantor_field_atlas(vintage_grid, points = capi_pts, raster = basemap_rast, output_file = "field_atlas.pdf")

# 4. Write a zoomable Esri Tile Package (.tpk + clean _tpk.zip) for Survey Solutions HQ
vantor_write_tpk(
  raster = basemap_rast,
  output_file = "ea_basemap.tpk",
  zoom = 15:17,
  zip_for_suso = TRUE
)
```

See `vignette("fieldwork-survey-resources", package = "vantorr")` and
`vignette("survey-solutions-workflow", package = "vantorr")` for the
complete fieldwork and Survey Solutions guides.

## Interactive `leaflet` Maps & R Shiny Modules

Stream Vantor Vivid mosaics (`WMTS` or `WMS`), seamline cutlines
(`mode = "wms"` or interactive `mode = "wfs"` polygons), and archive
strips directly into `leaflet` maps, or embed the pre-built Shiny
modules (`vantor_auth_ui()` and `vantor_explorer_ui()`):

``` r
library(leaflet)
library(vantorr)

# Stream Vantor Vivid 30cm WMTS tiles and seamlines in Leaflet
leaflet() |>
  setView(lng = -104.995, lat = 39.740, zoom = 15) |>
  add_vantor_basemap(
    product = "VIVID_STANDARD_30",
    service = "WMTS",
    show_seamlines = TRUE
  )

# Launch the interactive Shiny Explorer demo app
if (interactive()) {
  run_vantor_demo_app()
}
```

See `vignette("shiny-leaflet-integration", package = "vantorr")` or
visit the [companion website](https://michael-cw.github.io/vantorr/) for
full documentation.

## Acknowledgments

Parts of the development of this package were co-financed by the Yemen
Resilience, Recovery, and Reconstruction Trust Fund (Yemen Fund), which
was established by the World Bank in 2022 as a pooled funding mechanism
and a partnership platform to support Yemen’s transition over a 10-year
horizon. The Yemen Fund is supported by the United Kingdom’s Foreign,
Commonwealth and Development Office, the Kingdom of the Netherlands, the
Swiss Confederation, and the European Union Delegation to Yemen. The
author would like to express gratitude for this important and timely
contribution.

For more information about the Yemen Fund, visit:
<https://www.worldbank.org/en/programs/the-yemen-fund>

## License

MIT
