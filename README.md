
<!-- README.md is generated from README.Rmd. Please edit that file -->

# vantorr

<!-- badges: start -->

[![Lifecycle:
experimental](https://img.shields.io/badge/lifecycle-experimental-orange.svg)](https://lifecycle.r-lib.org/articles/stages.html#experimental)
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
- **Interactive `leaflet` & `shiny` Integration**: Stream WMTS/WMS
  basemaps, seamlines, and archive strips in `leaflet`
  (`add_vantor_basemap()`, `add_vantor_seamlines()`,
  `add_vantor_imagery()`), or embed drop-in Shiny modules
  (`vantor_auth_ui()`, `vantor_explorer_ui()`, `run_vantor_demo_app()`).

## Installation

You can install the development version of `vantorr` from
[GitHub](https://github.com/michael-cw/vantorr) with:

``` r
# install.packages("devtools")
devtools::install_github("michael-cw/vantorr, build_vignettes = TRUE")
```

## Authentication

The Vantor API supports both **API Keys** (long-lived, up to 180 days;
required for browser-based `leaflet` tile streaming) and **OAuth 2.0
Bearer Tokens** (session-based, with automatic refresh).

You can configure credentials in your `.Renviron` file
(`VANTOR_API_KEY`, or `VANTOR_USERNAME` and `VANTOR_PASSWORD`) or set
them interactively for your R session with `vantor_auth()`:

``` r
library(vantorr)

# Option 1: Set an API Key for the session
vantor_auth(api_key = "your_api_key")

# Option 2: Authenticate via OAuth2 (automatically caches and refreshes tokens)
vantor_auth(username = "your_email@example.com", password = "your_password")

# Check account credit usage and quota
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
a complete guide to building custom `leaflet` + `shiny` applications.

## License

MIT
