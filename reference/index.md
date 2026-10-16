# Package index

## Authentication & Account Usage

Manage session credentials via API keys or OAuth2 bearer tokens (with
automatic refresh) and monitor account activation credit usage.

- [`vantor_auth()`](https://michael-cw.github.io/vantorr/reference/vantor_auth.md)
  : Authenticate with the Vantor / Maxar API
- [`vantor_has_auth()`](https://michael-cw.github.io/vantorr/reference/vantor_has_auth.md)
  : Check if Vantor Authentication is Available
- [`vantor_auth_clear()`](https://michael-cw.github.io/vantorr/reference/vantor_auth_clear.md)
  : Clear Stored Vantor Credentials
- [`get_maxar_token()`](https://michael-cw.github.io/vantorr/reference/get_maxar_token.md)
  : Get Maxar / Vantor API Bearer Token
- [`refresh_maxar_token()`](https://michael-cw.github.io/vantorr/reference/refresh_maxar_token.md)
  : Refresh Maxar / Vantor API Bearer Token
- [`create_maxar_request()`](https://michael-cw.github.io/vantorr/reference/create_maxar_request.md)
  : Create Maxar / Vantor API Request with Authentication
- [`vantor_get_usage()`](https://michael-cw.github.io/vantorr/reference/vantor_get_usage.md)
  : Get Vantor Account Usage and Credit Overview

## WMS Basemaps & Seamlines

Retrieve high-resolution Vivid Mosaic GeoTIFFs, multi-tile mosaics, and
vector seamline polygons with acquisition metadata.

- [`get_maxar_wms_basemap()`](https://michael-cw.github.io/vantorr/reference/get_maxar_wms_basemap.md)
  : Get Maxar / Vantor WMS Basemap as GeoTIFF or Spatial Raster Object

- [`get_maxar_wms_tiles()`](https://michael-cw.github.io/vantorr/reference/get_maxar_wms_tiles.md)
  : Get Multiple WMS Basemap Tiles for Large Areas

- [`vantor_get_seamlines()`](https://michael-cw.github.io/vantorr/reference/vantor_get_seamlines.md)
  :

  Get Vantor Basemap Seamlines as an `sf` Object (OGC WFS)

## Fieldwork & Survey Data Collection Resources

Generate alphanumeric survey index grids, audit imagery vintage across
Enumeration Areas, encode offline Plus Codes, export CAPI preload files
(ODK, Kobo, Survey123), print multi-page PDF Field Atlases, and bundle
offline GeoPackages and Esri Tile Packages (.tpk / .tpkx) for Survey
Solutions and ArcGIS Field Maps.

- [`vantor_survey_grid()`](https://michael-cw.github.io/vantorr/reference/vantor_survey_grid.md)
  : Generate an Alphanumeric Survey Index Grid over an Area of Interest

- [`vantor_vintage_report()`](https://michael-cw.github.io/vantorr/reference/vantor_vintage_report.md)
  : Audit Basemap Imagery Vintage and Sensor Quality Across Survey Areas

- [`vantor_plus_code()`](https://michael-cw.github.io/vantorr/reference/vantor_plus_code.md)
  : Encode Coordinates as Open Location Codes (Google Plus Codes)

- [`vantor_export_capi()`](https://michael-cw.github.io/vantorr/reference/vantor_export_capi.md)
  : Prepare and Export Survey Sample Points for CAPI Fieldwork Apps

- [`vantor_field_atlas()`](https://michael-cw.github.io/vantorr/reference/vantor_field_atlas.md)
  : Generate a Printable Multi-Page Field Atlas (PDF Map Book)

- [`vantor_offline_package()`](https://michael-cw.github.io/vantorr/reference/vantor_offline_package.md)
  : Package Offline Fieldwork Vector Layers and Satellite Basemaps

- [`vantor_write_tpk()`](https://michael-cw.github.io/vantorr/reference/vantor_write_tpk.md)
  :

  Write an Esri Tile Package (`.tpk` or `.tpkx`) for Offline Field
  Basemaps

## Interactive Mapping (leaflet)

Stream Vantor Vivid mosaics, seamline overlays, and archive strips
directly into interactive leaflet web maps.

- [`vantor_wmts_url()`](https://michael-cw.github.io/vantorr/reference/vantor_wmts_url.md)
  : Construct a Vantor OGC WMTS Tile URL Template
- [`add_vantor_basemap()`](https://michael-cw.github.io/vantorr/reference/add_vantor_basemap.md)
  [`addVantorBasemap()`](https://michael-cw.github.io/vantorr/reference/add_vantor_basemap.md)
  : Add a Vantor Vivid Mosaic Basemap to a Leaflet Map
- [`add_vantor_seamlines()`](https://michael-cw.github.io/vantorr/reference/add_vantor_seamlines.md)
  [`addVantorSeamlines()`](https://michael-cw.github.io/vantorr/reference/add_vantor_seamlines.md)
  : Add Vantor Basemap Seamlines to a Leaflet Map
- [`add_vantor_imagery()`](https://michael-cw.github.io/vantorr/reference/add_vantor_imagery.md)
  [`addVantorImagery()`](https://michael-cw.github.io/vantorr/reference/add_vantor_imagery.md)
  : Add Vantor Archive Streaming Imagery to a Leaflet Map

## R Shiny Application Modules

Drop-in, namespace-safe Shiny UI and server modules for authentication,
interactive AOI selection, seamline inspection, and GeoTIFF export.

- [`vantor_auth_ui()`](https://michael-cw.github.io/vantorr/reference/vantor_auth_ui.md)
  : Vantor Authentication Shiny Module UI
- [`vantor_auth_server()`](https://michael-cw.github.io/vantorr/reference/vantor_auth_server.md)
  : Vantor Authentication Shiny Module Server
- [`vantor_explorer_ui()`](https://michael-cw.github.io/vantorr/reference/vantor_explorer_ui.md)
  : Vantor Interactive Imagery Explorer Shiny Module UI
- [`vantor_explorer_server()`](https://michael-cw.github.io/vantorr/reference/vantor_explorer_server.md)
  : Vantor Interactive Imagery Explorer Shiny Module Server
- [`run_vantor_demo_app()`](https://michael-cw.github.io/vantorr/reference/run_vantor_demo_app.md)
  : Launch Interactive Vantor Explorer Demo Shiny App

## Spatial Dispatch & Coordinate Helpers

S3 bounding box coercion across sf, terra, and stars spatial classes.

- [`vantor_as_bbox()`](https://michael-cw.github.io/vantorr/reference/vantor_as_bbox.md)
  : Coerce Spatial Objects to a Bounding Box in a Target CRS
- [`convert_bbox_to_3857()`](https://michael-cw.github.io/vantorr/reference/convert_bbox_to_3857.md)
  : Convert Lat/Lon Bounding Box to Web Mercator
