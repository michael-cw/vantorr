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
