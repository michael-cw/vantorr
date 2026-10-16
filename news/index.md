# Changelog

## vantorr 0.3.0

- Production of Fieldwork Resources for Survey Data Collection
  (`vantor_fieldwork.R`):
  - **Survey Index Grids & Enumeration Area Partitioning**:
    - Added
      [`vantor_survey_grid()`](https://michael-cw.github.io/vantorr/reference/vantor_survey_grid.md)
      to partition an Area of Interest into square or hexagonal
      enumeration cells with alphanumeric spreadsheet labels (`"A1"`,
      `"A2"`, `"B1"`, …), WGS84 centroids, and planar area (`area_m2`).
  - **Imagery Vintage & Quality Auditing**:
    - Added
      [`vantor_vintage_report()`](https://michael-cw.github.io/vantorr/reference/vantor_vintage_report.md)
      to intersect Enumeration Areas or survey grids with OGC WFS
      seamlines
      ([`vantor_get_seamlines()`](https://michael-cw.github.io/vantorr/reference/vantor_get_seamlines.md)),
      computing `earliest_acq_date`, `latest_acq_date`, `max_age_days`,
      `min_gsd_m`, `max_gsd_m`, `max_ona_deg`, contributing `sensors`,
      and flagging outdated cells via `vintage_warning`.
  - **CAPI Preload Export & Offline Plus Codes**:
    - Added
      [`vantor_plus_code()`](https://michael-cw.github.io/vantorr/reference/vantor_plus_code.md)
      to encode WGS84 coordinates into 8-, 10-, or 11-character Open
      Location Codes (Google Plus Codes) in pure R for offline
      navigation.
    - Added
      [`vantor_export_capi()`](https://michael-cw.github.io/vantorr/reference/vantor_export_capi.md)
      to enrich sample points/polygons with `odk_geopoint` strings,
      `plus_code`, `google_maps_url`, grid `cell_id`, and imagery
      vintage metadata, with optional CSV/GeoJSON export (`output_csv`,
      `output_geojson`) for ODK Collect, KoboToolbox, and ArcGIS
      Survey123, plus native tab-delimited `.tab` assignment preload
      export (`output_suso_tab`, `suso_gps_var`) for World Bank Survey
      Solutions.
  - **Printable Field Atlases, Esri Tile Packages (`.tpk` / `.tpkx`) &
    Offline Mobile Bundles**:
    - Added
      [`vantor_field_atlas()`](https://michael-cw.github.io/vantorr/reference/vantor_field_atlas.md)
      to generate multi-page printable PDF Field Atlases (Overview Index
      sheet + per-cell Detail Enumeration Sheets with scale bars, north
      arrows, optional Vantor RGB satellite imagery, and enumerator
      sign-off footers).
    - Added
      [`vantor_write_tpk()`](https://michael-cw.github.io/vantorr/reference/vantor_write_tpk.md)
      to generate multi-zoom Web Mercator (`EPSG:3857` / `WKID: 102100`)
      Esri Tile Packages in pure R—supporting both classic `.tpk`
      (Compact Cache V1) and the Apache-2.0 open specification `.tpkx`
      (Compact Cache V2), plus clean `<stem>_tpk.zip` upload archives
      (`zip_for_suso = TRUE`)—for zoomable offline basemaps in World
      Bank Survey Solutions, ArcGIS Field Maps, and Survey123.
    - Added
      [`vantor_offline_package()`](https://michael-cw.github.io/vantorr/reference/vantor_offline_package.md)
      to bundle multi-layer OGC GeoPackages (`.gpkg`), CAPI preload CSVs
      and Survey Solutions `.tab` files, Web Mercator (`EPSG:3857`)
      GeoTIFF basemaps, optional `.tpk`/`.tpkx` tile pyramids
      (`create_tpk`), and zipped Esri Shapefiles
      (`create_shapefiles = TRUE`) into a portable directory for Survey
      Solutions, QField, Mergin Maps, and ArcGIS Field Maps.
  - **Documentation**:
    - Added vignettes
      `"Producing Fieldwork Resources for Survey Data Collection"`
      (`vignettes/fieldwork-survey-resources.Rmd`) and
      `"Creating Spatial Resources for World Bank Survey Solutions Fieldwork"`
      (`vignettes/survey-solutions-workflow.Rmd`, including automated
      server deployment with `SurveySolutionsAPIv2`).

## vantorr 0.2.0

- R Shiny Modules, Interactive `leaflet` Mapping, WFS Seamlines, Usage
  API & Companion Website:
  - **Interactive `leaflet` Integration (`vantor_leaflet.R`)**:
    - Added
      [`vantor_wmts_url()`](https://michael-cw.github.io/vantorr/reference/vantor_wmts_url.md)
      to generate OGC WMTS `GetTile` URL templates (`EPSG:3857`) for
      `leaflet`, `mapgl`, or `tmap`.
    - Added
      [`add_vantor_basemap()`](https://michael-cw.github.io/vantorr/reference/add_vantor_basemap.md)
      (and alias
      [`addVantorBasemap()`](https://michael-cw.github.io/vantorr/reference/add_vantor_basemap.md))
      to stream Vantor Vivid mosaics (`VIVID_STANDARD_30`,
      `VIVID_ADVANCED_15`) via WMTS or WMS in `leaflet` and
      `leafletProxy`.
    - Added
      [`add_vantor_seamlines()`](https://michael-cw.github.io/vantorr/reference/add_vantor_seamlines.md)
      (and alias
      [`addVantorSeamlines()`](https://michael-cw.github.io/vantorr/reference/add_vantor_seamlines.md))
      supporting both transparent OGC WMS raster seamline overlays
      (`mode = "wms"`) and interactive OGC WFS vector polygons with
      clickable metadata popups (`mode = "wfs"`).
    - Added
      [`add_vantor_imagery()`](https://michael-cw.github.io/vantorr/reference/add_vantor_imagery.md)
      (and alias
      [`addVantorImagery()`](https://michael-cw.github.io/vantorr/reference/add_vantor_imagery.md))
      to stream specific archive strips (`feature_id` or custom
      `cql_filter`) via `/streaming/v1/ogc/wms`.
  - **Seamlines OGC WFS & Usage API v2 (`vantor_seamlines_usage.R`)**:
    - Added
      [`vantor_get_seamlines()`](https://michael-cw.github.io/vantorr/reference/vantor_get_seamlines.md)
      to fetch basemap seamline polygons and acquisition metadata
      (`acq_time`, `gsd`, `sensor`, `ona`, `catid`) as an `sf` object
      via OGC WFS.
    - Added
      [`vantor_get_usage()`](https://michael-cw.github.io/vantorr/reference/vantor_get_usage.md)
      to query the Account Services Usage API (`v2/activation/overview`,
      requiring OAuth2 Bearer Token authentication) for account credit
      limits, remaining quota, and consumption breakdowns.
  - **R Shiny Application Modules (`vantor_shiny.R`,
    `vantor_api_getmap.R`)**:
    - Added drop-in Shiny modules
      [`vantor_auth_ui()`](https://michael-cw.github.io/vantorr/reference/vantor_auth_ui.md)
      /
      [`vantor_auth_server()`](https://michael-cw.github.io/vantorr/reference/vantor_auth_server.md)
      (with automatic periodic OAuth2 token refresh) and
      [`vantor_explorer_ui()`](https://michael-cw.github.io/vantorr/reference/vantor_explorer_ui.md)
      /
      [`vantor_explorer_server()`](https://michael-cw.github.io/vantorr/reference/vantor_explorer_server.md),
      plus
      [`run_vantor_demo_app()`](https://michael-cw.github.io/vantorr/reference/run_vantor_demo_app.md).
    - Added `progress_callback` argument to
      [`get_maxar_wms_tiles()`](https://michael-cw.github.io/vantorr/reference/get_maxar_wms_tiles.md)
      for seamless integration with
      [`shiny::incProgress()`](https://rdrr.io/pkg/shiny/man/withProgress.html).
  - **Documentation & Companion Website**:
    - Added vignette
      `"Interactive Leaflet Maps and Shiny Apps with vantorr"`
      (`vignettes/shiny-leaflet-integration.Rmd`).
    - Added `pkgdown` companion website configuration (`_pkgdown.yml`)
      and GitHub Actions deployment workflow
      (`.github/workflows/pkgdown.yaml`).

## vantorr 0.1.0

- Initial Release Preparation & Core Refactoring:
  - **Authentication & Session Management (`vantor_api_gettoken.R`)**:
    - Added
      [`vantor_auth()`](https://michael-cw.github.io/vantorr/reference/vantor_auth.md),
      [`vantor_has_auth()`](https://michael-cw.github.io/vantorr/reference/vantor_has_auth.md),
      and
      [`vantor_auth_clear()`](https://michael-cw.github.io/vantorr/reference/vantor_auth_clear.md)
      to manage session credentials via OAuth2 Bearer tokens or API keys
      (`maxar-api-key`), with automatic fallback to `VANTOR_API_KEY`,
      `MAXAR_API_KEY`, `VANTOR_USERNAME`, and `VANTOR_PASSWORD`
      environment variables.
    - Added automatic session token caching and transparent refresh
      before expiration.
    - Updated
      [`create_maxar_request()`](https://michael-cw.github.io/vantorr/reference/create_maxar_request.md)
      to support both API keys and Bearer tokens, custom user-agent
      headers, and
      [`httr2::req_retry()`](https://httr2.r-lib.org/reference/req_retry.html)
      for transient HTTP errors (`429`, `500`, `502`, `503`, `504`).
  - **Spatial Ecosystem Dispatch (`spatial_dispatch.R`)**:
    - Added S3 generic
      [`vantor_as_bbox()`](https://michael-cw.github.io/vantorr/reference/vantor_as_bbox.md)
      supporting `sf` (`sf`, `sfc`, `sfg`, `bbox`), `terra`
      (`SpatRaster`, `SpatVector`, `SpatExtent`), `stars` (`stars`,
      `stars_proxy`), and numeric bounding box vectors with automatic
      CRS detection and transformation.
    - Added `return_type` support (`"file"`, `"SpatRaster"`, `"stars"`,
      `"stars_proxy"`) to
      [`get_maxar_wms_basemap()`](https://michael-cw.github.io/vantorr/reference/get_maxar_wms_basemap.md)
      and
      [`get_maxar_wms_tiles()`](https://michael-cw.github.io/vantorr/reference/get_maxar_wms_tiles.md).
  - **WMS Basemap & Tiling (`vantor_api_getmap.R`)**:
    - Enabled and modernized
      [`get_maxar_wms_tiles()`](https://michael-cw.github.io/vantorr/reference/get_maxar_wms_tiles.md)
      using
      [`terra::sprc()`](https://rspatial.github.io/terra/reference/sprc.html)
      and
      [`terra::merge()`](https://rspatial.github.io/terra/reference/merge.html)
      for multi-tile downloads.
    - Added support for `EPSG:4326` and `EPSG:3857` output projections
      (automatically handling WMS 1.3.0 axis ordering) and custom
      `cql_filter` queries in
      [`get_maxar_wms_basemap()`](https://michael-cw.github.io/vantorr/reference/get_maxar_wms_basemap.md).
  - **CRAN Repository Policy Compliance**:
    - Changed default file/directory output arguments in
      [`get_maxar_wms_basemap()`](https://michael-cw.github.io/vantorr/reference/get_maxar_wms_basemap.md)
      and
      [`get_maxar_wms_tiles()`](https://michael-cw.github.io/vantorr/reference/get_maxar_wms_tiles.md)
      to use [`tempfile()`](https://rdrr.io/r/base/tempfile.html) so
      functions never write to the user’s working directory by default.
    - Standardized all messaging on `cli` and ensured graceful failure
      (returning `invisible(NULL)` with an informative message rather
      than an unhandled error) when internet or API resources are
      unavailable.
    - Updated `DESCRIPTION` metadata, `Title`, `Description`, `URL`,
      `BugReports`, and `Suggests` fields to pass
      `R CMD check --as-cran`.
