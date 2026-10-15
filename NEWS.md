# vantorr 0.1.0

* Initial release Preparation & Core Refactoring:
  * **Authentication & Session Management (`vantor_api_gettoken.R`)**:
    * Added `vantor_auth()`, `vantor_has_auth()`, and `vantor_auth_clear()` to manage session credentials via OAuth2 Bearer tokens or API keys (`maxar-api-key`), with automatic fallback to `VANTOR_API_KEY`, `MAXAR_API_KEY`, `VANTOR_USERNAME`, and `VANTOR_PASSWORD` environment variables.
    * Added automatic session token caching and transparent refresh before expiration.
    * Updated `create_maxar_request()` to support both API keys and Bearer tokens, custom user-agent headers, and `httr2::req_retry()` for transient HTTP errors (`429`, `500`, `502`, `503`, `504`).
  * **Spatial Ecosystem Dispatch (`spatial_dispatch.R`)**:
    * Added S3 generic `vantor_as_bbox()` supporting `sf` (`sf`, `sfc`, `sfg`, `bbox`), `terra` (`SpatRaster`, `SpatVector`, `SpatExtent`), `stars` (`stars`, `stars_proxy`), and numeric bounding box vectors with automatic CRS detection and transformation.
    * Added `return_type` support (`"file"`, `"SpatRaster"`, `"stars"`, `"stars_proxy"`) to `get_maxar_wms_basemap()` and `get_maxar_wms_tiles()`.
  * **WMS Basemap & Tiling (`vantor_api_getmap.R`)**:
    * Enabled and modernized `get_maxar_wms_tiles()` using `terra::sprc()` and `terra::merge()` for multi-tile downloads.
    * Added support for `EPSG:4326` and `EPSG:3857` output projections (automatically handling WMS 1.3.0 axis ordering) and custom `cql_filter` queries in `get_maxar_wms_basemap()`.
  * **CRAN Repository Policy Compliance**:
    * Changed default file/directory output arguments in `get_maxar_wms_basemap()` and `get_maxar_wms_tiles()` to use `tempfile()` so functions never write to the user's working directory by default.
    * Standardized all messaging on `cli` and ensured graceful failure (returning `invisible(NULL)` with an informative message rather than an unhandled error) when internet or API resources are unavailable.
    * Updated `DESCRIPTION` metadata, `Title`, `Description`, `URL`, `BugReports`, and `Suggests` fields to pass `R CMD check --as-cran`.
