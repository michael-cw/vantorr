# Internal helper to resolve an API key for browser-based Leaflet tile requests
.resolve_leaflet_api_key <- function(api_key = NULL, verbose = TRUE) {
  if (!is.null(api_key) && nzchar(api_key)) {
    return(api_key)
  }

  if (!is.null(.vantorr_env$api_key) && nzchar(.vantorr_env$api_key)) {
    return(.vantorr_env$api_key)
  }

  env_key <- Sys.getenv("VANTOR_API_KEY", unset = Sys.getenv("MAXAR_API_KEY", unset = ""))
  if (nzchar(env_key)) {
    .vantorr_env$api_key <- env_key
    return(env_key)
  }

  if (!is.null(.vantorr_env$access_token) && nzchar(.vantorr_env$access_token)) {
    cli::cli_abort(c(
      "Browser-based {.pkg leaflet} tile layers require a Vantor API key ({.code maxar_api_key}).",
      "i" = "An OAuth2 bearer token is currently active, but web browsers cannot attach {.code Authorization: Bearer} headers to standard {.code <img>} tile requests.",
      "i" = "Supply {.arg api_key}, call {.code vantor_auth(api_key = ...)}, or set {.envvar VANTOR_API_KEY} in {.file .Renviron}."
    ))
  }

  cli::cli_abort(c(
    "A Vantor API key is required for {.pkg leaflet} tile streaming.",
    "i" = "Supply {.arg api_key}, call {.code vantor_auth(api_key = ...)}, or set {.envvar VANTOR_API_KEY} in {.file .Renviron}."
  ))
}


#' Construct a Vantor OGC WMTS Tile URL Template
#'
#' Generates an OGC Web Map Tile Service (WMTS) URL template (`{z}/{x}/{y}`)
#' for streaming Vantor / Maxar Vivid Mosaic basemap tiles in `EPSG:3857`
#' (Web Mercator) within `leaflet`, `mapgl`, or `tmap`.
#'
#' @param product Character string. Vivid product name. Default is
#'   `"VIVID_STANDARD_30"`. Options include `"VIVID_STANDARD_30"` and
#'   `"VIVID_ADVANCED_15"`.
#' @param api_key Character string. Optional Vantor/Maxar API key. If `NULL`,
#'   resolved from `vantor_auth()` or environment variables (`VANTOR_API_KEY`
#'   / `MAXAR_API_KEY`).
#' @param format Character string. Tile image format. Default is `"image/jpeg"`.
#'   Options include `"image/jpeg"`, `"image/png"`, and `"image/vnd.jpeg-png"`.
#' @param cql_filter Character string. Optional custom CQL filter. If `NULL`,
#'   constructed automatically from `product`.
#' @param verbose Logical. Print confirmation message. Default is `FALSE`.
#'
#' @return A character string containing the WMTS `GetTile` URL template with
#'   `{z}`, `{x}`, and `{y}` placeholders.
#'
#' @examples
#' url_template <- vantor_wmts_url(
#'   product = "VIVID_STANDARD_30",
#'   api_key = "example_api_key"
#' )
#'
#' @export
vantor_wmts_url <- function(product = "VIVID_STANDARD_30",
                            api_key = NULL,
                            format = "image/jpeg",
                            cql_filter = NULL,
                            verbose = FALSE) {

  key <- .resolve_leaflet_api_key(api_key = api_key, verbose = verbose)

  if (is.null(cql_filter)) {
    cql_filter <- sprintf("productName='%s'", product)
  }

  base_url <- "https://api.maxar.com/basemaps/v1/ogc/gwc/service/wmts"
  url_template <- paste0(
    base_url,
    "?service=WMTS",
    "&request=GetTile",
    "&version=1.0.0",
    "&layer=Maxar:Imagery",
    "&tileMatrixSet=EPSG:3857",
    "&tileMatrix=EPSG:3857:{z}",
    "&tileCol={x}",
    "&tileRow={y}",
    "&format=", utils::URLencode(format, reserved = TRUE),
    "&cql_filter=", utils::URLencode(cql_filter, reserved = TRUE),
    "&maxar_api_key=", utils::URLencode(key, reserved = TRUE)
  )

  if (verbose) {
    cli::cli_alert_success("Generated Vantor WMTS URL template for {.val {product}}.")
  }

  url_template
}


#' Add a Vantor Vivid Mosaic Basemap to a Leaflet Map
#'
#' Adds a high-resolution Vantor / Maxar Vivid Mosaic basemap (`VIVID_STANDARD_30`
#' or `VIVID_ADVANCED_15`) to a `leaflet` or `leafletProxy` map widget using
#' either pre-cached OGC WMTS tiles (recommended for fast interactive panning)
#' or dynamic OGC WMS rendering.
#'
#' @param map A `leaflet` or `leaflet_proxy` map widget object.
#' @param product Character string. Vivid product name. Default is
#'   `"VIVID_STANDARD_30"`. Options include `"VIVID_STANDARD_30"` (30 cm) and
#'   `"VIVID_ADVANCED_15"` (15 cm).
#' @param service Character string. OGC service protocol: `"WMTS"` (default,
#'   fastest in browser) or `"WMS"`.
#' @param api_key Character string. Optional Vantor/Maxar API key. If `NULL`,
#'   resolved from session authentication (`vantor_auth()`) or environment
#'   variables (`VANTOR_API_KEY` / `MAXAR_API_KEY`).
#' @param group Character string. Name of the leaflet layer group. Default is
#'   `"Vantor Vivid"`.
#' @param layerId Character string. Optional leaflet layer ID.
#' @param show_seamlines Logical. If `TRUE`, also overlays the corresponding
#'   cutline/seamline layer via [add_vantor_seamlines()]. Default is `FALSE`.
#' @param cql_filter Character string. Optional custom CQL filter.
#' @param shoreline_masking Logical. Apply shoreline masking (used when
#'   `service = "WMS"`). Default is `FALSE`.
#' @param max_zoom Integer. Maximum zoom level for the tile layer. Default is `21L`.
#' @param attribution Character string. Map attribution text.
#' @param options A list of tile options passed to `leaflet::tileOptions()` or
#'   `leaflet::WMSTileOptions()`.
#' @param verbose Logical. Print status messages. Default is
#'   `getOption("vantorr.verbose", TRUE)`.
#'
#' @return The modified `leaflet` or `leaflet_proxy` map object.
#'
#' @examples
#' if (requireNamespace("leaflet", quietly = TRUE)) {
#'   m <- leaflet::leaflet() |>
#'     add_vantor_basemap(
#'       product = "VIVID_STANDARD_30",
#'       api_key = "example_api_key",
#'       verbose = FALSE
#'     )
#' }
#'
#' @export
add_vantor_basemap <- function(map,
                               product = "VIVID_STANDARD_30",
                               service = c("WMTS", "WMS"),
                               api_key = NULL,
                               group = "Vantor Vivid",
                               layerId = NULL,
                               show_seamlines = FALSE,
                               cql_filter = NULL,
                               shoreline_masking = FALSE,
                               max_zoom = 21L,
                               attribution = "Imagery \u00a9 Vantor / Maxar Technologies",
                               options = NULL,
                               verbose = getOption("vantorr.verbose", TRUE)) {

  if (!requireNamespace("leaflet", quietly = TRUE)) {
    cli::cli_abort("Package {.pkg leaflet} is required for {.fn add_vantor_basemap}. Install it with {.code install.packages(\"leaflet\")}.")
  }

  service <- match.arg(service)
  key <- .resolve_leaflet_api_key(api_key = api_key, verbose = verbose)

  if (identical(service, "WMTS")) {
    wmts_url <- vantor_wmts_url(
      product = product,
      api_key = key,
      cql_filter = cql_filter,
      verbose = FALSE
    )

    if (is.null(options)) {
      options <- leaflet::tileOptions(maxZoom = max_zoom, maxNativeZoom = 20L)
    }

    map <- leaflet::addTiles(
      map = map,
      urlTemplate = wmts_url,
      attribution = attribution,
      layerId = layerId,
      group = group,
      options = options
    )
  } else {
    if (is.null(cql_filter)) {
      cql_filter <- sprintf("productName='%s'", product)
      if (isTRUE(shoreline_masking)) {
        cql_filter <- paste0(cql_filter, " OR productName='BASE_MOSAIC'")
      }
    }

    wms_url <- paste0(
      "https://api.maxar.com/basemaps/v1/ogc/wms?maxar_api_key=",
      utils::URLencode(key, reserved = TRUE)
    )

    if (is.null(options)) {
      options <- leaflet::WMSTileOptions(
        format = "image/vnd.jpeg-png",
        transparent = TRUE,
        version = "1.3.0",
        maxZoom = max_zoom,
        cql_filter = cql_filter,
        shoreline_masking = tolower(as.character(shoreline_masking))
      )
    }

    map <- leaflet::addWMSTiles(
      map = map,
      baseUrl = wms_url,
      layers = "Maxar:Imagery",
      layerId = layerId,
      group = group,
      options = options,
      attribution = attribution
    )
  }

  if (verbose) {
    cli::cli_alert_success("Added Vantor {.val {product}} ({service}) basemap layer to leaflet map.")
  }

  if (isTRUE(show_seamlines)) {
    map <- add_vantor_seamlines(
      map = map,
      product = product,
      api_key = key,
      verbose = verbose
    )
  }

  map
}

#' @rdname add_vantor_basemap
#' @export
addVantorBasemap <- add_vantor_basemap


#' Add Vantor Basemap Seamlines to a Leaflet Map
#'
#' Overlays Vantor / Maxar Vivid Mosaic seamlines onto a `leaflet` or
#' `leafletProxy` map. Supports either fast OGC WMS raster cutline rendering
#' (`mode = "wms"`, default) or interactive OGC WFS vector polygons
#' (`mode = "wfs"`) with clickable popups displaying acquisition date (`acq_time`),
#' resolution (`gsd`), sensor (`sensor`), and catalog ID (`catid`).
#'
#' @param map A `leaflet` or `leaflet_proxy` map widget object.
#' @param product Character string. Vivid product name. Default is
#'   `"VIVID_STANDARD_30"`.
#' @param mode Character string. Either `"wms"` (default, transparent raster
#'   overlay across all zoom levels) or `"wfs"` (fetches vector polygons via
#'   [vantor_get_seamlines()] for `bbox` with interactive metadata popups).
#' @param bbox Optional bounding box or spatial object required when
#'   `mode = "wfs"`.
#' @param api_key Character string. Optional Vantor/Maxar API key.
#' @param group Character string. Leaflet layer group name. Default is
#'   `"Vantor Seamlines"`.
#' @param layerId Character string. Optional leaflet layer ID.
#' @param cql_filter Character string. Optional custom CQL filter.
#' @param color Character string. Stroke color for vector polygons when
#'   `mode = "wfs"`. Default is `"#00E5FF"`.
#' @param weight Numeric. Stroke weight when `mode = "wfs"`. Default is `2`.
#' @param options Optional `leaflet::WMSTileOptions()` list when `mode = "wms"`.
#' @param verbose Logical. Print status messages. Default is
#'   `getOption("vantorr.verbose", TRUE)`.
#'
#' @return The modified `leaflet` or `leaflet_proxy` map object.
#'
#' @examples
#' if (requireNamespace("leaflet", quietly = TRUE)) {
#'   m <- leaflet::leaflet() |>
#'     add_vantor_seamlines(
#'       product = "VIVID_STANDARD_30",
#'       api_key = "example_api_key",
#'       verbose = FALSE
#'     )
#' }
#'
#' @export
add_vantor_seamlines <- function(map,
                                 product = "VIVID_STANDARD_30",
                                 mode = c("wms", "wfs"),
                                 bbox = NULL,
                                 api_key = NULL,
                                 group = "Vantor Seamlines",
                                 layerId = NULL,
                                 cql_filter = NULL,
                                 color = "#00E5FF",
                                 weight = 2,
                                 options = NULL,
                                 verbose = getOption("vantorr.verbose", TRUE)) {

  if (!requireNamespace("leaflet", quietly = TRUE)) {
    cli::cli_abort("Package {.pkg leaflet} is required for {.fn add_vantor_seamlines}. Install it with {.code install.packages(\"leaflet\")}.")
  }

  mode <- match.arg(mode)

  if (identical(mode, "wms")) {
    key <- .resolve_leaflet_api_key(api_key = api_key, verbose = verbose)

    if (is.null(cql_filter)) {
      cql_filter <- sprintf("product_name='%s'", product)
    }

    wms_url <- paste0(
      "https://api.maxar.com/basemaps/v1/seamlines/ows?maxar_api_key=",
      utils::URLencode(key, reserved = TRUE)
    )

    if (is.null(options)) {
      options <- leaflet::WMSTileOptions(
        format = "image/png",
        transparent = TRUE,
        version = "1.3.0",
        maxZoom = 21L,
        cql_filter = cql_filter
      )
    }

    map <- leaflet::addWMSTiles(
      map = map,
      baseUrl = wms_url,
      layers = "Maxar:seamline",
      layerId = layerId,
      group = group,
      options = options,
      attribution = "Seamlines \u00a9 Vantor / Maxar Technologies"
    )

    if (verbose) {
      cli::cli_alert_success("Added Vantor WMS seamlines overlay ({product}) to leaflet map.")
    }
  } else {
    if (is.null(bbox)) {
      cli::cli_abort("{.arg bbox} is required when {.code mode = \"wfs\"}.")
    }

    seamlines_sf <- vantor_get_seamlines(
      bbox = bbox,
      product = product,
      cql_filter = cql_filter,
      api_key = api_key,
      verbose = verbose
    )

    if (is.null(seamlines_sf) || nrow(seamlines_sf) == 0L) {
      if (verbose) {
        cli::cli_alert_warning("No WFS seamline polygons returned for the specified {.arg bbox}.")
      }
      return(map)
    }

    popup_html <- .format_seamline_popup(seamlines_sf)

    map <- leaflet::addPolygons(
      map = map,
      data = seamlines_sf,
      layerId = layerId,
      group = group,
      color = color,
      weight = weight,
      fillOpacity = 0.08,
      popup = popup_html
    )

    if (verbose) {
      cli::cli_alert_success("Added {nrow(seamlines_sf)} interactive WFS seamline polygon{?s} to leaflet map.")
    }
  }

  map
}

#' @rdname add_vantor_seamlines
#' @export
addVantorSeamlines <- add_vantor_seamlines

# Internal helper to format HTML popups for WFS seamlines
.format_seamline_popup <- function(sf_obj) {
  df <- sf::st_drop_geometry(sf_obj)
  get_col <- function(col_names) {
    for (nm in col_names) {
      if (nm %in% names(df)) return(as.character(df[[nm]]))
    }
    rep("N/A", nrow(df))
  }

  acq <- get_col(c("acq_time", "acq_time_latest", "acquisitionDate"))
  sensor <- get_col(c("sensor", "vehicle_name"))
  gsd <- get_col(c("gsd", "resolution"))
  ona <- get_col(c("ona", "offNadirAngle"))
  catid <- get_col(c("catid", "featureId", "id"))

  sprintf(
     paste0(
       "<div style='font-family:sans-serif;font-size:12px;'>",
       "<strong>Vantor Seamline Metadata</strong><br/>",
       "<b>Acquisition Date:</b> %s<br/>",
       "<b>Sensor:</b> %s<br/>",
       "<b>GSD (Resolution):</b> %s m<br/>",
       "<b>Off-Nadir Angle:</b> %s&deg;<br/>",
       "<b>Catalog ID:</b> <code>%s</code>",
       "</div>"
     ),
     acq, sensor, gsd, ona, catid
  )
}


#' Add Vantor Archive Streaming Imagery to a Leaflet Map
#'
#' Streams individual archive satellite imagery strips or filtered collections
#' from the Vantor / Maxar Streaming Imagery OGC WMS endpoint
#' (`https://api.maxar.com/streaming/v1/ogc/wms`) onto a `leaflet` or
#' `leafletProxy` map.
#'
#' @param map A `leaflet` or `leaflet_proxy` map widget object.
#' @param feature_id Character string. Optional catalog / feature ID of a
#'   specific satellite strip (e.g., `"10300100EBEDF600"`). Used to construct
#'   `cql_filter = "featureId='<feature_id>'"` if `cql_filter` is `NULL`.
#' @param cql_filter Character string. Optional custom CQL filter, such as
#'   `"(acquisitionDate>='2023-01-01')AND(cloudCover<0.20)"`.
#' @param api_key Character string. Optional Vantor/Maxar API key.
#' @param group Character string. Leaflet layer group name. Default is
#'   `"Vantor Archive Imagery"`.
#' @param layerId Character string. Optional leaflet layer ID.
#' @param format Character string. WMS tile image format. Default is
#'   `"image/vnd.jpeg-png"`.
#' @param transparent Logical. Request transparent background for no-data pixels.
#'   Default is `TRUE`.
#' @param max_zoom Integer. Maximum zoom level. Default is `21L`.
#' @param attribution Character string. Map attribution text.
#' @param options Optional `leaflet::WMSTileOptions()` list.
#' @param verbose Logical. Print status messages. Default is
#'   `getOption("vantorr.verbose", TRUE)`.
#'
#' @return The modified `leaflet` or `leaflet_proxy` map object.
#'
#' @examples
#' if (requireNamespace("leaflet", quietly = TRUE)) {
#'   m <- leaflet::leaflet() |>
#'     add_vantor_imagery(
#'       feature_id = "10300100EBEDF600",
#'       api_key = "example_api_key",
#'       verbose = FALSE
#'     )
#' }
#'
#' @export
add_vantor_imagery <- function(map,
                               feature_id = NULL,
                               cql_filter = NULL,
                               api_key = NULL,
                               group = "Vantor Archive Imagery",
                               layerId = NULL,
                               format = "image/vnd.jpeg-png",
                               transparent = TRUE,
                               max_zoom = 21L,
                               attribution = "Imagery \u00a9 Vantor / Maxar Technologies",
                               options = NULL,
                               verbose = getOption("vantorr.verbose", TRUE)) {

  if (!requireNamespace("leaflet", quietly = TRUE)) {
    cli::cli_abort("Package {.pkg leaflet} is required for {.fn add_vantor_imagery}. Install it with {.code install.packages(\"leaflet\")}.")
  }

  key <- .resolve_leaflet_api_key(api_key = api_key, verbose = verbose)

  if (is.null(cql_filter) && !is.null(feature_id) && nzchar(feature_id)) {
    cql_filter <- sprintf("featureId='%s'", feature_id)
  }

  wms_url <- paste0(
    "https://api.maxar.com/streaming/v1/ogc/wms?maxar_api_key=",
    utils::URLencode(key, reserved = TRUE)
  )

  if (is.null(options)) {
    wms_args <- list(
      format = format,
      transparent = transparent,
      version = "1.3.0",
      maxZoom = max_zoom
    )
    if (!is.null(cql_filter) && nzchar(cql_filter)) {
      wms_args$cql_filter <- cql_filter
    }
    options <- do.call(leaflet::WMSTileOptions, wms_args)
  }

  map <- leaflet::addWMSTiles(
    map = map,
    baseUrl = wms_url,
    layers = "Maxar:Imagery",
    layerId = layerId,
    group = group,
    options = options,
    attribution = attribution
  )

  if (verbose) {
    filter_desc <- if (!is.null(cql_filter)) cql_filter else "default mosaic"
    cli::cli_alert_success("Added Vantor streaming archive imagery ({filter_desc}) to leaflet map.")
  }

  map
}

#' @rdname add_vantor_imagery
#' @export
addVantorImagery <- add_vantor_imagery
