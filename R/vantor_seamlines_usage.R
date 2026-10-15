#' Get Vantor Basemap Seamlines as an `sf` Object (OGC WFS)
#'
#' Queries the Vantor / Maxar Basemap Seamlines OGC Web Feature Service (WFS)
#' to retrieve vector polygons representing the individual satellite imagery
#' strips that make up a Vivid Mosaic basemap within an area of interest.
#'
#' Each returned polygon feature typically includes metadata attributes such as:
#' - `acq_time`, `acq_time_earliest`, `acq_time_latest`: Imagery acquisition timestamp(s)
#' - `gsd`: Ground sample distance (resolution)
#' - `sensor` / `vehicle_name`: Satellite sensor identifier (e.g., `"WV03"`, `"WV02"`)
#' - `ona`: Off-nadir angle
#' - `catid`: Archive catalog ID of the source strip
#'
#' @param bbox A spatial object (`sf`, `sfc`, `bbox`, `SpatRaster`, `SpatVector`,
#'   `SpatExtent`, `stars`, `stars_proxy`) or a numeric vector of length 4:
#'   `c(xmin, ymin, xmax, ymax)`.
#' @param product Character string. Vivid mosaic product name. Default is
#'   `"VIVID_STANDARD_30"`. Options include `"VIVID_STANDARD_30"` and
#'   `"VIVID_ADVANCED_15"`.
#' @param cql_filter Character string. Optional custom CQL filter. If `NULL`,
#'   constructed automatically from `product` and `bbox`.
#' @param token Character string. Optional OAuth2 bearer token.
#' @param api_key Character string. Optional Maxar/Vantor API key.
#' @param verbose Logical. Print progress messages. Default is
#'   `getOption("vantorr.verbose", TRUE)`.
#'
#' @return An `sf` object (in `EPSG:4326`) containing the seamline polygons and
#'   their acquisition metadata, or `NULL` invisibly if the API request fails.
#'
#' @examples
#' \donttest{
#' if (vantor_has_auth()) {
#'   aoi <- sf::st_bbox(
#'     c(xmin = -105.01, ymin = 39.73, xmax = -104.99, ymax = 39.75),
#'     crs = sf::st_crs(4326)
#'   )
#'   seamlines <- vantor_get_seamlines(aoi, verbose = FALSE)
#' }
#' }
#'
#' @export
vantor_get_seamlines <- function(bbox,
                                 product = "VIVID_STANDARD_30",
                                 cql_filter = NULL,
                                 token = NULL,
                                 api_key = NULL,
                                 verbose = getOption("vantorr.verbose", TRUE)) {

  if (missing(bbox) || is.null(bbox)) {
    cli::cli_abort("{.arg bbox} is required.")
  }

  auth <- .resolve_auth(token = token, api_key = api_key, verbose = verbose)

  # WFS 2.0.0 EPSG:4326 BBOX filter uses (ymin, xmin, ymax, xmax)
  bbox_4326 <- as.numeric(vantor_as_bbox(bbox, target_crs = 4326))

  if (is.null(cql_filter)) {
    cql_filter <- sprintf(
      "product_name='%s' AND BBOX(seamline_geometry,%s,%s,%s,%s)",
      product,
      format(bbox_4326[2], scientific = FALSE),
      format(bbox_4326[1], scientific = FALSE),
      format(bbox_4326[4], scientific = FALSE),
      format(bbox_4326[3], scientific = FALSE)
    )
  }

  wfs_url <- "https://api.maxar.com/basemaps/v1/seamlines/wfs"

  request <- create_maxar_request(
    url = wfs_url,
    token = if (identical(auth$type, "bearer")) auth$value else NULL,
    api_key = if (identical(auth$type, "api_key")) auth$value else NULL
  ) |>
    httr2::req_url_query(
      service = "WFS",
      request = "GetFeature",
      version = "2.0.0",
      typeNames = "Maxar:seamline",
      outputFormat = "json",
      srsName = "EPSG:4326",
      cql_filter = cql_filter
    ) |>
    httr2::req_error(is_error = function(resp) FALSE)

  if (verbose) {
    cli::cli_alert_info("Requesting WFS seamline features ({product})...")
  }

  response <- tryCatch(
    httr2::req_perform(request),
    error = function(e) {
      cli::cli_alert_danger("Error connecting to Vantor WFS seamlines endpoint: {conditionMessage(e)}")
      NULL
    }
  )

  if (is.null(response)) {
    return(invisible(NULL))
  }

  if (httr2::resp_status(response) != 200L) {
    error_msg <- tryCatch(
      httr2::resp_body_string(response),
      error = function(e) httr2::resp_status_desc(response)
    )
    cli::cli_alert_danger(
      "WFS seamlines request failed with status {httr2::resp_status(response)}: {substr(error_msg, 1, 300)}"
    )
    return(invisible(NULL))
  }

  geojson_str <- tryCatch(
    httr2::resp_body_string(response),
    error = function(e) NULL
  )

  if (is.null(geojson_str) || !nzchar(geojson_str)) {
    cli::cli_alert_danger("Received empty response from Vantor WFS seamlines endpoint.")
    return(invisible(NULL))
  }

  tmp_geojson <- tempfile(pattern = "vantor_seamlines_", fileext = ".geojson")
  on.exit(unlink(tmp_geojson), add = TRUE)
  writeLines(geojson_str, tmp_geojson, useBytes = TRUE)

  seamlines_sf <- tryCatch(
    sf::st_read(tmp_geojson, quiet = TRUE),
    error = function(e) {
      cli::cli_alert_danger("Failed to parse WFS seamlines response as GeoJSON: {conditionMessage(e)}")
      NULL
    }
  )

  if (is.null(seamlines_sf)) {
    return(invisible(NULL))
  }

  if (verbose) {
    cli::cli_alert_success("Retrieved {nrow(seamlines_sf)} seamline feature{?s} ({product}).")
  }

  seamlines_sf
}


#' Get Vantor Account Usage and Credit Overview
#'
#' Queries the Vantor / Maxar Usage API (`v2/activation/overview`) to retrieve
#' activation credit limits, total usage, and categorized consumption metrics.
#' Useful for monitoring account quota in interactive R sessions or Shiny
#' dashboards before launching large multi-tile downloads.
#'
#' @note **Authentication Requirement (OAuth2 Bearer Token Only):**
#' The Usage API (`https://api.maxar.com/usage/v2/activation/overview`) is part
#' of the Vantor **Account Services (Admin API)**
#' (<https://developers.maxar.com/docs/admin/guides/usage>). Per the Vantor API
#' specification, **API keys (`maxar-api-key`) are not accepted by Admin APIs**.
#' You must authenticate using an OAuth2 Bearer Token via
#' `vantor_auth(username = "...", password = "...")` (or `VANTOR_USERNAME` and
#' `VANTOR_PASSWORD` in `.Renviron`), and your account must have Admin
#' permissions to view activation usage.
#'
#' @param category Character string. Grouping category for usage breakdown.
#'   One of `"PRODUCT"` (default), `"DAY"`, `"WEEK"`, `"MONTH"`, or `"QUARTER"`.
#' @param unit Character string. Measurement unit for usage metrics.
#'   One of `"CREDIT"` (default), `"FEATURE_COUNT"`, `"REQUEST_COUNT"`,
#'   `"MAP_VIEW"`, `"GIGABYTE"`, or `"SQ_KM"`.
#' @param token Character string. Optional OAuth2 bearer token (`access_token`
#'   from [get_maxar_token()]). If `NULL`, resolved from the active OAuth2
#'   session cache or `VANTOR_USERNAME` / `VANTOR_PASSWORD` environment variables.
#' @param verbose Logical. Print summary messages. Default is
#'   `getOption("vantorr.verbose", TRUE)`.
#'
#' @return A list containing the parsed usage overview response (including
#'   `activations`, `usageCategories`, and a tidy `summary_df` data frame when
#'   available), or `NULL` invisibly if the API request fails.
#'
#' @examples
#' \donttest{
#' if (nzchar(Sys.getenv("VANTOR_USERNAME")) && nzchar(Sys.getenv("VANTOR_PASSWORD"))) {
#'   usage <- vantor_get_usage(category = "PRODUCT", unit = "CREDIT", verbose = FALSE)
#' }
#' }
#'
#' @export
vantor_get_usage <- function(category = c("PRODUCT", "DAY", "WEEK", "MONTH", "QUARTER"),
                             unit = c("CREDIT", "FEATURE_COUNT", "REQUEST_COUNT", "MAP_VIEW", "GIGABYTE", "SQ_KM"),
                             token = NULL,
                             verbose = getOption("vantorr.verbose", TRUE)) {

  category <- match.arg(category)
  unit <- match.arg(unit)

  bearer_token <- .resolve_bearer_auth(token = token, verbose = verbose)

  usage_url <- "https://api.maxar.com/usage/v2/activation/overview"

  request <- create_maxar_request(
    url = usage_url,
    token = bearer_token
  ) |>
    httr2::req_url_query(
      category = category,
      unit = unit
    ) |>
    httr2::req_error(is_error = function(resp) FALSE)

  if (verbose) {
    cli::cli_alert_info("Querying Vantor Usage API (category = {category}, unit = {unit})...")
  }

  response <- tryCatch(
    httr2::req_perform(request),
    error = function(e) {
      cli::cli_alert_danger("Error connecting to Vantor Usage API: {conditionMessage(e)}")
      NULL
    }
  )

  if (is.null(response)) {
    return(invisible(NULL))
  }

  if (httr2::resp_status(response) != 200L) {
    error_msg <- tryCatch(
      httr2::resp_body_string(response),
      error = function(e) httr2::resp_status_desc(response)
    )
    cli::cli_alert_danger(
      "Usage API request failed with status {httr2::resp_status(response)}: {substr(error_msg, 1, 300)}"
    )
    return(invisible(NULL))
  }

  usage_data <- tryCatch(
    httr2::resp_body_json(response),
    error = function(e) {
      cli::cli_alert_danger("Failed to parse Usage API JSON response: {conditionMessage(e)}")
      NULL
    }
  )

  if (is.null(usage_data)) {
    return(invisible(NULL))
  }

  # Extract a tidy summary data.frame of activations if present
  if (!is.null(usage_data$activations) && length(usage_data$activations) > 0L) {
    rows <- lapply(usage_data$activations, function(act) {
      data.frame(
        activation_number = as.character(act$activationNumber %||% NA_character_),
        start_date = as.character(act$startDate %||% NA_character_),
        end_date = as.character(act$endDate %||% NA_character_),
        credit_limit = as.numeric(act$creditLimit %||% NA_real_),
        total_credits_used = as.numeric(act$totalCreditsUsed %||% NA_real_),
        percentage_used = as.numeric(act$percentageUsed %||% NA_real_),
        stringsAsFactors = FALSE
      )
    })
    usage_data$summary_df <- do.call(rbind, rows)
  }

  if (verbose) {
    cli::cli_alert_success("Successfully retrieved Vantor account usage overview.")
  }

  usage_data
}

# Internal helper to resolve an OAuth2 Bearer token for Admin APIs (e.g., Usage API)
.resolve_bearer_auth <- function(token = NULL, verbose = FALSE) {
  if (!is.null(token) && nzchar(token)) {
    return(token)
  }

  # Check session cache for bearer token
  if (!is.null(.vantorr_env$access_token) && nzchar(.vantorr_env$access_token)) {
    if (!is.null(.vantorr_env$expires_at) && Sys.time() >= (.vantorr_env$expires_at - 60)) {
      if (!is.null(.vantorr_env$refresh_token) && nzchar(.vantorr_env$refresh_token)) {
        if (verbose) cli::cli_alert_info("Session token expiring; refreshing automatically...")
        refreshed <- refresh_maxar_token(.vantorr_env$refresh_token, verbose = verbose)
        if (!is.null(refreshed) && !is.null(refreshed$access_token)) {
          return(refreshed$access_token)
        }
      }
    } else {
      return(.vantorr_env$access_token)
    }
  }

  # Check environment variables for username & password
  env_user <- Sys.getenv("VANTOR_USERNAME", unset = Sys.getenv("MAXAR_USERNAME", unset = ""))
  env_pass <- Sys.getenv("VANTOR_PASSWORD", unset = Sys.getenv("MAXAR_PASSWORD", unset = ""))
  if (nzchar(env_user) && nzchar(env_pass)) {
    if (verbose) {
      cli::cli_alert_info("Authenticating with OAuth2 credentials from environment variables...")
    }
    tok <- get_maxar_token(username = env_user, password = env_pass, verbose = verbose)
    if (!is.null(tok) && !is.null(tok$access_token)) {
      return(tok$access_token)
    }
  }

  # If only an API key is present, explain specifically why Admin/Usage API rejects it
  has_key <- (!is.null(.vantorr_env$api_key) && nzchar(.vantorr_env$api_key)) ||
    nzchar(Sys.getenv("VANTOR_API_KEY", unset = Sys.getenv("MAXAR_API_KEY", unset = "")))

  if (has_key) {
    cli::cli_abort(c(
      "The Vantor Usage API ({.fn vantor_get_usage}) is an Account Services (Admin) endpoint and requires an OAuth2 Bearer token.",
      "!" = "API keys ({.code maxar-api-key}) are not accepted by Vantor Admin APIs.",
      "i" = "Authenticate with {.code vantor_auth(username = ..., password = ...)}, supply {.arg token}, or set {.envvar VANTOR_USERNAME} and {.envvar VANTOR_PASSWORD} in {.file .Renviron}."
    ))
  }

  cli::cli_abort(c(
    "No Vantor/Maxar OAuth2 bearer token or username/password credentials found for {.fn vantor_get_usage}.",
    "i" = "Call {.code vantor_auth(username = ..., password = ...)}, supply {.arg token}, or set {.envvar VANTOR_USERNAME} and {.envvar VANTOR_PASSWORD} in {.file .Renviron}."
  ))
}

# Internal null-coalescing helper
`%||%` <- function(x, y) {
  if (is.null(x) || length(x) == 0L) y else x
}

