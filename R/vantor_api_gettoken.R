# Internal package environment for caching session state and tokens
.vantorr_env <- new.env(parent = emptyenv())

#' Authenticate with the Vantor / Maxar API
#'
#' Sets or retrieves authentication credentials for the current R session.
#' Supports both long-lived API keys (`maxar-api-key`) and OAuth2 Bearer tokens
#' (`Authorization: Bearer <token>`) with automatic refresh when expired.
#'
#' By default, `vantor_auth()` checks the following environment variables if
#' arguments are not explicitly supplied:
#' - `VANTOR_API_KEY` or `MAXAR_API_KEY`
#' - `VANTOR_USERNAME` or `MAXAR_USERNAME`
#' - `VANTOR_PASSWORD` or `MAXAR_PASSWORD`
#'
#' @note **Authentication Scope Across Vantor Endpoints:**
#' - **OGC & Non-Administrative APIs** ([get_maxar_wms_basemap()],
#'   [get_maxar_wms_tiles()], [vantor_get_seamlines()]): Accept **either** an
#'   API key (`maxar-api-key`) or an OAuth2 Bearer token.
#' - **Browser-Based `leaflet` Tile Layers** ([add_vantor_basemap()],
#'   [add_vantor_seamlines()], [add_vantor_imagery()], [vantor_wmts_url()]):
#'   Require an **API key** (`maxar_api_key` URL query parameter).
#' - **Account Services / Admin APIs** ([vantor_get_usage()]): Require an
#'   **OAuth2 Bearer token** (`username` and `password`, or `token`). Per the
#'   Vantor API specification, Admin APIs do not accept API keys.
#'
#' @param api_key Character string. A Vantor/Maxar API key.
#' @param username Character string. Email address for OAuth2 authentication.
#' @param password Character string. Password for OAuth2 authentication.
#' @param token Character string. An existing OAuth2 access token.
#' @param refresh_token Character string. An existing OAuth2 refresh token.
#' @param verbose Logical. Print status messages. Defaults to
#'   `getOption("vantorr.verbose", TRUE)`.
#'
#' @return Invisibly returns a list representing the active authentication state,
#'   or `NULL` if no credentials could be resolved.
#'
#' @examples
#' # Check if credentials are configured in the environment
#' vantor_has_auth(verbose = FALSE)
#'
#' # Set an API key for the current session
#' vantor_auth(api_key = "your_api_key_here", verbose = FALSE)
#'
#' # Clear session credentials
#' vantor_auth_clear(verbose = FALSE)
#'
#' @export
vantor_auth <- function(api_key = NULL,
                        username = NULL,
                        password = NULL,
                        token = NULL,
                        refresh_token = NULL,
                        verbose = getOption("vantorr.verbose", TRUE)) {

  has_explicit <- FALSE
  res <- NULL

  if (!is.null(api_key) && nzchar(api_key)) {
    .vantorr_env$api_key <- api_key
    has_explicit <- TRUE
    if (verbose) cli::cli_alert_success("Vantor API key stored for this session.")
    res <- list(type = "api_key", api_key = api_key)
  }

  if (!is.null(token) && nzchar(token)) {
    .vantorr_env$access_token <- token
    .vantorr_env$refresh_token <- refresh_token
    .vantorr_env$expires_at <- Sys.time() + 3600
    has_explicit <- TRUE
    if (verbose) cli::cli_alert_success("Vantor bearer token stored for this session.")
    res <- list(
      type = "bearer",
      access_token = token,
      refresh_token = refresh_token,
      expires_at = .vantorr_env$expires_at
    )
  }

  if (!is.null(username) && nzchar(username) && !is.null(password) && nzchar(password)) {
    has_explicit <- TRUE
    tok <- get_maxar_token(username = username, password = password, verbose = verbose)
    if (is.null(tok)) return(invisible(res))
    res <- list(
      type = "bearer",
      access_token = .vantorr_env$access_token,
      refresh_token = .vantorr_env$refresh_token,
      expires_at = .vantorr_env$expires_at
    )
  }

  if (has_explicit) {
    return(invisible(res))
  }

  # Fallback to environment variables
  env_key <- Sys.getenv("VANTOR_API_KEY", unset = Sys.getenv("MAXAR_API_KEY", unset = ""))
  if (nzchar(env_key)) {
    .vantorr_env$api_key <- env_key
    if (verbose) {
      cli::cli_alert_success("Vantor API key loaded from environment variable and stored for this session.")
    }
    return(invisible(list(type = "api_key", api_key = env_key)))
  }

  env_user <- Sys.getenv("VANTOR_USERNAME", unset = Sys.getenv("MAXAR_USERNAME", unset = ""))
  env_pass <- Sys.getenv("VANTOR_PASSWORD", unset = Sys.getenv("MAXAR_PASSWORD", unset = ""))
  if (nzchar(env_user) && nzchar(env_pass)) {
    if (verbose) {
      cli::cli_alert_info("Authenticating with credentials from environment variables...")
    }
    tok <- get_maxar_token(username = env_user, password = env_pass, verbose = verbose)
    if (is.null(tok)) return(invisible(NULL))
    return(invisible(list(
      type = "bearer",
      access_token = .vantorr_env$access_token,
      refresh_token = .vantorr_env$refresh_token,
      expires_at = .vantorr_env$expires_at
    )))
  }

  if (verbose) {
    cli::cli_alert_warning("No Vantor/Maxar authentication credentials supplied or found in environment.")
  }
  invisible(NULL)
}

#' Check if Vantor Authentication is Available
#'
#' Checks whether an active session token, API key, or environment credentials
#' are available. Useful for guarding examples and automated tests.
#'
#' @param verbose Logical. If `TRUE`, prints the detected authentication source.
#'   Defaults to `FALSE`.
#'
#' @return Logical `TRUE` if authentication credentials are available, `FALSE` otherwise.
#' @examples
#' vantor_has_auth()
#' @export
vantor_has_auth <- function(verbose = FALSE) {
  if (!is.null(.vantorr_env$api_key) && nzchar(.vantorr_env$api_key)) {
    if (verbose) cli::cli_alert_success("Active session API key found.")
    return(TRUE)
  }
  if (!is.null(.vantorr_env$access_token) && nzchar(.vantorr_env$access_token)) {
    if (verbose) cli::cli_alert_success("Active session OAuth2 bearer token found.")
    return(TRUE)
  }

  env_key <- Sys.getenv("VANTOR_API_KEY", unset = Sys.getenv("MAXAR_API_KEY", unset = ""))
  if (nzchar(env_key)) {
    if (verbose) cli::cli_alert_success("API key found in environment variables.")
    return(TRUE)
  }

  env_user <- Sys.getenv("VANTOR_USERNAME", unset = Sys.getenv("MAXAR_USERNAME", unset = ""))
  env_pass <- Sys.getenv("VANTOR_PASSWORD", unset = Sys.getenv("MAXAR_PASSWORD", unset = ""))
  if (nzchar(env_user) && nzchar(env_pass)) {
    if (verbose) cli::cli_alert_success("Username and password found in environment variables.")
    return(TRUE)
  }

  if (verbose) cli::cli_alert_warning("No Vantor/Maxar credentials found.")
  FALSE
}

#' Clear Stored Vantor Credentials
#'
#' Removes any cached API key or OAuth2 tokens from the internal session environment.
#'
#' @param verbose Logical. Print confirmation message. Defaults to
#'   `getOption("vantorr.verbose", TRUE)`.
#'
#' @return Invisibly returns `TRUE`.
#' @examples
#' vantor_auth_clear(verbose = FALSE)
#' @export
vantor_auth_clear <- function(verbose = getOption("vantorr.verbose", TRUE)) {
  rm(list = ls(envir = .vantorr_env), envir = .vantorr_env)
  if (verbose) {
    cli::cli_alert_success("Cleared stored Vantor session credentials.")
  }
  invisible(TRUE)
}

#' Resolve Active Credentials (Internal)
#' @noRd
.resolve_auth <- function(token = NULL, api_key = NULL, verbose = FALSE) {
  if (!is.null(api_key) && nzchar(api_key)) {
    return(list(type = "api_key", value = api_key))
  }
  if (!is.null(token) && nzchar(token)) {
    return(list(type = "bearer", value = token))
  }

  # Check session cache for API key
  if (!is.null(.vantorr_env$api_key) && nzchar(.vantorr_env$api_key)) {
    return(list(type = "api_key", value = .vantorr_env$api_key))
  }

  # Check session cache for bearer token
  if (!is.null(.vantorr_env$access_token) && nzchar(.vantorr_env$access_token)) {
    # Check if expired or expiring within 60 seconds
    if (!is.null(.vantorr_env$expires_at) && Sys.time() >= (.vantorr_env$expires_at - 60)) {
      if (!is.null(.vantorr_env$refresh_token) && nzchar(.vantorr_env$refresh_token)) {
        if (verbose) cli::cli_alert_info("Session token expiring; refreshing automatically...")
        refreshed <- refresh_maxar_token(.vantorr_env$refresh_token, verbose = verbose)
        if (!is.null(refreshed) && !is.null(refreshed$access_token)) {
          return(list(type = "bearer", value = refreshed$access_token))
        }
      }
    } else {
      return(list(type = "bearer", value = .vantorr_env$access_token))
    }
  }

  # Try initializing from environment variables
  auth_state <- vantor_auth(verbose = verbose)
  if (!is.null(auth_state)) {
    if (identical(auth_state$type, "api_key")) {
      return(list(type = "api_key", value = auth_state$api_key))
    } else if (identical(auth_state$type, "bearer")) {
      return(list(type = "bearer", value = auth_state$access_token))
    }
  }

  cli::cli_abort(c(
    "No Vantor/Maxar authentication credentials provided or found.",
    "i" = "Supply {.arg api_key} or {.arg token}, call {.fn vantor_auth}, or set {.envvar VANTOR_API_KEY} in {.file .Renviron}."
  ))
}


#' Get Maxar / Vantor API Bearer Token
#'
#' Retrieves a bearer token from the Maxar/Vantor Authentication API
#' using the OAuth2 Password Flow and caches it in the current session for
#' automatic refresh.
#'
#' @param username Character string. The email address associated with your Maxar/Vantor account.
#'   Defaults to `Sys.getenv("VANTOR_USERNAME", Sys.getenv("MAXAR_USERNAME"))`.
#' @param password Character string. The password for your Maxar/Vantor account.
#'   Defaults to `Sys.getenv("VANTOR_PASSWORD", Sys.getenv("MAXAR_PASSWORD"))`.
#' @param verbose Logical. If `TRUE`, prints token status details. Defaults to
#'   `getOption("vantorr.verbose", TRUE)`.
#'
#' @return A list containing the token response (`access_token`, `expires_in`,
#'   `refresh_token`, `token_type`), or `NULL` (with an alert) if the API cannot be reached.
#'
#' @examples
#' \donttest{
#' if (vantor_has_auth() && nzchar(Sys.getenv("VANTOR_USERNAME"))) {
#'   token_response <- get_maxar_token(verbose = FALSE)
#' }
#' }
#'
#' @export
get_maxar_token <- function(username = Sys.getenv("VANTOR_USERNAME", unset = Sys.getenv("MAXAR_USERNAME", unset = "")),
                            password = Sys.getenv("VANTOR_PASSWORD", unset = Sys.getenv("MAXAR_PASSWORD", unset = "")),
                            verbose = getOption("vantorr.verbose", TRUE)) {

  if (missing(username) || is.null(username) || !nzchar(username) ||
      missing(password) || is.null(password) || !nzchar(password)) {
    cli::cli_abort("Both {.arg username} and {.arg password} are required.")
  }

  token_url <- "https://account.maxar.com/auth/realms/mds/protocol/openid-connect/token"

  if (verbose) {
    cli::cli_alert_info("Requesting OAuth2 bearer token from Vantor Authentication API...")
  }

  request <- httr2::request(token_url) |>
    httr2::req_method("POST") |>
    httr2::req_headers(
      "Content-Type" = "application/x-www-form-urlencoded"
    ) |>
    httr2::req_body_form(
      client_id = "mgp",
      username = username,
      password = password,
      grant_type = "password"
    ) |>
    httr2::req_retry(max_tries = 3, is_transient = function(resp) {
      httr2::resp_status(resp) %in% c(429L, 500L, 502L, 503L, 504L)
    }) |>
    httr2::req_error(is_error = function(resp) FALSE)

  response <- tryCatch(
    httr2::req_perform(request),
    error = function(e) {
      cli::cli_alert_danger("Could not connect to Vantor Authentication API: {conditionMessage(e)}")
      NULL
    }
  )

  if (is.null(response)) {
    return(invisible(NULL))
  }

  if (httr2::resp_status(response) != 200L) {
    cli::cli_alert_danger(
      "Authentication failed with status {httr2::resp_status(response)} ({httr2::resp_status_desc(response)})."
    )
    return(invisible(NULL))
  }

  token_data <- httr2::resp_body_json(response)

  # Cache in package session environment
  .vantorr_env$access_token <- token_data$access_token
  .vantorr_env$refresh_token <- token_data$refresh_token
  if (!is.null(token_data$expires_in)) {
    .vantorr_env$expires_at <- Sys.time() + as.numeric(token_data$expires_in)
  }

  if (verbose) {
    cli::cli_alert_success("Successfully retrieved and cached OAuth2 bearer token.")
    cli::cli_alert_warning("Token expires in {token_data$expires_in} seconds.")
  }

  token_data
}


#' Refresh Maxar / Vantor API Bearer Token
#'
#' Refreshes a bearer token using a refresh token, avoiding the need to resend
#' username and password, and updates the session token cache.
#'
#' @param refresh_token Character string. The refresh token from a previous token response.
#'   Defaults to the cached session refresh token if available.
#' @param verbose Logical. If `TRUE`, prints token refresh details. Defaults to
#'   `getOption("vantorr.verbose", TRUE)`.
#'
#' @return A list containing the new token response, or `NULL` if refresh fails.
#'
#' @examples
#' \donttest{
#' if (nzchar(Sys.getenv("VANTOR_USERNAME")) && nzchar(Sys.getenv("VANTOR_PASSWORD"))) {
#'   tok <- get_maxar_token(verbose = FALSE)
#'   if (!is.null(tok$refresh_token)) {
#'     new_token <- refresh_maxar_token(tok$refresh_token, verbose = FALSE)
#'   }
#' }
#' }
#'
#' @export
refresh_maxar_token <- function(refresh_token = .vantorr_env$refresh_token,
                                verbose = getOption("vantorr.verbose", TRUE)) {

  if (missing(refresh_token) && is.null(refresh_token)) {
    cli::cli_abort("{.arg refresh_token} is required.")
  }
  if (is.null(refresh_token) || !nzchar(refresh_token)) {
    cli::cli_abort("{.arg refresh_token} is required.")
  }

  token_url <- "https://account.maxar.com/auth/realms/mds/protocol/openid-connect/token"

  if (verbose) {
    cli::cli_alert_info("Refreshing OAuth2 bearer token...")
  }

  request <- httr2::request(token_url) |>
    httr2::req_method("POST") |>
    httr2::req_headers(
      "Content-Type" = "application/x-www-form-urlencoded"
    ) |>
    httr2::req_body_form(
      client_id = "mgp",
      grant_type = "refresh_token",
      refresh_token = refresh_token
    ) |>
    httr2::req_retry(max_tries = 3, is_transient = function(resp) {
      httr2::resp_status(resp) %in% c(429L, 500L, 502L, 503L, 504L)
    }) |>
    httr2::req_error(is_error = function(resp) FALSE)

  response <- tryCatch(
    httr2::req_perform(request),
    error = function(e) {
      cli::cli_alert_danger("Could not connect to Vantor Authentication API to refresh token: {conditionMessage(e)}")
      NULL
    }
  )

  if (is.null(response)) {
    return(invisible(NULL))
  }

  if (httr2::resp_status(response) != 200L) {
    cli::cli_alert_danger(
      "Token refresh failed with status {httr2::resp_status(response)} ({httr2::resp_status_desc(response)})."
    )
    return(invisible(NULL))
  }

  token_data <- httr2::resp_body_json(response)

  .vantorr_env$access_token <- token_data$access_token
  .vantorr_env$refresh_token <- token_data$refresh_token
  if (!is.null(token_data$expires_in)) {
    .vantorr_env$expires_at <- Sys.time() + as.numeric(token_data$expires_in)
  }

  if (verbose) {
    cli::cli_alert_success("Successfully refreshed OAuth2 bearer token.")
    cli::cli_alert_warning("Token expires in {token_data$expires_in} seconds.")
  }

  token_data
}


#' Create Maxar / Vantor API Request with Authentication
#'
#' Helper function to create an `httr2` request with Vantor/Maxar bearer token
#' or API key authentication and built-in retry logic for transient HTTP errors.
#'
#' @param url Character string. The API endpoint URL.
#' @param token Character string. Optional bearer token (`access_token` from `get_maxar_token()`).
#' @param api_key Character string. Optional Maxar/Vantor API key.
#' @param verbose Logical. If `TRUE`, prints confirmation of the configured request.
#'   Defaults to `FALSE`.
#'
#' @return An `httr2_request` object with authentication headers and retry policies set.
#'
#' @examples
#' req <- create_maxar_request(
#'   "https://api.maxar.com/discovery/v1",
#'   api_key = "example_key"
#' )
#'
#' @export
create_maxar_request <- function(url, token = NULL, api_key = NULL, verbose = FALSE) {
  auth <- .resolve_auth(token = token, api_key = api_key, verbose = verbose)

  req <- httr2::request(url) |>
    httr2::req_user_agent("vantorr (https://github.com/michael-cw/vantorr)") |>
    httr2::req_retry(
      max_tries = 3,
      is_transient = function(resp) {
        httr2::resp_status(resp) %in% c(429L, 500L, 502L, 503L, 504L)
      }
    )

  if (identical(auth$type, "api_key")) {
    req <- req |>
      httr2::req_headers("maxar-api-key" = auth$value)
  } else {
    req <- req |>
      httr2::req_headers(Authorization = paste("Bearer", auth$value))
  }

  if (verbose) {
    cli::cli_alert_success("Created authenticated Vantor request ({auth$type}) for {.url {url}}.")
  }

  req
}
