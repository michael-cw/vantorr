#' Get Maxar API Bearer Token
#'
#' This function retrieves a bearer token from the Maxar Authentication API
#' using the OAuth2 Password Flow.
#'
#' @param username Character string. The email address associated with your Maxar account
#' @param password Character string. The password for your Maxar account
#' @param verbose Logical. If TRUE, prints the full response. Default is FALSE
#'
#' @return A list containing the token response, including:
#'   - access_token: The bearer token for API access
#'   - expires_in: Token validity duration in seconds
#'   - refresh_token: Token for refreshing access without re-authentication
#'   - token_type: Type of token (Bearer)
#'   - Additional response fields
#'
#' @examples
#' \dontrun{
#' # Get a token
#' token_response <- get_maxar_token("your_email@example.com", "your_password")
#'
#' # Extract the bearer token
#' bearer_token <- token_response$access_token
#'
#' # Use the token in subsequent API calls
#' # Authorization: Bearer <token>
#' }
#'
#' @export
get_maxar_token <- function(username, password, verbose = FALSE) {

  # Validate inputs
  if (missing(username) || missing(password)) {
    cli::cli_abort("Username and password are required")
  }

  # API endpoint
  token_url <- "https://account.maxar.com/auth/realms/mds/protocol/openid-connect/token"

  # Create the request
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
    httr2::req_error(is_error = function(resp) FALSE)  # Handle errors manually

  # Perform the request
  tryCatch({
    response <- httr2::req_perform(request)
  }, error = function(e) {
    cli::cli_abort(paste("Error retrieving token:", e$message))
  })
  # Check status
  if (httr2::resp_status(response) != 200) {
    cli::cli_abort(paste("Authentication failed with status:",
                         httr2::resp_status(response),
                         "\nReason:", httr2::resp_status_desc(response)))
  }

  # Parse JSON response
  token_data <- httr2::resp_body_json(response)

  # Print success message
  cli::cli_alert_success("Successfully retrieved token.")
  cli::cli_alert_warning(paste("Token expires in:", token_data$expires_in, "seconds\n"))

  # Print full response if verbose
  if (verbose) {
    cli::cli_alert_info("Full response:")
    print(token_data)
  }

  return(token_data)

}


#' Refresh Maxar API Bearer Token
#'
#' This function refreshes a bearer token using a refresh token,
#' avoiding the need to resend username and password.
#'
#' @param refresh_token Character string. The refresh token from a previous token response
#' @param verbose Logical. If TRUE, prints the full response. Default is FALSE
#'
#' @return A list containing the new token response
#'
#' @examples
#' \dontrun{
#' # First get an initial token
#' token_response <- get_maxar_token("your_email@example.com", "your_password")
#'
#' # Later, refresh the token
#' new_token_response <- refresh_maxar_token(token_response$refresh_token)
#' }
#'
#' @export
refresh_maxar_token <- function(refresh_token, verbose = FALSE) {

  # Validate input
  if (missing(refresh_token)) {
    stop("Refresh token is required")
  }

  # API endpoint
  token_url <- "https://account.maxar.com/auth/realms/mds/protocol/openid-connect/token"

  # Create the request
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
    httr2::req_error(is_error = function(resp) FALSE)  # Handle errors manually

  # Perform the request
  tryCatch({
    response <- httr2::req_perform(request)
  }, error = function(e) {
    stop(paste("Error refreshing token:", e$message))
  })

  # Check status
  if (httr2::resp_status(response) != 200) {
    stop(paste("Token refresh failed with status:",
               httr2::resp_status(response),
               "\nReason:", httr2::resp_status_desc(response)))
  }

  # Parse JSON response
  token_data <- httr2::resp_body_json(response)

  # Print success message
  cli::cli_alert_success("Successfully refreshed token.")
  cli::cli_alert_warning(paste("Token expires in:", token_data$expires_in, "seconds\n"))

  # Print full response if verbose
  if (verbose) {
    cli::cli_alert_info("Full response:")
    print(token_data)
  }

  return(token_data)

}


#' Create Maxar API Request with Bearer Token
#'
#' Helper function to create an httr2 request with Maxar bearer token authentication
#'
#' @param url Character string. The API endpoint URL
#' @param token Character string. The bearer token (access_token from get_maxar_token)
#'
#' @return An httr2 request object with authentication header set
#'
#' @examples
#' \dontrun{
#' # Get token
#' token_response <- get_maxar_token("your_email@example.com", "your_password")
#'
#' # Create authenticated request for another API endpoint
#' req <- create_maxar_request(
#'   "https://api.maxar.com/some-endpoint",
#'   token_response$access_token
#' )
#'
#' # Perform the request
#' response <- req_perform(req)
#' }
#'
#' @export
create_maxar_request <- function(url, token) {
  httr2::request(url) |>
    httr2::req_headers(
      Authorization = paste("Bearer", token)
    )
}


# Example usage function
#' @examples
#' \dontrun{
#' # Complete workflow example
#' library(httr2)
#'
#' # 1. Get initial token
#' token_response <- get_maxar_token(
#'   username = "your_email@example.com",
#'   password = "your_password"
#' )
#'
#' # 2. Use the token for API calls
#' my_token <- token_response$access_token
#'
#' # Example: Make an authenticated API call
#' api_request <- create_maxar_request(
#'   "https://api.maxar.com/your-endpoint",
#'   my_token
#' )
#'
#' # Add any additional parameters needed
#' api_request <- api_request |>
#'   req_url_query(param1 = "value1", param2 = "value2")
#'
#' # Perform the request
#' api_response <- req_perform(api_request)
#'
#' # 3. When token expires (after ~2 hours), refresh it
#' new_token_response <- refresh_maxar_token(token_response$refresh_token)
#'
#' # Continue using the new token
#' my_token <- new_token_response$access_token
#' }
