# Refresh Maxar / Vantor API Bearer Token

Refreshes a bearer token using a refresh token, avoiding the need to
resend username and password, and updates the session token cache.

## Usage

``` r
refresh_maxar_token(
  refresh_token = .vantorr_env$refresh_token,
  verbose = getOption("vantorr.verbose", TRUE)
)
```

## Arguments

- refresh_token:

  Character string. The refresh token from a previous token response.
  Defaults to the cached session refresh token if available.

- verbose:

  Logical. If `TRUE`, prints token refresh details. Defaults to
  `getOption("vantorr.verbose", TRUE)`.

## Value

A list containing the new token response, or `NULL` if refresh fails.

## Examples

``` r
# \donttest{
if (!is.null(vantorr:::.vantorr_env$refresh_token)) {
  new_token <- refresh_maxar_token(verbose = FALSE)
}
# }
```
