# Get Maxar / Vantor API Bearer Token

Retrieves a bearer token from the Maxar/Vantor Authentication API using
the OAuth2 Password Flow and caches it in the current session for
automatic refresh.

## Usage

``` r
get_maxar_token(
  username = Sys.getenv("VANTOR_USERNAME", unset = Sys.getenv("MAXAR_USERNAME", unset =
    "")),
  password = Sys.getenv("VANTOR_PASSWORD", unset = Sys.getenv("MAXAR_PASSWORD", unset =
    "")),
  verbose = getOption("vantorr.verbose", TRUE)
)
```

## Arguments

- username:

  Character string. The email address associated with your Maxar/Vantor
  account. Defaults to
  `Sys.getenv("VANTOR_USERNAME", Sys.getenv("MAXAR_USERNAME"))`.

- password:

  Character string. The password for your Maxar/Vantor account. Defaults
  to `Sys.getenv("VANTOR_PASSWORD", Sys.getenv("MAXAR_PASSWORD"))`.

- verbose:

  Logical. If `TRUE`, prints token status details. Defaults to
  `getOption("vantorr.verbose", TRUE)`.

## Value

A list containing the token response (`access_token`, `expires_in`,
`refresh_token`, `token_type`), or `NULL` (with an alert) if the API
cannot be reached.

## Examples

``` r
# \donttest{
if (vantor_has_auth() && nzchar(Sys.getenv("VANTOR_USERNAME"))) {
  token_response <- get_maxar_token(verbose = FALSE)
}
# }
```
