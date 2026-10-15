# Vantor Authentication Shiny Module Server

Server logic for the
[`vantor_auth_ui()`](https://michael-cw.github.io/vantorr/reference/vantor_auth_ui.md)
Shiny module. Manages session credentials, provides reactive
authentication state for downstream modules (such as
[`vantor_explorer_server()`](https://michael-cw.github.io/vantorr/reference/vantor_explorer_server.md)),
and automatically refreshes OAuth2 bearer tokens in the background so
long-running Shiny sessions do not expire.

## Usage

``` r
vantor_auth_server(
  id,
  auto_refresh_ms = 3e+05,
  verbose = getOption("vantorr.verbose", TRUE)
)
```

## Arguments

- id:

  Character string. The Shiny module namespace ID.

- auto_refresh_ms:

  Numeric. Interval in milliseconds to check and automatically refresh
  expiring OAuth2 tokens via
  [`shiny::invalidateLater()`](https://rdrr.io/pkg/shiny/man/invalidateLater.html).
  Default is `300000` (5 minutes).

- verbose:

  Logical. Print status messages to the R console. Default is
  `getOption("vantorr.verbose", TRUE)`.

## Value

A [`shiny::reactive`](https://rdrr.io/pkg/shiny/man/reactive.html)
expression returning a list with elements: `authenticated` (logical),
`type` (`"api_key"`, `"bearer"`, or `"none"`), `api_key` (character or
`NULL`), and `updated_at` (`POSIXct`).

## Examples

``` r
if (requireNamespace("shiny", quietly = TRUE)) {
  server <- function(input, output, session) {
    auth_state <- vantor_auth_server("auth", verbose = FALSE)
  }
}
```
