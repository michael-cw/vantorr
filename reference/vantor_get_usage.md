# Get Vantor Account Usage and Credit Overview

Queries the Vantor / Maxar Usage API (`v2/activation/overview`) to
retrieve activation credit limits, total usage, and categorized
consumption metrics. Useful for monitoring account quota in interactive
R sessions or Shiny dashboards before launching large multi-tile
downloads.

## Usage

``` r
vantor_get_usage(
  category = c("PRODUCT", "DAY", "WEEK", "MONTH", "QUARTER"),
  unit = c("CREDIT", "FEATURE_COUNT", "REQUEST_COUNT", "MAP_VIEW", "GIGABYTE", "SQ_KM"),
  token = NULL,
  verbose = getOption("vantorr.verbose", TRUE)
)
```

## Arguments

- category:

  Character string. Grouping category for usage breakdown. One of
  `"PRODUCT"` (default), `"DAY"`, `"WEEK"`, `"MONTH"`, or `"QUARTER"`.

- unit:

  Character string. Measurement unit for usage metrics. One of
  `"CREDIT"` (default), `"FEATURE_COUNT"`, `"REQUEST_COUNT"`,
  `"MAP_VIEW"`, `"GIGABYTE"`, or `"SQ_KM"`.

- token:

  Character string. Optional OAuth2 bearer token (`access_token` from
  [`get_maxar_token()`](https://michael-cw.github.io/vantorr/reference/get_maxar_token.md)).
  If `NULL`, resolved from the active OAuth2 session cache or
  `VANTOR_USERNAME` / `VANTOR_PASSWORD` environment variables.

- verbose:

  Logical. Print summary messages. Default is
  `getOption("vantorr.verbose", TRUE)`.

## Value

A list containing the parsed usage overview response (including
`activations`, `usageCategories`, and a tidy `summary_df` data frame
when available), or `NULL` invisibly if the API request fails.

## Note

**Authentication Requirement (OAuth2 Bearer Token Only):** The Usage API
(`https://api.maxar.com/usage/v2/activation/overview`) is part of the
Vantor **Account Services (Admin API)**
(<https://developers.maxar.com/docs/admin/guides/usage>). Per the Vantor
API specification, **API keys (`maxar-api-key`) are not accepted by
Admin APIs**. You must authenticate using an OAuth2 Bearer Token via
`vantor_auth(username = "...", password = "...")` (or `VANTOR_USERNAME`
and `VANTOR_PASSWORD` in `.Renviron`), and your account must have Admin
permissions to view activation usage.

## Examples

``` r
# \donttest{
if (nzchar(Sys.getenv("VANTOR_USERNAME")) && nzchar(Sys.getenv("VANTOR_PASSWORD"))) {
  usage <- vantor_get_usage(category = "PRODUCT", unit = "CREDIT", verbose = FALSE)
}
# }
```
