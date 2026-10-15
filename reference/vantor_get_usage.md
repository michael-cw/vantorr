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
  api_key = NULL,
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

  Character string. Optional OAuth2 bearer token.

- api_key:

  Character string. Optional Maxar/Vantor API key.

- verbose:

  Logical. Print summary messages. Default is
  `getOption("vantorr.verbose", TRUE)`.

## Value

A list containing the parsed usage overview response (including
`activations`, `usageCategories`, and a tidy `summary_df` data frame
when available), or `NULL` invisibly if the API request fails.

## Examples

``` r
# \donttest{
if (vantor_has_auth()) {
  usage <- vantor_get_usage(category = "PRODUCT", unit = "CREDIT", verbose = FALSE)
}
# }
```
