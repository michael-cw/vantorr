# Vantor Authentication Shiny Module UI

Creates a compact, namespace-safe Shiny UI panel for configuring Vantor
/ Maxar API credentials (API Key or OAuth2 Username/Password),
displaying connection status, and optionally checking account credit
usage.

## Usage

``` r
vantor_auth_ui(id, show_usage = TRUE)
```

## Arguments

- id:

  Character string. The Shiny module namespace ID.

- show_usage:

  Logical. Include a button and output area to query account credit
  usage via
  [`vantor_get_usage()`](https://michael-cw.github.io/vantorr/reference/vantor_get_usage.md).
  Default is `TRUE`.

## Value

A `shiny.tag.list` UI definition.

## Examples

``` r
if (requireNamespace("shiny", quietly = TRUE)) {
  ui <- shiny::fluidPage(
    vantor_auth_ui("auth")
  )
}
```
