# Create Maxar / Vantor API Request with Authentication

Helper function to create an `httr2` request with Vantor/Maxar bearer
token or API key authentication and built-in retry logic for transient
HTTP errors.

## Usage

``` r
create_maxar_request(url, token = NULL, api_key = NULL, verbose = FALSE)
```

## Arguments

- url:

  Character string. The API endpoint URL.

- token:

  Character string. Optional bearer token (`access_token` from
  [`get_maxar_token()`](https://michael-cw.github.io/vantorr/reference/get_maxar_token.md)).

- api_key:

  Character string. Optional Maxar/Vantor API key.

- verbose:

  Logical. If `TRUE`, prints confirmation of the configured request.
  Defaults to `FALSE`.

## Value

An `httr2_request` object with authentication headers and retry policies
set.

## Examples

``` r
req <- create_maxar_request(
  "https://api.maxar.com/discovery/v1",
  api_key = "example_key"
)
```
