# Check if Vantor Authentication is Available

Checks whether an active session token, API key, or environment
credentials are available. Useful for guarding examples and automated
tests.

## Usage

``` r
vantor_has_auth(verbose = FALSE)
```

## Arguments

- verbose:

  Logical. If `TRUE`, prints the detected authentication source.
  Defaults to `FALSE`.

## Value

Logical `TRUE` if authentication credentials are available, `FALSE`
otherwise.

## Examples

``` r
vantor_has_auth()
#> [1] FALSE
```
