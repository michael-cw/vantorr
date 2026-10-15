# Clear Stored Vantor Credentials

Removes any cached API key or OAuth2 tokens from the internal session
environment.

## Usage

``` r
vantor_auth_clear(verbose = getOption("vantorr.verbose", TRUE))
```

## Arguments

- verbose:

  Logical. Print confirmation message. Defaults to
  `getOption("vantorr.verbose", TRUE)`.

## Value

Invisibly returns `TRUE`.

## Examples

``` r
vantor_auth_clear(verbose = FALSE)
```
