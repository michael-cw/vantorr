# Encode Coordinates as Open Location Codes (Google Plus Codes)

Converts WGS84 longitude and latitude coordinates into 10- or
11-character **Open Location Codes (Plus Codes)** (e.g.,
`"85FQPPV4+2V"`). Plus Codes are an open, offline-capable alphanumeric
street address standard supported by Google Maps, ODK Collect, and field
navigation apps without requiring an internet connection.

## Usage

``` r
vantor_plus_code(lon, lat, code_length = 10L)
```

## Arguments

- lon:

  Numeric vector of longitudes in decimal degrees (`-180` to `180`).

- lat:

  Numeric vector of latitudes in decimal degrees (`-90` to `90`).

- code_length:

  Integer. Number of significant characters in the Plus Code (`10` for
  ~14 x 14 m precision, `11` for ~3 x 3 m building-level precision).
  Default is `10L`.

## Value

A character vector of formatted Plus Codes (with a `"+"` separator after
the 8th character).

## Examples

``` r
# Encode coordinates in downtown Denver, CO
vantor_plus_code(lon = -104.995, lat = 39.740)
#> [1] "85FQP2R4+22"
```
