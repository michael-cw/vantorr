# Construct a Vantor OGC WMTS Tile URL Template

Generates an OGC Web Map Tile Service (WMTS) URL template
(`{z}/{x}/{y}`) for streaming Vantor / Maxar Vivid Mosaic basemap tiles
in `EPSG:3857` (Web Mercator) within `leaflet`, `mapgl`, or `tmap`.

## Usage

``` r
vantor_wmts_url(
  product = "VIVID_STANDARD_30",
  api_key = NULL,
  format = "image/jpeg",
  cql_filter = NULL,
  verbose = FALSE
)
```

## Arguments

- product:

  Character string. Vivid product name. Default is
  `"VIVID_STANDARD_30"`. Options include `"VIVID_STANDARD_30"` and
  `"VIVID_ADVANCED_15"`.

- api_key:

  Character string. Optional Vantor/Maxar API key. If `NULL`, resolved
  from
  [`vantor_auth()`](https://michael-cw.github.io/vantorr/reference/vantor_auth.md)
  or environment variables (`VANTOR_API_KEY` / `MAXAR_API_KEY`).

- format:

  Character string. Tile image format. Default is `"image/jpeg"`.
  Options include `"image/jpeg"`, `"image/png"`, and
  `"image/vnd.jpeg-png"`.

- cql_filter:

  Character string. Optional custom CQL filter. If `NULL`, constructed
  automatically from `product`.

- verbose:

  Logical. Print confirmation message. Default is `FALSE`.

## Value

A character string containing the WMTS `GetTile` URL template with
`{z}`, `{x}`, and `{y}` placeholders.

## Examples

``` r
url_template <- vantor_wmts_url(
  product = "VIVID_STANDARD_30",
  api_key = "example_api_key"
)
```
