# Get Vantor Basemap Seamlines as an `sf` Object (OGC WFS)

Queries the Vantor / Maxar Basemap Seamlines OGC Web Feature Service
(WFS) to retrieve vector polygons representing the individual satellite
imagery strips that make up a Vivid Mosaic basemap within an area of
interest.

## Usage

``` r
vantor_get_seamlines(
  bbox,
  product = "VIVID_STANDARD_30",
  cql_filter = NULL,
  token = NULL,
  api_key = NULL,
  verbose = getOption("vantorr.verbose", TRUE)
)
```

## Arguments

- bbox:

  A spatial object (`sf`, `sfc`, `bbox`, `SpatRaster`, `SpatVector`,
  `SpatExtent`, `stars`, `stars_proxy`) or a numeric vector of length 4:
  `c(xmin, ymin, xmax, ymax)`.

- product:

  Character string. Vivid mosaic product name. Default is
  `"VIVID_STANDARD_30"`. Options include `"VIVID_STANDARD_30"` and
  `"VIVID_ADVANCED_15"`.

- cql_filter:

  Character string. Optional custom CQL filter. If `NULL`, constructed
  automatically from `product` and `bbox`.

- token:

  Character string. Optional OAuth2 bearer token.

- api_key:

  Character string. Optional Maxar/Vantor API key.

- verbose:

  Logical. Print progress messages. Default is
  `getOption("vantorr.verbose", TRUE)`.

## Value

An `sf` object (in `EPSG:4326`) containing the seamline polygons and
their acquisition metadata, or `NULL` invisibly if the API request
fails.

## Details

Each returned polygon feature typically includes metadata attributes
such as:

- `acq_time`, `acq_time_earliest`, `acq_time_latest`: Imagery
  acquisition timestamp(s)

- `gsd`: Ground sample distance (resolution)

- `sensor` / `vehicle_name`: Satellite sensor identifier (e.g.,
  `"WV03"`, `"WV02"`)

- `ona`: Off-nadir angle

- `catid`: Archive catalog ID of the source strip

## Examples

``` r
# \donttest{
if (vantor_has_auth()) {
  aoi <- sf::st_bbox(
    c(xmin = -105.01, ymin = 39.73, xmax = -104.99, ymax = 39.75),
    crs = sf::st_crs(4326)
  )
  seamlines <- vantor_get_seamlines(aoi, verbose = FALSE)
}
# }
```
