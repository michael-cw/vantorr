# Convert Lat/Lon Bounding Box to Web Mercator

Helper function to convert a bounding box or spatial object from
`EPSG:4326` (lat/lon) to `EPSG:3857` (Web Mercator).

## Usage

``` r
convert_bbox_to_3857(bbox_4326)
```

## Arguments

- bbox_4326:

  Numeric vector of length 4 (`c(lon_min, lat_min, lon_max, lat_max)`)
  or any spatial object supported by
  [`vantor_as_bbox()`](https://michael-cw.github.io/vantorr/reference/vantor_as_bbox.md).

## Value

Numeric vector of length 4 with bounding box coordinates in `EPSG:3857`:
`c(x_min, y_min, x_max, y_max)`.

## Examples

``` r
# Denver area in lat/lon
bbox_latlon <- c(-105.01, 39.72, -104.96, 39.75)
bbox_mercator <- convert_bbox_to_3857(bbox_latlon)
```
