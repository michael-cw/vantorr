# Coerce Spatial Objects to a Bounding Box in a Target CRS

S3 generic function that extracts a bounding box from spatial objects
across major R geography packages (`sf`, `terra`, `stars`) or numeric
vectors and transforms it to a target Coordinate Reference System (CRS).

## Usage

``` r
vantor_as_bbox(x, target_crs = 3857, source_crs = NULL, ...)
```

## Arguments

- x:

  A spatial object (`sf`, `sfc`, `bbox`, `SpatRaster`, `SpatVector`,
  `SpatExtent`, `stars`, `stars_proxy`) or a numeric vector of length 4:
  `c(xmin, ymin, xmax, ymax)`.

- target_crs:

  Integer or character string. Target EPSG code or CRS definition.
  Default is `3857` (Web Mercator).

- source_crs:

  Integer or character string. Source CRS used only when `x` is a plain
  numeric vector without a `"crs"` attribute. If `NULL`, heuristically
  detects `EPSG:4326` if coordinates fall within valid
  longitude/latitude bounds (`[-180, 180]` and `[-90, 90]`), or
  `EPSG:3857` otherwise.

- ...:

  Additional arguments passed to methods.

## Value

An `sf` `bbox` object (`c(xmin, ymin, xmax, ymax)`) in `target_crs`.

## Examples

``` r
# From a lat/lon numeric vector (automatically detected as EPSG:4326)
vantor_as_bbox(c(-105.01, 39.72, -104.96, 39.75), target_crs = 3857)
#>      xmin      ymin      xmax      ymax 
#> -11689660   4825336 -11684094   4829679 

# From an sf bbox object
bb <- sf::st_bbox(c(xmin = -77.04, ymin = 38.88, xmax = -77.03, ymax = 38.89),
                  crs = sf::st_crs(4326))
vantor_as_bbox(bb, target_crs = 3857)
#>     xmin     ymin     xmax     ymax 
#> -8576054  4704497 -8574940  4705927 
```
