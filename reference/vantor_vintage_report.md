# Audit Basemap Imagery Vintage and Sensor Quality Across Survey Areas

Evaluates the acquisition dates (`acq_time`), ground sample distance
(`gsd`), off-nadir angle (`ona`), and contributing satellite sensors
across an Area of Interest or a set of Enumeration Area / survey grid
polygons. Can either query the Vantor OGC WFS Seamlines endpoint via
[`vantor_get_seamlines()`](https://michael-cw.github.io/vantorr/reference/vantor_get_seamlines.md)
or use a pre-fetched `seamlines` `sf` object for offline workflows.

## Usage

``` r
vantor_vintage_report(
  aoi,
  seamlines = NULL,
  product = "VIVID_STANDARD_30",
  max_age_days = 730,
  reference_date = Sys.Date(),
  token = NULL,
  api_key = NULL,
  verbose = getOption("vantorr.verbose", TRUE)
)
```

## Arguments

- aoi:

  An `sf` object (such as Enumeration Areas or the output of
  [`vantor_survey_grid()`](https://michael-cw.github.io/vantorr/reference/vantor_survey_grid.md)),
  or any spatial bounding box supported by
  [`vantor_as_bbox()`](https://michael-cw.github.io/vantorr/reference/vantor_as_bbox.md).

- seamlines:

  Optional `sf` object of seamline polygons previously fetched via
  [`vantor_get_seamlines()`](https://michael-cw.github.io/vantorr/reference/vantor_get_seamlines.md).
  If `NULL`, fetched automatically from the Vantor WFS API.

- product:

  Character string. Vivid product name when fetching seamlines. Default
  is `"VIVID_STANDARD_30"`.

- max_age_days:

  Numeric. Threshold in days beyond which imagery is flagged with
  `vintage_warning = TRUE`. Default is `730` (2 years).

- reference_date:

  `Date` or character string (`"YYYY-MM-DD"`) used to calculate
  `age_days`. Defaults to
  [`Sys.Date()`](https://rdrr.io/r/base/Sys.time.html).

- token:

  Character string. Optional OAuth2 bearer token.

- api_key:

  Character string. Optional Vantor/Maxar API key.

- verbose:

  Logical. Print summary messages. Default is
  `getOption("vantorr.verbose", TRUE)`.

## Value

An `sf` object representing `aoi` enriched with vintage audit columns:

- `n_strips`: Number of intersecting satellite mosaic strips

- `earliest_acq_date`, `latest_acq_date`: Earliest and latest strip
  acquisition dates (`YYYY-MM-DD`)

- `max_age_days`: Age in days of the oldest intersecting imagery strip
  relative to `reference_date`

- `min_gsd_m`, `max_gsd_m`: Minimum and maximum ground sample distance
  (meters)

- `max_ona_deg`: Maximum off-nadir angle (degrees)

- `sensors`: Comma-separated list of contributing satellite sensors

- `vintage_warning`: Logical `TRUE` if `max_age_days > max_age_days`
  threshold

## Examples

``` r
# Offline example using synthetic seamlines and a survey grid
aoi <- sf::st_bbox(
  c(xmin = -105.01, ymin = 39.73, xmax = -105.00, ymax = 39.74),
  crs = sf::st_crs(4326)
)
grid_sf <- vantor_survey_grid(aoi, cell_size = 600, verbose = FALSE)

mock_seamline <- sf::st_sf(
  acq_time = "2024-05-10T18:00:00Z",
  gsd = 0.30,
  ona = 11.5,
  sensor = "WV03",
  catid = "1040010012345600",
  geometry = sf::st_as_sfc(aoi)
)

audit_sf <- vantor_vintage_report(
  aoi = grid_sf,
  seamlines = mock_seamline,
  verbose = FALSE
)
```
