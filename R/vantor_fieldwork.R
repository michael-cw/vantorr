#' Generate an Alphanumeric Survey Index Grid over an Area of Interest
#'
#' Partitions an Area of Interest (AOI) into a regular grid of square or
#' hexagonal enumeration cells with human-readable alphanumeric index labels
#' (`"A1"`, `"A2"`, `"B1"`, etc.), cell centroids (`centroid_lon`,
#' `centroid_lat`), and area in square meters (`area_m2`). Designed for
#' planning field enumeration areas, printing map book index sheets with
#' [vantor_field_atlas()], and stratified spatial sampling.
#'
#' @param aoi A spatial object (`sf`, `sfc`, `bbox`, `SpatRaster`, `SpatVector`,
#'   `SpatExtent`, `stars`, `stars_proxy`) or a numeric bounding box vector of
#'   length 4 (`c(xmin, ymin, xmax, ymax)` in `EPSG:4326`).
#' @param cell_size Numeric. Target width/diameter of each grid cell in meters
#'   (computed in Web Mercator `EPSG:3857`). Default is `500` (500 m).
#' @param shape Character string. Grid cell geometry: `"square"` (default) or
#'   `"hexagonal"`.
#' @param clip_to_aoi Logical. If `TRUE` (default) and `aoi` is an `sf`, `sfc`,
#'   or `SpatVector` polygon, retains only grid cells that spatially intersect
#'   the `aoi` geometry.
#' @param crs Numeric or character. Output Coordinate Reference System for the
#'   returned `sf` grid. Default is `4326` (`EPSG:4326`).
#' @param verbose Logical. Print status messages. Default is
#'   `getOption("vantorr.verbose", TRUE)`.
#'
#' @return An `sf` polygon object with columns:
#'   - `cell_id`: Alphanumeric grid label (`"A1"`, `"A2"`, ..., `"B1"`, ...)
#'   - `row_idx`: Row index (top-to-bottom, 1-based)
#'   - `col_idx`: Column index (left-to-right, 1-based)
#'   - `centroid_lon`, `centroid_lat`: Cell centroid coordinates in `EPSG:4326`
#'   - `area_m2`: Approximate planar area in square meters
#'   - `geometry`: Polygon geometry in `crs`
#'
#' @examples
#' aoi <- sf::st_bbox(
#'   c(xmin = -105.01, ymin = 39.73, xmax = -104.99, ymax = 39.75),
#'   crs = sf::st_crs(4326)
#' )
#' grid_sf <- vantor_survey_grid(aoi, cell_size = 500, verbose = FALSE)
#' head(grid_sf)
#'
#' @export
vantor_survey_grid <- function(aoi,
                               cell_size = 500,
                               shape = c("square", "hexagonal"),
                               clip_to_aoi = TRUE,
                               crs = 4326,
                               verbose = getOption("vantorr.verbose", TRUE)) {

  if (missing(aoi) || is.null(aoi)) {
    cli::cli_abort("{.arg aoi} is required.")
  }
  if (!is.numeric(cell_size) || length(cell_size) != 1L || cell_size <= 0) {
    cli::cli_abort("{.arg cell_size} must be a single positive number in meters.")
  }

  shape <- match.arg(shape)
  is_square <- identical(shape, "square")

  # Convert AOI to EPSG:3857 for metric grid construction
  bbox_3857 <- vantor_as_bbox(aoi, target_crs = 3857)
  poly_3857 <- sf::st_as_sfc(bbox_3857)

  grid_sfc <- sf::st_make_grid(
    poly_3857,
    cellsize = cell_size,
    square = is_square
  )

  if (length(grid_sfc) == 0L) {
    cli::cli_abort("Could not generate grid cells for the supplied {.arg aoi} and {.arg cell_size}.")
  }

  # Compute centroids in EPSG:3857 to assign row (top-to-bottom) and col (left-to-right) indices
  cents_3857 <- sf::st_coordinates(sf::st_centroid(grid_sfc))
  # Round to nearest tenth of a meter to group rows and columns cleanly
  unique_y <- sort(unique(round(cents_3857[, "Y"], 1)), decreasing = TRUE)
  unique_x <- sort(unique(round(cents_3857[, "X"], 1)), decreasing = FALSE)

  row_idx <- match(round(cents_3857[, "Y"], 1), unique_y)
  col_idx <- match(round(cents_3857[, "X"], 1), unique_x)

  row_labels <- .index_to_excel_letters(row_idx)
  cell_id <- paste0(row_labels, col_idx)

  grid_sf_3857 <- sf::st_sf(
    cell_id = cell_id,
    row_idx = as.integer(row_idx),
    col_idx = as.integer(col_idx),
    geometry = grid_sfc
  )

  # Order rows top-to-bottom, left-to-right
  ord <- order(grid_sf_3857$row_idx, grid_sf_3857$col_idx)
  grid_sf_3857 <- grid_sf_3857[ord, ]
  rownames(grid_sf_3857) <- NULL

  # Optionally filter to cells intersecting the actual AOI polygon geometry
  if (isTRUE(clip_to_aoi)) {
    aoi_geom <- .extract_aoi_sfc_3857(aoi)
    if (!is.null(aoi_geom)) {
      hits <- lengths(sf::st_intersects(grid_sf_3857, aoi_geom)) > 0L
      grid_sf_3857 <- grid_sf_3857[hits, ]
    }
  }

  # Calculate planar area in m2 and WGS84 centroids
  grid_sf_3857$area_m2 <- as.numeric(sf::st_area(grid_sf_3857))
  cents_4326 <- sf::st_coordinates(
    sf::st_transform(sf::st_centroid(sf::st_geometry(grid_sf_3857)), 4326)
  )
  grid_sf_3857$centroid_lon <- round(cents_4326[, "X"], 6)
  grid_sf_3857$centroid_lat <- round(cents_4326[, "Y"], 6)

  out_sf <- sf::st_transform(grid_sf_3857, sf::st_crs(crs))
  # Reorder columns cleanly
  out_sf <- out_sf[, c("cell_id", "row_idx", "col_idx", "centroid_lon", "centroid_lat", "area_m2", "geometry")]

  if (verbose) {
    cli::cli_alert_success(
      "Generated {nrow(out_sf)} {shape} survey grid cell{?s} ({cell_size} m) labeled {.val {out_sf$cell_id[1]}} to {.val {out_sf$cell_id[nrow(out_sf)]}}."
    )
  }

  out_sf
}

# Internal helper: convert 1-based integer index to spreadsheet letters (A..Z, AA..ZZ)
.index_to_excel_letters <- function(idx) {
  vapply(idx, function(n) {
    letters_out <- character(0)
    while (n > 0L) {
      rem <- (n - 1L) %% 26L
      letters_out <- c(LETTERS[rem + 1L], letters_out)
      n <- (n - 1L) %/% 26L
    }
    paste0(letters_out, collapse = "")
  }, character(1))
}

# Internal helper: extract sfc polygon in EPSG:3857 if aoi is an sf/sfc/SpatVector object
.extract_aoi_sfc_3857 <- function(aoi) {
  if (inherits(aoi, c("sf", "sfc"))) {
    geom <- sf::st_geometry(aoi)
    if (is.na(sf::st_crs(geom))) return(NULL)
    return(sf::st_transform(geom, 3857))
  }
  if (inherits(aoi, "SpatVector") && requireNamespace("terra", quietly = TRUE)) {
    sf_obj <- sf::st_as_sf(aoi)
    if (is.na(sf::st_crs(sf_obj))) return(NULL)
    return(sf::st_transform(sf::st_geometry(sf_obj), 3857))
  }
  NULL
}


#' Audit Basemap Imagery Vintage and Sensor Quality Across Survey Areas
#'
#' Evaluates the acquisition dates (`acq_time`), ground sample distance (`gsd`),
#' off-nadir angle (`ona`), and contributing satellite sensors across an Area
#' of Interest or a set of Enumeration Area / survey grid polygons. Can either
#' query the Vantor OGC WFS Seamlines endpoint via [vantor_get_seamlines()] or
#' use a pre-fetched `seamlines` `sf` object for offline workflows.
#'
#' @param aoi An `sf` object (such as Enumeration Areas or the output of
#'   [vantor_survey_grid()]), or any spatial bounding box supported by
#'   [vantor_as_bbox()].
#' @param seamlines Optional `sf` object of seamline polygons previously fetched
#'   via [vantor_get_seamlines()]. If `NULL`, fetched automatically from the
#'   Vantor WFS API.
#' @param product Character string. Vivid product name when fetching seamlines.
#'   Default is `"VIVID_STANDARD_30"`.
#' @param max_age_days Numeric. Threshold in days beyond which imagery is
#'   flagged with `vintage_warning = TRUE`. Default is `730` (2 years).
#' @param reference_date `Date` or character string (`"YYYY-MM-DD"`) used to
#'   calculate `age_days`. Defaults to `Sys.Date()`.
#' @param token Character string. Optional OAuth2 bearer token.
#' @param api_key Character string. Optional Vantor/Maxar API key.
#' @param verbose Logical. Print summary messages. Default is
#'   `getOption("vantorr.verbose", TRUE)`.
#'
#' @return An `sf` object representing `aoi` enriched with vintage audit columns:
#'   - `n_strips`: Number of intersecting satellite mosaic strips
#'   - `earliest_acq_date`, `latest_acq_date`: Earliest and latest strip acquisition dates (`YYYY-MM-DD`)
#'   - `max_age_days`: Age in days of the oldest intersecting imagery strip relative to `reference_date`
#'   - `min_gsd_m`, `max_gsd_m`: Minimum and maximum ground sample distance (meters)
#'   - `max_ona_deg`: Maximum off-nadir angle (degrees)
#'   - `sensors`: Comma-separated list of contributing satellite sensors
#'   - `vintage_warning`: Logical `TRUE` if `max_age_days > max_age_days` threshold
#'
#' @examples
#' # Offline example using synthetic seamlines and a survey grid
#' aoi <- sf::st_bbox(
#'   c(xmin = -105.01, ymin = 39.73, xmax = -105.00, ymax = 39.74),
#'   crs = sf::st_crs(4326)
#' )
#' grid_sf <- vantor_survey_grid(aoi, cell_size = 600, verbose = FALSE)
#'
#' mock_seamline <- sf::st_sf(
#'   acq_time = "2024-05-10T18:00:00Z",
#'   gsd = 0.30,
#'   ona = 11.5,
#'   sensor = "WV03",
#'   catid = "1040010012345600",
#'   geometry = sf::st_as_sfc(aoi)
#' )
#'
#' audit_sf <- vantor_vintage_report(
#'   aoi = grid_sf,
#'   seamlines = mock_seamline,
#'   verbose = FALSE
#' )
#'
#' @export
vantor_vintage_report <- function(aoi,
                                  seamlines = NULL,
                                  product = "VIVID_STANDARD_30",
                                  max_age_days = 730,
                                  reference_date = Sys.Date(),
                                  token = NULL,
                                  api_key = NULL,
                                  verbose = getOption("vantorr.verbose", TRUE)) {

  if (missing(aoi) || is.null(aoi)) {
    cli::cli_abort("{.arg aoi} is required.")
  }

  ref_date <- as.Date(reference_date)

  # Coerce AOI to an sf object in EPSG:4326
  if (inherits(aoi, "sf")) {
    aoi_sf <- sf::st_transform(aoi, 4326)
  } else if (inherits(aoi, "sfc")) {
    aoi_sf <- sf::st_sf(id = seq_along(aoi), geometry = sf::st_transform(aoi, 4326))
  } else {
    bb_4326 <- vantor_as_bbox(aoi, target_crs = 4326)
    aoi_sf <- sf::st_sf(id = 1L, geometry = sf::st_as_sfc(bb_4326))
  }

  if (is.null(seamlines)) {
    seamlines <- vantor_get_seamlines(
      bbox = aoi_sf,
      product = product,
      token = token,
      api_key = api_key,
      verbose = verbose
    )
    if (is.null(seamlines)) {
      return(invisible(NULL))
    }
  } else {
    if (!inherits(seamlines, "sf")) {
      cli::cli_abort("{.arg seamlines} must be an {.cls sf} object.")
    }
    seamlines <- sf::st_transform(seamlines, 4326)
  }

  # Suppress s2 spherical geometry warnings for planar attribute summaries
  hits_list <- suppressMessages(sf::st_intersects(aoi_sf, seamlines))
  sl_df <- sf::st_drop_geometry(seamlines)

  # Extract standard seamline columns safely
  acq_raw <- .extract_first_matching_col(sl_df, c("acq_time", "acq_time_earliest", "acq_time_latest", "acquisitionDate"))
  acq_dates <- as.Date(substr(as.character(acq_raw), 1, 10))
  gsd_vals <- suppressWarnings(as.numeric(.extract_first_matching_col(sl_df, c("gsd", "resolution"))))
  ona_vals <- suppressWarnings(as.numeric(.extract_first_matching_col(sl_df, c("ona", "offNadirAngle"))))
  sensor_vals <- as.character(.extract_first_matching_col(sl_df, c("sensor", "vehicle_name")))

  n_feat <- nrow(aoi_sf)
  n_strips <- integer(n_feat)
  earliest_acq <- rep(NA_character_, n_feat)
  latest_acq <- rep(NA_character_, n_feat)
  age_days_out <- rep(NA_real_, n_feat)
  min_gsd <- rep(NA_real_, n_feat)
  max_gsd <- rep(NA_real_, n_feat)
  max_ona <- rep(NA_real_, n_feat)
  sensors_out <- rep(NA_character_, n_feat)
  warn_out <- rep(FALSE, n_feat)

  for (i in seq_len(n_feat)) {
    idx <- hits_list[[i]]
    n_strips[i] <- length(idx)
    if (length(idx) > 0L) {
      d_sub <- acq_dates[idx]
      d_valid <- d_sub[!is.na(d_sub)]
      if (length(d_valid) > 0L) {
        min_d <- min(d_valid)
        max_d <- max(d_valid)
        earliest_acq[i] <- format(min_d, "%Y-%m-%d")
        latest_acq[i] <- format(max_d, "%Y-%m-%d")
        age_days_out[i] <- as.numeric(ref_date - min_d)
        warn_out[i] <- !is.na(age_days_out[i]) && (age_days_out[i] > max_age_days)
      }
      g_sub <- gsd_vals[idx]
      if (any(!is.na(g_sub))) {
        min_gsd[i] <- min(g_sub, na.rm = TRUE)
        max_gsd[i] <- max(g_sub, na.rm = TRUE)
      }
      o_sub <- ona_vals[idx]
      if (any(!is.na(o_sub))) {
        max_ona[i] <- max(o_sub, na.rm = TRUE)
      }
      s_sub <- unique(sensor_vals[idx])
      s_sub <- s_sub[!is.na(s_sub) & nzchar(s_sub)]
      if (length(s_sub) > 0L) {
        sensors_out[i] <- paste(s_sub, collapse = ", ")
      }
    }
  }

  aoi_sf$n_strips <- n_strips
  aoi_sf$earliest_acq_date <- earliest_acq
  aoi_sf$latest_acq_date <- latest_acq
  aoi_sf$max_age_days <- age_days_out
  aoi_sf$min_gsd_m <- min_gsd
  aoi_sf$max_gsd_m <- max_gsd
  aoi_sf$max_ona_deg <- max_ona
  aoi_sf$sensors <- sensors_out
  aoi_sf$vintage_warning <- warn_out

  if (verbose) {
    n_flagged <- sum(warn_out, na.rm = TRUE)
    cli::cli_alert_success(
      "Completed vintage audit across {n_feat} area{?s}: {n_flagged} area{?s} flagged as older than {max_age_days} days."
    )
  }

  aoi_sf
}

.extract_first_matching_col <- function(df, candidates) {
  for (col in candidates) {
    if (col %in% names(df)) return(df[[col]])
  }
  rep(NA, nrow(df))
}


#' Encode Coordinates as Open Location Codes (Google Plus Codes)
#'
#' Converts WGS84 longitude and latitude coordinates into 10- or 11-character
#' **Open Location Codes (Plus Codes)** (e.g., `"85FQPPV4+2V"`). Plus Codes are
#' an open, offline-capable alphanumeric street address standard supported by
#' Google Maps, ODK Collect, and field navigation apps without requiring an
#' internet connection.
#'
#' @param lon Numeric vector of longitudes in decimal degrees (`-180` to `180`).
#' @param lat Numeric vector of latitudes in decimal degrees (`-90` to `90`).
#' @param code_length Integer. Number of significant characters in the Plus Code
#'   (`10` for ~14 x 14 m precision, `11` for ~3 x 3 m building-level precision).
#'   Default is `10L`.
#'
#' @return A character vector of formatted Plus Codes (with a `"+"` separator
#'   after the 8th character).
#'
#' @examples
#' # Encode coordinates in downtown Denver, CO
#' vantor_plus_code(lon = -104.995, lat = 39.740)
#'
#' @export
vantor_plus_code <- function(lon, lat, code_length = 10L) {
  if (length(lon) != length(lat)) {
    cli::cli_abort("{.arg lon} and {.arg lat} must have the same length.")
  }
  if (!code_length %in% c(8L, 10L, 11L)) {
    cli::cli_abort("{.arg code_length} must be 8, 10, or 11.")
  }

  alphabet <- strsplit("23456789CFGHJMPQRVWX", "")[[1]]

  vapply(seq_along(lon), function(i) {
    x <- as.numeric(lon[i])
    y <- as.numeric(lat[i])
    if (is.na(x) || is.na(y)) return(NA_character_)

    # Clip latitude and normalize longitude per OLC specification
    y <- min(max(y, -90), 90)
    if (y == 90) y <- y - 1e-6
    while (x < -180) x <- x + 360
    while (x >= 180) x <- x - 360

    adj_lat <- y + 90
    adj_lon <- x + 180

    # Work in integer micro-units (20^3 * 5 = 40000 per degree) to avoid floating-point drift
    lat_val <- floor(adj_lat * 8000 * 5 + 1e-6)
    lon_val <- floor(adj_lon * 8000 * 4 + 1e-6)

    # Extra grid refinement step if code_length == 11
    grid_char <- ""
    if (code_length == 11L) {
      lat_digit <- lat_val %% 5L
      lon_digit <- lon_val %% 4L
      grid_idx <- lat_digit * 4L + lon_digit
      grid_char <- alphabet[grid_idx + 1L]
    }

    lat_p <- floor(adj_lat * 8000 + 1e-6)
    lon_p <- floor(adj_lon * 8000 + 1e-6)

    lat_digits <- integer(5)
    lon_digits <- integer(5)
    for (k in 5:1) {
      lat_digits[k] <- (lat_p %% 20L) + 1L
      lon_digits[k] <- (lon_p %% 20L) + 1L
      lat_p <- lat_p %/% 20L
      lon_p <- lon_p %/% 20L
    }

    n_pairs <- min(code_length, 10L) %/% 2L
    chars <- character(n_pairs * 2L)
    for (p in seq_len(n_pairs)) {
      chars[2L * p - 1L] <- alphabet[lat_digits[p]]
      chars[2L * p] <- alphabet[lon_digits[p]]
    }

    code_10 <- paste0(
      paste0(chars[1:8], collapse = ""),
      "+",
      if (n_pairs == 5L) paste0(chars[9:10], collapse = "") else ""
    )

    paste0(code_10, grid_char)
  }, character(1))
}


#' Prepare and Export Survey Sample Points for CAPI Fieldwork Apps
#'
#' Enriches a set of survey sample locations (`sf` points or polygons, such as
#' household structures, enumeration clusters, or ground-truth plots) with
#' navigation and preload fields ready for **ODK Collect**, **KoboToolbox**,
#' **ArcGIS Survey123**, **World Bank Survey Solutions**, and **QField**:
#' - `sample_id`: Unique formatted sample identifier
#' - `longitude`, `latitude`: WGS84 decimal degree coordinates
#' - `odk_geopoint`: Preload string (`"<lat> <lon> 0 0"`) used by ODK / Kobo `geopoint` questions
#' - `plus_code`: Offline-capable 10-character Google Plus Code via [vantor_plus_code()]
#' - `google_maps_url`: One-click navigation link for mobile enumerators
#' - Optional grid cell assignment (`cell_id`) if a `grid` from [vantor_survey_grid()] is supplied
#' - Optional imagery vintage metadata (`acq_date`, `sensor`, `gsd_m`) if `seamlines` are supplied
#'
#' Optionally writes the enriched dataset to a CSV preload table (`output_csv`)
#' and/or a GeoJSON file (`output_geojson`).
#'
#' @param samples An `sf` object (`POINT` or `POLYGON`; polygons are converted
#'   to representative points via `sf::st_point_on_surface()`).
#' @param id_col Character string. Optional existing column name in `samples`
#'   to use as `sample_id`. If `NULL`, generates sequential IDs (`"S001"`, `"S002"`, ...).
#' @param grid Optional `sf` grid from [vantor_survey_grid()] used to assign each
#'   sample to its Enumeration Area / grid `cell_id`.
#' @param seamlines Optional `sf` seamline polygons from [vantor_get_seamlines()]
#'   used to attach imagery vintage (`imagery_acq_date`, `imagery_sensor`) to
#'   each sample point.
#' @param output_csv Optional file path to write a CSV preload table (for ODK
#'   Collect, KoboToolbox, and ArcGIS Survey123). Default is `NULL`.
#' @param output_geojson Optional file path to write a GeoJSON file for mobile
#'   GIS map overlays (QField, Mergin Maps). Default is `NULL`.
#' @param output_suso_tab Optional file path to write a tab-delimited (`.tab`)
#'   assignment preload file formatted for **World Bank Survey Solutions**
#'   Headquarters (`Upload Assignments`). Includes composite GPS columns
#'   (`<suso_gps_var>__Latitude` and `<suso_gps_var>__Longitude`) so sampled
#'   structures appear as interactive pins on the Interviewer tablet Map
#'   Dashboard over the `.tpk` basemap, plus `_target_area` (from `cell_id`),
#'   `plus_code`, and `imagery_acq_date`. Default is `NULL`.
#' @param suso_gps_var Optional character string. Variable name of the
#'   identifying GPS question in Survey Solutions Designer (e.g., `"hh_gps"`).
#'   When provided (or when `output_suso_tab` is set), the coordinate columns on
#'   both the returned `sf` object and the exported `.tab` file are named
#'   `<suso_gps_var>__Latitude` and `<suso_gps_var>__Longitude` instead of
#'   `latitude` and `longitude`. Default is `NULL` (uses `"hh_gps"` automatically
#'   when `output_suso_tab` is supplied).
#' @param verbose Logical. Print progress messages. Default is
#'   `getOption("vantorr.verbose", TRUE)`.
#'
#' @return An enriched `sf` point object in `EPSG:4326`.
#'
#' @examples
#' pts <- sf::st_as_sf(
#'   data.frame(
#'     hh_id = c("HH_01", "HH_02"),
#'     lon = c(-105.005, -105.002),
#'     lat = c(39.735, 39.738)
#'   ),
#'   coords = c("lon", "lat"),
#'   crs = 4326
#' )
#'
#' capi_sf <- vantor_export_capi(pts, id_col = "hh_id", suso_gps_var = "hh_gps", verbose = FALSE)
#' head(capi_sf)
#'
#' @export
vantor_export_capi <- function(samples,
                               id_col = NULL,
                               grid = NULL,
                               seamlines = NULL,
                               output_csv = NULL,
                               output_geojson = NULL,
                               output_suso_tab = NULL,
                               suso_gps_var = NULL,
                               verbose = getOption("vantorr.verbose", TRUE)) {

  if (missing(samples) || is.null(samples) || !inherits(samples, "sf")) {
    cli::cli_abort("{.arg samples} must be an {.cls sf} object.")
  }

  pts_4326 <- sf::st_transform(samples, 4326)
  geom_types <- as.character(sf::st_geometry_type(pts_4326))
  if (any(!geom_types %in% c("POINT", "MULTIPOINT"))) {
    pts_4326 <- suppressWarnings(sf::st_point_on_surface(pts_4326))
  }

  coords <- sf::st_coordinates(pts_4326)
  lon <- round(coords[, "X"], 6)
  lat <- round(coords[, "Y"], 6)

  if (!is.null(id_col) && id_col %in% names(pts_4326)) {
    sample_id <- as.character(pts_4326[[id_col]])
  } else if ("sample_id" %in% names(pts_4326)) {
    sample_id <- as.character(pts_4326[["sample_id"]])
  } else {
    sample_id <- sprintf("S%03d", seq_len(nrow(pts_4326)))
  }

  use_suso_gps <- (!is.null(suso_gps_var) && nzchar(suso_gps_var)) ||
    (!is.null(output_suso_tab) && nzchar(output_suso_tab))
  gps_prefix <- if (!is.null(suso_gps_var) && nzchar(suso_gps_var)) suso_gps_var else "hh_gps"

  pts_4326$sample_id <- sample_id
  if (use_suso_gps) {
    pts_4326[["longitude"]] <- NULL
    pts_4326[["latitude"]] <- NULL
    pts_4326[[paste0(gps_prefix, "__Latitude")]] <- lat
    pts_4326[[paste0(gps_prefix, "__Longitude")]] <- lon
  } else {
    pts_4326[["longitude"]] <- lon
    pts_4326[["latitude"]] <- lat
  }
  pts_4326$odk_geopoint <- sprintf("%.6f %.6f 0 0", lat, lon)
  pts_4326$plus_code <- vantor_plus_code(lon = lon, lat = lat, code_length = 10L)
  pts_4326$google_maps_url <- sprintf("https://www.google.com/maps/search/?api=1&query=%.6f,%.6f", lat, lon)

  # Optional spatial join with survey grid cell_id
  if (!is.null(grid) && inherits(grid, "sf")) {
    grid_4326 <- sf::st_transform(grid, 4326)
    hits <- suppressMessages(sf::st_intersects(pts_4326, grid_4326))
    cell_col <- if ("cell_id" %in% names(grid_4326)) "cell_id" else names(grid_4326)[1]
    pts_4326$cell_id <- vapply(hits, function(idx) {
      if (length(idx) == 0L) NA_character_ else as.character(grid_4326[[cell_col]][idx[1]])
    }, character(1))
  }

  # Optional spatial join with seamline vintage metadata
  if (!is.null(seamlines) && inherits(seamlines, "sf")) {
    sl_4326 <- sf::st_transform(seamlines, 4326)
    hits <- suppressMessages(sf::st_intersects(pts_4326, sl_4326))
    sl_df <- sf::st_drop_geometry(sl_4326)
    acq_col <- .extract_first_matching_col(sl_df, c("acq_time", "acq_time_latest", "acquisitionDate"))
    sen_col <- .extract_first_matching_col(sl_df, c("sensor", "vehicle_name"))

    pts_4326$imagery_acq_date <- vapply(hits, function(idx) {
      if (length(idx) == 0L) NA_character_ else substr(as.character(acq_col[idx[1]]), 1, 10)
    }, character(1))
    pts_4326$imagery_sensor <- vapply(hits, function(idx) {
      if (length(idx) == 0L) NA_character_ else as.character(sen_col[idx[1]])
    }, character(1))
  }

  if (!is.null(output_csv) && nzchar(output_csv)) {
    dir.create(dirname(output_csv), recursive = TRUE, showWarnings = FALSE)
    utils::write.csv(sf::st_drop_geometry(pts_4326), file = output_csv, row.names = FALSE)
    if (verbose) {
      cli::cli_alert_success("Saved CAPI preload CSV ({nrow(pts_4326)} row{?s}): {.file {output_csv}}")
    }
  }

  if (!is.null(output_geojson) && nzchar(output_geojson)) {
    dir.create(dirname(output_geojson), recursive = TRUE, showWarnings = FALSE)
    if (file.exists(output_geojson)) unlink(output_geojson)
    sf::st_write(pts_4326, dsn = output_geojson, driver = "GeoJSON", quiet = TRUE)
    if (verbose) {
      cli::cli_alert_success("Saved CAPI GeoJSON overlay ({nrow(pts_4326)} feature{?s}): {.file {output_geojson}}")
    }
  }

  if (!is.null(output_suso_tab) && nzchar(output_suso_tab)) {
    dir.create(dirname(output_suso_tab), recursive = TRUE, showWarnings = FALSE)
    suso_df <- .format_suso_preload_df(pts_4326, suso_gps_var = gps_prefix)
    utils::write.table(
      suso_df,
      file = output_suso_tab,
      sep = "\t",
      row.names = FALSE,
      quote = FALSE,
      na = ""
    )
    if (verbose) {
      cli::cli_alert_success("Saved Survey Solutions preload TAB ({nrow(suso_df)} row{?s}): {.file {output_suso_tab}}")
    }
  }

  if (verbose && is.null(output_csv) && is.null(output_geojson) && is.null(output_suso_tab)) {
    cli::cli_alert_success("Prepared {nrow(pts_4326)} CAPI sample feature{?s} with Plus Codes and ODK geopoints.")
  }

  pts_4326
}

#' Format an enriched CAPI sf object as a Survey Solutions .tab preload data.frame
#' @noRd
.format_suso_preload_df <- function(pts_4326, suso_gps_var = "hh_gps") {
  gps_prefix <- if (!is.null(suso_gps_var) && nzchar(suso_gps_var)) suso_gps_var else "hh_gps"
  lat_col_name <- paste0(gps_prefix, "__Latitude")
  lon_col_name <- paste0(gps_prefix, "__Longitude")

  base_df <- as.data.frame(sf::st_drop_geometry(pts_4326), stringsAsFactors = FALSE, check.names = FALSE)

  if (inherits(pts_4326, "sf")) {
    coords <- sf::st_coordinates(sf::st_transform(pts_4326, 4326))
    lon_vals <- round(coords[, "X"], 6)
    lat_vals <- round(coords[, "Y"], 6)
  } else {
    lat_vals <- if (lat_col_name %in% names(base_df)) base_df[[lat_col_name]] else base_df[["latitude"]]
    lon_vals <- if (lon_col_name %in% names(base_df)) base_df[[lon_col_name]] else base_df[["longitude"]]
  }

  drop_cols <- unique(c(
    "longitude", "latitude", "lon", "lat",
    "odk_geopoint", "google_maps_url", "_quantity",
    lat_col_name, lon_col_name,
    grep("__(Latitude|Longitude)$", names(base_df), value = TRUE)
  ))
  keep_cols <- setdiff(names(base_df), drop_cols)
  suso_df <- base_df[, keep_cols, drop = FALSE]

  suso_df[[lat_col_name]] <- lat_vals
  suso_df[[lon_col_name]] <- lon_vals

  if ("cell_id" %in% names(base_df) && !"_target_area" %in% names(suso_df)) {
    suso_df[["_target_area"]] <- base_df[["cell_id"]]
  }

  suso_df
}


#' Generate a Printable Multi-Page Field Atlas (PDF Map Book)
#'
#' Creates a multi-page printable PDF Field Atlas (Map Book) for field
#' supervisors and survey enumerators:
#' - **Page 1 (Overview Index Map)**: Displays the full Area of Interest,
#'   all Enumeration Area / survey grid boundaries labeled by `id_col`
#'   (e.g., `"A1"`, `"A2"`), sample points, scale bar, and north arrow.
#' - **Subsequent Pages (Detail Enumeration Sheets)**: One page per grid cell
#'   or Enumeration Area polygon, zoomed in with a metric margin (`buffer_m`),
#'   optional high-resolution Vantor RGB satellite imagery background, labeled
#'   sample points, scale bar, north arrow, and a metadata header/footer with
#'   coordinates and imagery vintage.
#'
#' Works both **offline** (using a pre-downloaded `raster` `SpatRaster` or
#' vector-only mode when `fetch_basemap = FALSE`) and **online** (fetching
#' Vantor WMS basemap imagery automatically when `fetch_basemap = TRUE`).
#'
#' @param grid An `sf` polygon object representing survey grid cells (e.g. from
#'   [vantor_survey_grid()]) or Enumeration Area boundaries.
#' @param points Optional `sf` point object of sample locations (e.g. from
#'   [vantor_export_capi()]) to plot and label on each map sheet.
#' @param raster Optional `terra::SpatRaster` or GeoTIFF file path to use as the
#'   RGB satellite imagery background.
#' @param output_file Character string. Output PDF file path. Defaults to
#'   `tempfile(pattern = "vantor_field_atlas_", fileext = ".pdf")` in compliance
#'   with CRAN policy.
#' @param id_col Character string. Column name in `grid` used to label each
#'   enumeration sheet. Default is `"cell_id"`.
#' @param point_label_col Character string. Optional column name in `points`
#'   used to label sample markers on detail sheets. Default is `"sample_id"`.
#' @param title Character string. Title printed on the overview cover map.
#'   Default is `"Vantor Field Enumeration Atlas"`.
#' @param buffer_m Numeric. Buffer distance in meters around each detail cell.
#'   Default is `50`.
#' @param fetch_basemap Logical. If `TRUE` and `raster` is `NULL`, downloads
#'   Vantor WMS satellite imagery for the atlas extent via
#'   [get_maxar_wms_basemap()]. Default is `FALSE`.
#' @param resolution Numeric. Ground resolution in meters when
#'   `fetch_basemap = TRUE`. Default is `0.5`.
#' @param product Character string. Vivid product name when
#'   `fetch_basemap = TRUE`. Default is `"VIVID_STANDARD_30"`.
#' @param paper_width Numeric. PDF page width in inches. Default is `11` (landscape).
#' @param paper_height Numeric. PDF page height in inches. Default is `8.5`.
#' @param verbose Logical. Print progress messages. Default is
#'   `getOption("vantorr.verbose", TRUE)`.
#'
#' @return Invisibly returns the file path to the generated PDF Field Atlas.
#'
#' @examples
#' aoi <- sf::st_bbox(
#'   c(xmin = -105.01, ymin = 39.73, xmax = -105.00, ymax = 39.74),
#'   crs = sf::st_crs(4326)
#' )
#' grid_sf <- vantor_survey_grid(aoi, cell_size = 600, verbose = FALSE)
#' pdf_path <- vantor_field_atlas(grid_sf, verbose = FALSE)
#' unlink(pdf_path)
#'
#' @export
vantor_field_atlas <- function(grid,
                               points = NULL,
                               raster = NULL,
                               output_file = tempfile(pattern = "vantor_field_atlas_", fileext = ".pdf"),
                               id_col = "cell_id",
                               point_label_col = "sample_id",
                               title = "Vantor Field Enumeration Atlas",
                               buffer_m = 50,
                               fetch_basemap = FALSE,
                               resolution = 0.5,
                               product = "VIVID_STANDARD_30",
                               paper_width = 11,
                               paper_height = 8.5,
                               verbose = getOption("vantorr.verbose", TRUE)) {

  if (missing(grid) || is.null(grid) || !inherits(grid, "sf")) {
    cli::cli_abort("{.arg grid} must be an {.cls sf} polygon object (e.g., from {.fn vantor_survey_grid}).")
  }

  # Work in EPSG:3857 so metric buffers and scale bars are accurate
  grid_3857 <- sf::st_transform(grid, 3857)
  grid_4326 <- sf::st_transform(grid, 4326)

  if (!id_col %in% names(grid_3857)) {
    grid_3857[[id_col]] <- sprintf("EA_%02d", seq_len(nrow(grid_3857)))
  }

  pts_3857 <- NULL
  if (!is.null(points) && inherits(points, "sf") && nrow(points) > 0L) {
    pts_3857 <- sf::st_transform(points, 3857)
  }

  # Resolve optional background SpatRaster
  rast_3857 <- NULL
  if (!is.null(raster)) {
    if (!requireNamespace("terra", quietly = TRUE)) {
      cli::cli_abort("Package {.pkg terra} is required to plot {.arg raster} backgrounds.")
    }
    r_obj <- if (is.character(raster)) terra::rast(raster) else raster
    rast_3857 <- if (!terra::same.crs(r_obj, "EPSG:3857")) {
      terra::project(r_obj, "EPSG:3857")
    } else {
      r_obj
    }
  } else if (isTRUE(fetch_basemap)) {
    if (!requireNamespace("terra", quietly = TRUE)) {
      cli::cli_abort("Package {.pkg terra} is required when {.code fetch_basemap = TRUE}.")
    }
    full_bb_3857 <- sf::st_bbox(sf::st_buffer(sf::st_union(grid_3857), dist = buffer_m))
    rast_3857 <- get_maxar_wms_basemap(
      bbox = full_bb_3857,
      resolution = resolution,
      product = product,
      crs = "EPSG:3857",
      return_type = "SpatRaster",
      verbose = verbose
    )
  }

  dir.create(dirname(output_file), recursive = TRUE, showWarnings = FALSE)

  grDevices::pdf(file = output_file, width = paper_width, height = paper_height)
  on.exit(grDevices::dev.off(), add = TRUE)

  # --- Page 1: Overview Index Sheet ---
  graphics::par(mar = c(3.5, 3.5, 3.5, 1.5))
  ov_bbox <- as.numeric(sf::st_bbox(sf::st_buffer(sf::st_union(grid_3857), dist = buffer_m)))

  plot(
    sf::st_geometry(grid_3857),
    xlim = c(ov_bbox[1], ov_bbox[3]),
    ylim = c(ov_bbox[2], ov_bbox[4]),
    col = NA,
    border = NA,
    main = paste0(title, " - Overview Index (", nrow(grid_3857), " Sheets)")
  )

  if (!is.null(rast_3857)) {
    terra::plotRGB(rast_3857, r = 1, g = 2, b = 3, stretch = "lin", add = TRUE)
  }

  plot(
    sf::st_geometry(grid_3857),
    add = TRUE,
    border = "#00E5FF",
    lwd = 2,
    col = grDevices::adjustcolor("#0054AD", alpha.f = if (is.null(rast_3857)) 0.08 else 0.0)
  )

  if (!is.null(pts_3857)) {
    plot(sf::st_geometry(pts_3857), add = TRUE, pch = 21, bg = "#FF3B30", col = "white", cex = 1.1)
  }

  cents <- sf::st_coordinates(sf::st_centroid(sf::st_geometry(grid_3857)))
  graphics::text(
    x = cents[, "X"],
    y = cents[, "Y"],
    labels = as.character(grid_3857[[id_col]]),
    font = 2,
    cex = 1.0,
    col = if (is.null(rast_3857)) "#002B5B" else "#FFFF00"
  )

  .draw_map_decorations(ov_bbox)

  # --- Pages 2..N+1: Detail Sheets per Grid Cell / EA ---
  cents_wgs84 <- sf::st_coordinates(sf::st_centroid(sf::st_geometry(grid_4326)))

  for (i in seq_len(nrow(grid_3857))) {
    cell_row <- grid_3857[i, ]
    cell_label <- as.character(cell_row[[id_col]])
    cell_buf <- sf::st_buffer(sf::st_geometry(cell_row), dist = buffer_m)
    cb <- as.numeric(sf::st_bbox(cell_buf))

    vintage_str <- ""
    if ("latest_acq_date" %in% names(cell_row) && !is.na(cell_row$latest_acq_date)) {
      vintage_str <- paste0(" | Imagery Date: ", cell_row$latest_acq_date)
    }

    sheet_title <- sprintf(
      "Sheet %s (%d of %d) | Centroid: %.5f, %.5f%s",
      cell_label, i, nrow(grid_3857),
      cents_wgs84[i, "Y"], cents_wgs84[i, "X"],
      vintage_str
    )

    plot(
      cell_buf,
      xlim = c(cb[1], cb[3]),
      ylim = c(cb[2], cb[4]),
      col = NA,
      border = NA,
      main = sheet_title
    )

    if (!is.null(rast_3857)) {
      cell_ext <- terra::ext(cb[1], cb[3], cb[2], cb[4])
      cell_crop <- tryCatch(terra::crop(rast_3857, cell_ext), error = function(e) NULL)
      if (!is.null(cell_crop)) {
        terra::plotRGB(cell_crop, r = 1, g = 2, b = 3, stretch = "lin", add = TRUE)
      }
    }

    # Plot neighboring grid cells faintly for context
    plot(sf::st_geometry(grid_3857), add = TRUE, border = "#CCCCCC", lty = 2, lwd = 1)
    # Highlight active cell boundary
    plot(sf::st_geometry(cell_row), add = TRUE, border = "#FFCC00", lwd = 3)

    if (!is.null(pts_3857)) {
      in_view <- lengths(sf::st_intersects(pts_3857, cell_buf)) > 0L
      pts_sub <- pts_3857[in_view, ]
      if (nrow(pts_sub) > 0L) {
        plot(sf::st_geometry(pts_sub), add = TRUE, pch = 21, bg = "#FF3B30", col = "white", cex = 1.4)
        if (!is.null(point_label_col) && point_label_col %in% names(pts_sub)) {
          pt_xy <- sf::st_coordinates(pts_sub)
          graphics::text(
            x = pt_xy[, "X"],
            y = pt_xy[, "Y"],
            labels = as.character(pts_sub[[point_label_col]]),
            pos = 3,
            cex = 0.8,
            font = 2,
            col = if (is.null(rast_3857)) "#111111" else "#FFFFFF"
          )
        }
      }
    }

    .draw_map_decorations(cb)
    graphics::mtext(
      sprintf("Field Notes / Enumerator ID: ________________________   Date: ____/____/202__   Plus Code: %s",
              vantor_plus_code(cents_wgs84[i, "X"], cents_wgs84[i, "Y"])),
      side = 1, line = 2, cex = 0.85
    )
  }

  if (verbose) {
    cli::cli_alert_success(
      "Generated {nrow(grid_3857) + 1L}-page Field Atlas PDF: {.file {output_file}}"
    )
  }

  invisible(output_file)
}

# Internal helper to draw a metric scale bar and north arrow in EPSG:3857 plot coordinates
.draw_map_decorations <- function(bbox_3857) {
  dx <- bbox_3857[3] - bbox_3857[1]
  dy <- bbox_3857[4] - bbox_3857[2]

  # Choose a clean scale bar length (~15-25% of map width)
  target_m <- dx * 0.2
  pretty_lengths <- c(25, 50, 100, 200, 250, 500, 1000, 2000, 5000)
  bar_m <- pretty_lengths[which.min(abs(pretty_lengths - target_m))]

  x0 <- bbox_3857[1] + dx * 0.05
  y0 <- bbox_3857[2] + dy * 0.05
  x1 <- x0 + bar_m
  bar_h <- dy * 0.015

  graphics::rect(x0 - dx * 0.01, y0 - dy * 0.02, x1 + dx * 0.02, y0 + dy * 0.05,
                 col = grDevices::adjustcolor("white", alpha.f = 0.8), border = NA)
  graphics::rect(x0, y0, x0 + bar_m / 2, y0 + bar_h, col = "black", border = "black")
  graphics::rect(x0 + bar_m / 2, y0, x1, y0 + bar_h, col = "white", border = "black")
  graphics::text(x0 + bar_m / 2, y0 + bar_h * 2.2, labels = paste0(bar_m, " m"), cex = 0.75, font = 2)

  # North arrow in top-right corner
  nx <- bbox_3857[3] - dx * 0.05
  ny0 <- bbox_3857[4] - dy * 0.12
  ny1 <- bbox_3857[4] - dy * 0.04
  graphics::arrows(nx, ny0, nx, ny1, length = 0.1, lwd = 2, col = "black")
  graphics::text(nx, ny1 + dy * 0.02, labels = "N", font = 2, cex = 0.9)
}


#' Package Offline Fieldwork Vector Layers and Satellite Basemaps
#'
#' Bundles survey grid cells, sample points, seamline vintage polygons, and
#' high-resolution Vantor satellite imagery into a self-contained directory
#' ready to copy onto mobile tablets for offline use in **QField**, **Mergin
#' Maps**, **ArcGIS Field Maps**, **World Bank Survey Solutions**, or **ODK Collect**:
#' - Writes all vector layers (`grid`, `samples`, `seamlines`) into a single
#'   multi-layer OGC **GeoPackage (`.gpkg`)**.
#' - Writes a CSV preload table (`<package_name>_samples.csv`) and a
#'   **World Bank Survey Solutions** tab-delimited assignment preload file
#'   (`<package_name>_suso_preload.tab`) when `samples` is provided.
#' - Saves or downloads the satellite basemap as a Web Mercator (`EPSG:3857`)
#'   georeferenced **GeoTIFF (`.tif`)**.
#' - Optionally builds a multi-zoom Esri **Tile Package (`.tpk` or `.tpkx`)** via
#'   [vantor_write_tpk()] for zoomable offline basemaps in **Survey Solutions**
#'   (ArcGIS Maps SDK for .NET v200.x) and **ArcGIS Field Maps**.
#' - Optionally exports zipped **Esri Shapefiles (`_shapes.zip`)** for uploading
#'   vector Enumeration Area boundaries to Survey Solutions Headquarters.
#'
#' @param output_dir Character string. Target directory for the offline bundle.
#'   Defaults to `tempfile(pattern = "vantor_offline_pkg_")` in compliance with
#'   CRAN policy.
#' @param grid Optional `sf` polygon layer (e.g., from [vantor_survey_grid()]).
#' @param samples Optional `sf` point layer (e.g., from [vantor_export_capi()]).
#' @param seamlines Optional `sf` seamline vintage layer (e.g., from
#'   [vantor_get_seamlines()]).
#' @param raster Optional `terra::SpatRaster` or GeoTIFF file path to include
#'   in the offline bundle. Automatically reprojected to Web Mercator
#'   (`EPSG:3857`) for Survey Solutions / Esri compatibility.
#' @param fetch_basemap Logical. If `TRUE` and `raster` is `NULL`, downloads
#'   the Vantor WMS basemap for the extent of `grid` or `samples`. Default is
#'   `FALSE`.
#' @param create_tpk Logical or character (`"tpk"` or `"tpkx"`). If `TRUE` or
#'   `"tpk"`, generates an Esri `.tpk` (Compact Cache V1) tile package; if
#'   `"tpkx"`, generates an open-specification `.tpkx` (Compact Cache V2) tile
#'   package supported by Survey Solutions v24.09+ (ArcGIS Maps SDK v200.x).
#'   Default is `FALSE`.
#' @param tpk_zoom Integer vector. Web Mercator zoom levels (`0` to `22`) to
#'   include when `create_tpk` is enabled. Default is `15:16`.
#' @param create_shapefiles Logical. If `TRUE`, also writes `grid` and `samples`
#'   as `EPSG:4326` Esri Shapefiles bundled into `<package_name>_shapes.zip` for
#'   Survey Solutions Headquarters upload. Default is `FALSE`.
#' @param suso_gps_var Character string. Variable name of the identifying GPS
#'   question in your Survey Solutions Designer questionnaire used to format
#'   `<suso_gps_var>__Latitude` and `<suso_gps_var>__Longitude` columns in
#'   `<package_name>_suso_preload.tab`. Default is `"hh_gps"`.
#' @param resolution Numeric. Ground resolution in meters when
#'   `fetch_basemap = TRUE`. Default is `0.5`.
#' @param product Character string. Vivid product name when
#'   `fetch_basemap = TRUE`. Default is `"VIVID_STANDARD_30"`.
#' @param package_name Character string. Base filename prefix for bundled files.
#'   Default is `"vantor_fieldwork"`.
#' @param verbose Logical. Print progress messages. Default is
#'   `getOption("vantorr.verbose", TRUE)`.
#'
#' @return A list containing the paths to the generated offline bundle files:
#'   `output_dir`, `gpkg_path`, `layers`, `csv_path`, `suso_tab_path`,
#'   `raster_path`, `tpk_path`, `tpk_zip_path`, and `shapefile_zip_path`.
#'
#' @examples
#' aoi <- sf::st_bbox(
#'   c(xmin = -105.01, ymin = 39.73, xmax = -105.00, ymax = 39.74),
#'   crs = sf::st_crs(4326)
#' )
#' grid_sf <- vantor_survey_grid(aoi, cell_size = 600, verbose = FALSE)
#' bundle <- vantor_offline_package(
#'   grid = grid_sf,
#'   create_shapefiles = TRUE,
#'   verbose = FALSE
#' )
#' unlink(bundle$output_dir, recursive = TRUE)
#'
#' @export
vantor_offline_package <- function(output_dir = tempfile(pattern = "vantor_offline_pkg_"),
                                   grid = NULL,
                                   samples = NULL,
                                   seamlines = NULL,
                                   raster = NULL,
                                   fetch_basemap = FALSE,
                                   create_tpk = FALSE,
                                   tpk_zoom = 15:16,
                                   create_shapefiles = FALSE,
                                   suso_gps_var = "hh_gps",
                                   resolution = 0.5,
                                   product = "VIVID_STANDARD_30",
                                   package_name = "vantor_fieldwork",
                                   verbose = getOption("vantorr.verbose", TRUE)) {

  if (is.null(grid) && is.null(samples) && is.null(seamlines) && is.null(raster)) {
    cli::cli_abort("At least one spatial layer ({.arg grid}, {.arg samples}, {.arg seamlines}, or {.arg raster}) must be supplied.")
  }

  if (!dir.exists(output_dir)) {
    dir.create(output_dir, recursive = TRUE, showWarnings = FALSE)
  }

  gpkg_path <- file.path(output_dir, paste0(package_name, ".gpkg"))
  if (file.exists(gpkg_path)) unlink(gpkg_path)

  written_layers <- character(0)

  if (!is.null(grid) && inherits(grid, "sf")) {
    sf::st_write(sf::st_transform(grid, 4326), dsn = gpkg_path, layer = "survey_grid", quiet = TRUE, append = FALSE)
    written_layers <- c(written_layers, "survey_grid")
  }

  csv_path <- NULL
  suso_tab_path <- NULL
  capi_pts <- NULL
  if (!is.null(samples) && inherits(samples, "sf")) {
    capi_pts <- if ("plus_code" %in% names(samples)) {
      sf::st_transform(samples, 4326)
    } else {
      vantor_export_capi(samples, grid = grid, seamlines = seamlines, suso_gps_var = suso_gps_var, verbose = FALSE)
    }
    sf::st_write(capi_pts, dsn = gpkg_path, layer = "survey_samples", quiet = TRUE, append = TRUE)
    written_layers <- c(written_layers, "survey_samples")

    csv_df <- sf::st_drop_geometry(capi_pts)
    csv_path <- file.path(output_dir, paste0(package_name, "_samples.csv"))
    utils::write.csv(csv_df, file = csv_path, row.names = FALSE)

    suso_tab_path <- file.path(output_dir, paste0(package_name, "_suso_preload.tab"))
    suso_df <- .format_suso_preload_df(capi_pts, suso_gps_var = suso_gps_var)
    utils::write.table(
      suso_df,
      file = suso_tab_path,
      sep = "\t",
      row.names = FALSE,
      quote = FALSE,
      na = ""
    )
  }

  if (!is.null(seamlines) && inherits(seamlines, "sf")) {
    sf::st_write(sf::st_transform(seamlines, 4326), dsn = gpkg_path, layer = "imagery_seamlines", quiet = TRUE, append = TRUE)
    written_layers <- c(written_layers, "imagery_seamlines")
  }

  shapefile_zip_path <- NULL
  if (isTRUE(create_shapefiles) && (!is.null(grid) || !is.null(capi_pts))) {
    shp_staging <- tempfile(pattern = "vantor_shp_")
    dir.create(shp_staging, recursive = TRUE, showWarnings = FALSE)
    on.exit(unlink(shp_staging, recursive = TRUE), add = TRUE)

    if (!is.null(grid) && inherits(grid, "sf")) {
      grid_shp <- sf::st_transform(grid, 4326)
      if (!"label" %in% tolower(names(grid_shp)) && "cell_id" %in% names(grid_shp)) {
        grid_shp$label <- as.character(grid_shp$cell_id)
      }
      geom_col_g <- attr(grid_shp, "sf_column")
      non_geom_g <- setdiff(names(grid_shp), geom_col_g)
      names(grid_shp)[match(non_geom_g, names(grid_shp))] <- make.unique(substr(non_geom_g, 1L, 8L), sep = "")
      sf::st_write(
        grid_shp,
        dsn = file.path(shp_staging, paste0(package_name, "_grid.shp")),
        quiet = TRUE,
        append = FALSE
      )
    }

    if (!is.null(capi_pts) && inherits(capi_pts, "sf")) {
      pts_shp <- sf::st_transform(capi_pts, 4326)
      if (!"label" %in% tolower(names(pts_shp)) && "sample_id" %in% names(pts_shp)) {
        pts_shp$label <- as.character(pts_shp$sample_id)
      }
      geom_col <- attr(pts_shp, "sf_column")
      non_geom <- setdiff(names(pts_shp), geom_col)
      names(pts_shp)[match(non_geom, names(pts_shp))] <- make.unique(substr(non_geom, 1L, 8L), sep = "")
      sf::st_write(
        pts_shp,
        dsn = file.path(shp_staging, paste0(package_name, "_samples.shp")),
        quiet = TRUE,
        append = FALSE
      )
    }

    abs_out_dir <- normalizePath(output_dir, mustWork = TRUE)
    shapefile_zip_path <- file.path(abs_out_dir, paste0(package_name, "_shapes.zip"))
    if (file.exists(shapefile_zip_path)) unlink(shapefile_zip_path)
    shp_files <- list.files(shp_staging, full.names = FALSE)
    if (length(shp_files) > 0L) {
      old_wd <- getwd()
      tryCatch(
        {
          setwd(shp_staging)
          utils::zip(zipfile = shapefile_zip_path, files = shp_files, extras = "-q -X")
        },
        finally = setwd(old_wd)
      )
    }
  }

  raster_path <- NULL
  rast_obj <- NULL
  if (!is.null(raster)) {
    raster_path <- file.path(output_dir, paste0(package_name, "_basemap.tif"))
    if (requireNamespace("terra", quietly = TRUE)) {
      rast_obj <- if (inherits(raster, "SpatRaster")) raster else terra::rast(raster)
      # Ensure Web Mercator (EPSG:3857) for Survey Solutions / Esri compatibility
      crs_str <- terra::crs(rast_obj, describe = TRUE)$code
      if (is.na(crs_str) || !identical(as.character(crs_str), "3857")) {
        rast_obj <- terra::project(rast_obj, "EPSG:3857")
      }
      terra::writeRaster(rast_obj, raster_path, overwrite = TRUE)
    } else if (is.character(raster) && file.exists(raster)) {
      file.copy(raster, raster_path, overwrite = TRUE)
    }
  } else if (isTRUE(fetch_basemap)) {
    ref_obj <- grid %||% samples %||% seamlines
    if (!is.null(ref_obj)) {
      raster_path <- file.path(output_dir, paste0(package_name, "_basemap.tif"))
      get_maxar_wms_basemap(
        bbox = ref_obj,
        output_file = raster_path,
        resolution = resolution,
        product = product,
        return_type = "file",
        verbose = verbose
      )
      if (requireNamespace("terra", quietly = TRUE) && file.exists(raster_path)) {
        rast_obj <- terra::rast(raster_path)
      }
    }
  }

  tpk_path <- NULL
  tpk_zip_path <- NULL
  if (!identical(create_tpk, FALSE) && !is.null(create_tpk)) {
    tpk_fmt <- if (is.character(create_tpk) && tolower(create_tpk) == "tpkx") "tpkx" else "tpk"
    tpk_ext <- paste0(".", tpk_fmt)
    tpk_path <- file.path(output_dir, paste0(package_name, tpk_ext))
    ref_aoi <- grid %||% samples %||% seamlines %||% rast_obj %||% raster
    tpk_res <- vantor_write_tpk(
      x = ref_aoi,
      output_file = tpk_path,
      zoom = tpk_zoom,
      format = tpk_fmt,
      raster = rast_obj %||% raster,
      product = product,
      title = package_name,
      zip_for_suso = TRUE,
      verbose = verbose
    )
    tpk_zip_path <- attr(tpk_res, "suso_zip_path")
  }

  if (verbose) {
    cli::cli_alert_success(
      "Created offline fieldwork bundle in {.file {output_dir}} ({length(written_layers)} GeoPackage layer{?s}: {.val {written_layers}})."
    )
  }

  list(
    output_dir = output_dir,
    gpkg_path = if (length(written_layers) > 0L) gpkg_path else NULL,
    layers = written_layers,
    csv_path = csv_path,
    suso_tab_path = suso_tab_path,
    raster_path = raster_path,
    tpk_path = tpk_path,
    tpk_zip_path = tpk_zip_path,
    shapefile_zip_path = shapefile_zip_path
  )
}


#' Write an Esri Tile Package (`.tpk` or `.tpkx`) for Offline Field Basemaps
#'
#' @description
#' Generates a multi-zoom Web Mercator (`EPSG:3857`) Esri Tile Package
#' (`.tpk` Compact Cache V1 or `.tpkx` Compact Cache V2) in pure R without
#' requiring proprietary ArcGIS software or ArcGIS Online credits. Designed for
#' zoomable offline satellite basemaps in **World Bank Survey Solutions**
#' (including v24.09+ built on the **ArcGIS Maps SDK for .NET v200.x**),
#' **ArcGIS Field Maps**, and **ArcGIS Survey123**.
#'
#' Tiles can either be sliced offline from a local `terra::SpatRaster` / GeoTIFF
#' (`raster`), or fetched directly across zoom levels from the Vantor WMS/WMTS
#' imagery service (`raster = NULL`).
#'
#' @param x Optional spatial object (`sf`, `sfc`, `bbox`, `SpatRaster`,
#'   `SpatVector`, `SpatExtent`, `stars`, or numeric `c(xmin, ymin, xmax, ymax)`)
#'   defining the Area of Interest. If `NULL`, the extent of `raster` is used.
#' @param output_file Character string. Destination file path ending in `.tpk`
#'   or `.tpkx`. Defaults to `tempfile(fileext = ".tpk")` in compliance with
#'   CRAN policy.
#' @param zoom Integer vector of Web Mercator (`EPSG:3857`) zoom levels (`0` to
#'   `22`) to render into the tile pyramid. Default is `15:17`.
#' @param format Character string: `"auto"` (inferred from `output_file`
#'   extension), `"tpk"` (Compact Cache V1 with `.bundle` + `.bundlx` and XML
#'   metadata), or `"tpkx"` (Apache-2.0 open specification Compact Cache V2
#'   with `.bundle` and JSON metadata, supported by ArcGIS Maps SDK v200.x).
#' @param raster Optional `terra::SpatRaster` or file path to a GeoTIFF raster.
#'   When supplied, tiles are resampled offline from `raster`. When `NULL`,
#'   tiles are downloaded from the Vantor WMS service using active credentials
#'   ([vantor_auth()]).
#' @param product Character string. Vantor Vivid mosaic product identifier when
#'   fetching online tiles (`raster = NULL`). Default is `"VIVID_STANDARD_30"`.
#' @param max_tiles Integer. Safety cap on total tiles across all requested
#'   zoom levels to prevent accidental massive downloads. Default is `2500L`.
#' @param title Character string. Package title embedded in `iteminfo`. Default
#'   is `"Vantor Satellite Basemap"`.
#' @param zip_for_suso Logical. If `TRUE`, also creates a clean outer `.zip`
#'   archive (`<stem>_tpk.zip` or `<stem>_tpkx.zip`) using `zip -q -X -j`
#'   (stripping macOS `__MACOSX/._*` resource forks that Survey Solutions
#'   Headquarters rejects when users compress files via macOS Finder). Default
#'   is `FALSE`.
#' @param aoi Optional alias for `x` (Area of Interest).
#' @param verbose Logical. Print progress messages. Default is
#'   `getOption("vantorr.verbose", TRUE)`.
#'
#' @return Invisibly returns the normalized `output_file` path, with attributes
#'   `n_tiles`, `zoom`, `format`, and `suso_zip_path` (when
#'   `zip_for_suso = TRUE`).
#'
#' @examples
#' if (requireNamespace("terra", quietly = TRUE)) {
#'   # Create a synthetic RGB SpatRaster in EPSG:3857
#'   r <- terra::rast(
#'     xmin = -11689500, xmax = -11688500,
#'     ymin = 4826500, ymax = 4827500,
#'     nrows = 128, ncols = 128, nlyrs = 3,
#'     crs = "EPSG:3857"
#'   )
#'   terra::values(r) <- sample(50:220, terra::ncell(r) * 3, replace = TRUE)
#'
#'   # Export both .tpk (Compact Cache V1) and .tpkx (Compact Cache V2)
#'   tpk_file <- vantor_write_tpk(
#'     raster = r,
#'     output_file = tempfile(fileext = ".tpk"),
#'     zoom = 14:15,
#'     verbose = FALSE
#'   )
#'   tpkx_file <- vantor_write_tpk(
#'     raster = r,
#'     output_file = tempfile(fileext = ".tpkx"),
#'     zoom = 14:15,
#'     verbose = FALSE
#'   )
#'   unlink(c(tpk_file, tpkx_file))
#' }
#'
#' @export
vantor_write_tpk <- function(x = NULL,
                             output_file = tempfile(fileext = ".tpk"),
                             zoom = 15:17,
                             format = c("auto", "tpk", "tpkx"),
                             raster = NULL,
                             product = "VIVID_STANDARD_30",
                             max_tiles = 2500L,
                             title = "Vantor Satellite Basemap",
                             zip_for_suso = FALSE,
                             aoi = NULL,
                             verbose = getOption("vantorr.verbose", TRUE)) {
  x <- x %||% aoi
  format <- match.arg(format)
  if (format == "auto") {
    format <- if (grepl("\\.tpkx$", tolower(output_file))) "tpkx" else "tpk"
  }

  zoom <- sort(unique(as.integer(zoom)))
  if (length(zoom) == 0L || any(is.na(zoom)) || any(zoom < 0L | zoom > 22L)) {
    cli::cli_abort("{.arg zoom} must be an integer vector of Web Mercator zoom levels between 0 and 22.")
  }

  rast_3857 <- NULL
  if (!is.null(raster)) {
    if (!requireNamespace("terra", quietly = TRUE)) {
      cli::cli_abort("Package {.pkg terra} is required to build a TPK from a raster.")
    }
    rast_in <- if (inherits(raster, "SpatRaster")) raster else terra::rast(raster)
    crs_code <- terra::crs(rast_in, describe = TRUE)$code
    rast_3857 <- if (is.na(crs_code) || !identical(as.character(crs_code), "3857")) {
      terra::project(rast_in, "EPSG:3857")
    } else {
      rast_in
    }
  }

  target_obj <- x %||% rast_3857
  if (is.null(target_obj)) {
    cli::cli_abort("Either {.arg x} (AOI) or {.arg raster} must be provided.")
  }

  bbox_4326 <- vantor_as_bbox(target_obj, srs = "EPSG:4326")
  bbox_3857 <- vantor_as_bbox(target_obj, srs = "EPSG:3857")

  # Clamp latitude to Web Mercator limits
  ymin_lat <- max(-85.05112878, min(85.05112878, bbox_4326["ymin"]))
  ymax_lat <- max(-85.05112878, min(85.05112878, bbox_4326["ymax"]))
  xmin_lon <- max(-180, min(180, bbox_4326["xmin"]))
  xmax_lon <- max(-180, min(180, bbox_4326["xmax"]))

  # Enumerate tile grid across all zoom levels
  tile_grid_list <- lapply(zoom, function(z) {
    c_min <- .lon_to_tile_col(xmin_lon, z)
    c_max <- .lon_to_tile_col(xmax_lon, z)
    r_min <- .lat_to_tile_row(ymax_lat, z)
    r_max <- .lat_to_tile_row(ymin_lat, z)
    expand.grid(
      z = z,
      row = seq.int(r_min, r_max),
      col = seq.int(c_min, c_max),
      KEEP.OUT.ATTRS = FALSE,
      stringsAsFactors = FALSE
    )
  })
  tile_grid <- do.call(rbind, tile_grid_list)
  n_total <- nrow(tile_grid)

  if (n_total > max_tiles) {
    cli::cli_abort(c(
      "Requested zoom levels ({.val {zoom}}) require {n_total} tiles, exceeding {.arg max_tiles} ({max_tiles}).",
      "i" = "Reduce the maximum zoom level or increase {.arg max_tiles} if intended."
    ))
  }

  if (verbose) {
    cli::cli_alert_info(
      "Building Esri {.val {toupper(format)}} tile package ({n_total} tile{?s} across zoom level{?s} {.val {zoom}})..."
    )
  }

  # Render or download PNG raw bytes for each tile
  tile_grid$raw_bytes <- vector("list", n_total)
  for (i in seq_len(n_total)) {
    z_i <- tile_grid$z[i]
    row_i <- tile_grid$row[i]
    col_i <- tile_grid$col[i]
    tb <- .tile_bounds_3857(z_i, row_i, col_i)

    if (!is.null(rast_3857)) {
      tile_grid$raw_bytes[[i]] <- .render_raster_tile_png(rast_3857, tb)
    } else {
      tile_grid$raw_bytes[[i]] <- .fetch_vantor_tile_png(tb, product = product)
    }
  }

  # Stage directory structure and pack binary bundles
  staging_dir <- tempfile(pattern = "vantor_tpk_stage_")
  dir.create(staging_dir, recursive = TRUE, showWarnings = FALSE)
  on.exit(unlink(staging_dir, recursive = TRUE), add = TRUE)

  if (format == "tpkx") {
    .stage_tpkx_archive(
      staging_dir = staging_dir,
      tile_grid = tile_grid,
      zoom = zoom,
      bbox_4326 = bbox_4326,
      bbox_3857 = bbox_3857,
      title = title
    )
  } else {
    .stage_tpk_v1_archive(
      staging_dir = staging_dir,
      tile_grid = tile_grid,
      zoom = zoom,
      bbox_4326 = bbox_4326,
      bbox_3857 = bbox_3857,
      title = title
    )
  }

  out_dir <- dirname(output_file)
  if (!dir.exists(out_dir)) {
    dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
  }
  if (file.exists(output_file)) unlink(output_file)
  abs_output <- file.path(normalizePath(out_dir, mustWork = TRUE), basename(output_file))

  old_wd <- getwd()
  tryCatch(
    {
      setwd(staging_dir)
      rel_files <- list.files(".", recursive = TRUE, all.files = FALSE)
      utils::zip(zipfile = abs_output, files = rel_files, extras = "-0 -q -X")
    },
    finally = setwd(old_wd)
  )

  suso_zip_path <- NULL
  if (isTRUE(zip_for_suso)) {
    suso_zip_path <- sub("\\.(tpkx?)$", "_\\1.zip", abs_output, ignore.case = TRUE)
    if (identical(suso_zip_path, abs_output)) {
      suso_zip_path <- paste0(abs_output, "_tpk.zip")
    }
    if (file.exists(suso_zip_path)) unlink(suso_zip_path)
    utils::zip(zipfile = suso_zip_path, files = abs_output, extras = "-q -X -j")
  }

  if (verbose) {
    sz_kb <- round(file.info(abs_output)$size / 1024, 1)
    cli::cli_alert_success(
      "Saved Esri {.val {toupper(format)}} tile package ({n_total} tile{?s}, {sz_kb} KB) to {.file {abs_output}}."
    )
    if (!is.null(suso_zip_path)) {
      cli::cli_alert_success(
        "Created clean Survey Solutions upload archive (no __MACOSX metadata): {.file {suso_zip_path}}"
      )
    }
  }

  attr(abs_output, "n_tiles") <- n_total
  attr(abs_output, "zoom") <- zoom
  attr(abs_output, "format") <- format
  attr(abs_output, "suso_zip_path") <- suso_zip_path
  invisible(abs_output)
}


# -------------------------------------------------------------------------
# Internal Web Mercator & Esri Compact Cache V1 / V2 Helpers
# -------------------------------------------------------------------------

.WEBMERC_ORIGIN <- 20037508.342787

#' @noRd
.lon_to_tile_col <- function(lon, z) {
  n <- 2^z
  col <- floor((lon + 180) / 360 * n)
  as.integer(pmax(0, pmin(n - 1, col)))
}

#' @noRd
.lat_to_tile_row <- function(lat, z) {
  n <- 2^z
  lat_rad <- lat * pi / 180
  row <- floor((1 - log(tan(lat_rad) + 1 / cos(lat_rad)) / pi) / 2 * n)
  as.integer(pmax(0, pmin(n - 1, row)))
}

#' @noRd
.tile_bounds_3857 <- function(z, row, col) {
  span <- (2 * .WEBMERC_ORIGIN) / (2^z)
  c(
    xmin = -.WEBMERC_ORIGIN + col * span,
    ymin = .WEBMERC_ORIGIN - (row + 1) * span,
    xmax = -.WEBMERC_ORIGIN + (col + 1) * span,
    ymax = .WEBMERC_ORIGIN - row * span
  )
}

#' @noRd
.int_to_le_raw <- function(val, n_bytes) {
  val <- as.numeric(val)
  as.raw(floor(val / (256^(seq_len(n_bytes) - 1L))) %% 256)
}

#' @noRd
.render_raster_tile_png <- function(rast_3857, tb) {
  tmpl <- terra::rast(
    xmin = tb["xmin"], xmax = tb["xmax"],
    ymin = tb["ymin"], ymax = tb["ymax"],
    nrows = 256L, ncols = 256L,
    crs = "EPSG:3857"
  )
  warped <- suppressWarnings(terra::resample(rast_3857, tmpl, method = "bilinear"))
  n_lyrs <- terra::nlyr(warped)
  vals <- terra::values(warped, mat = TRUE)
  if (is.null(vals) || nrow(vals) == 0L) {
    vals <- matrix(255, nrow = 256L * 256L, ncol = 3L)
  } else if (n_lyrs == 1L) {
    vals <- cbind(vals[, 1L], vals[, 1L], vals[, 1L])
  } else {
    vals <- vals[, 1:3, drop = FALSE]
  }
  vals[is.na(vals)] <- 255
  vals[vals < 0] <- 0
  vals[vals > 255] <- 255
  vals <- vals / 255

  arr <- array(0, dim = c(256L, 256L, 3L))
  arr[, , 1L] <- matrix(vals[, 1L], nrow = 256L, ncol = 256L, byrow = TRUE)
  arr[, , 2L] <- matrix(vals[, 2L], nrow = 256L, ncol = 256L, byrow = TRUE)
  arr[, , 3L] <- matrix(vals[, 3L], nrow = 256L, ncol = 256L, byrow = TRUE)

  tmp_png <- tempfile(fileext = ".png")
  on.exit(unlink(tmp_png), add = TRUE)
  grDevices::png(filename = tmp_png, width = 256L, height = 256L, units = "px", bg = "white")
  graphics::par(mar = c(0, 0, 0, 0), xaxs = "i", yaxs = "i")
  graphics::plot.new()
  graphics::plot.window(xlim = c(0, 1), ylim = c(0, 1), asp = NA)
  graphics::rasterImage(grDevices::as.raster(arr), 0, 0, 1, 1, interpolate = FALSE)
  grDevices::dev.off()

  readBin(tmp_png, what = "raw", n = file.info(tmp_png)$size)
}

#' @noRd
.fetch_vantor_tile_png <- function(tb, product = "VIVID_STANDARD_30") {
  base_url <- "https://api.maxar.com/streaming/v1/ogc/wms"
  req <- create_maxar_request(base_url)
  cql_str <- sprintf("productName='%s'", product)
  bbox_str <- paste(tb[c("xmin", "ymin", "xmax", "ymax")], collapse = ",")

  req <- httr2::req_url_query(
    req,
    SERVICE = "WMS",
    VERSION = "1.1.1",
    REQUEST = "GetMap",
    LAYERS = "Maxar:Imagery",
    SRS = "EPSG:3857",
    BBOX = bbox_str,
    WIDTH = 256L,
    HEIGHT = 256L,
    FORMAT = "image/png",
    CQL_FILTER = cql_str
  )
  resp <- httr2::req_perform(req)
  httr2::resp_body_raw(resp)
}

#' Write Compact Cache V1 .bundle and .bundlx files
#' @noRd
.write_compact_v1_bundle <- function(sub_df, bundle_row, bundle_col, level_dir) {
  prefix <- sprintf("R%04xC%04x", bundle_row, bundle_col)
  bundle_path <- file.path(level_dir, paste0(prefix, ".bundle"))
  bundlx_path <- file.path(level_dir, paste0(prefix, ".bundlx"))

  # Every one of the 16,384 slots in column-major order (128 * local_col + local_row)
  # has a sequential 4-byte length prefix (0 if empty, or t_len followed by tile bytes)
  chunks <- replicate(16384L, raw(4L), simplify = FALSE)
  max_tile_len <- 0L

  for (i in seq_len(nrow(sub_df))) {
    raw_tile <- sub_df$raw_bytes[[i]]
    t_len <- length(raw_tile)
    if (t_len > 0L) {
      l_row <- sub_df$row[i] - bundle_row
      l_col <- sub_df$col[i] - bundle_col
      idx <- 128L * l_col + l_row + 1L
      chunks[[idx]] <- c(.int_to_le_raw(t_len, 4L), raw_tile)
      if (t_len > max_tile_len) max_tile_len <- t_len
    }
  }

  chunk_lens <- lengths(chunks)
  offsets <- 60 + c(0, utils::head(cumsum(chunk_lens), -1L))
  payload_raw <- unlist(chunks, use.names = FALSE)
  file_size <- 60 + length(payload_raw)

  # 60-byte V1 bundle header (including 16-byte user header with bundle row/col bounds)
  header_v1 <- c(
    .int_to_le_raw(3, 4L),
    .int_to_le_raw(16384, 4L),
    .int_to_le_raw(max_tile_len, 4L),
    .int_to_le_raw(5, 4L),
    .int_to_le_raw(0, 8L),
    .int_to_le_raw(file_size, 8L),
    .int_to_le_raw(40, 8L),
    .int_to_le_raw(16, 4L),
    .int_to_le_raw(bundle_row, 4L),
    .int_to_le_raw(bundle_row + 127L, 4L),
    .int_to_le_raw(bundle_col, 4L),
    .int_to_le_raw(bundle_col + 127L, 4L)
  )
  writeBin(c(header_v1, payload_raw), bundle_path)

  # 81,952-byte V1 .bundlx index file (16B header + 16384 * 5B offsets + 16B footer)
  bundlx_header <- c(
    .int_to_le_raw(3, 4L),
    .int_to_le_raw(16, 4L),
    .int_to_le_raw(16384, 4L),
    .int_to_le_raw(5, 4L)
  )
  bundlx_index <- as.vector(rbind(
    as.raw(offsets %% 256),
    as.raw(floor(offsets / 256) %% 256),
    as.raw(floor(offsets / 65536) %% 256),
    as.raw(floor(offsets / 16777216) %% 256),
    as.raw(floor(offsets / 4294967296) %% 256)
  ))
  bundlx_footer <- c(
    .int_to_le_raw(0, 4L),
    .int_to_le_raw(16, 4L),
    .int_to_le_raw(16, 4L),
    .int_to_le_raw(0, 4L)
  )
  writeBin(c(bundlx_header, bundlx_index, bundlx_footer), bundlx_path)
}

#' Write Compact Cache V2 .bundle file (Apache-2.0 Open Spec)
#' @noRd
.write_compact_v2_bundle <- function(sub_df, bundle_row, bundle_col, level_dir) {
  prefix <- sprintf("R%04xC%04x", bundle_row, bundle_col)
  bundle_path <- file.path(level_dir, paste0(prefix, ".bundle"))

  index_records <- replicate(16384L, raw(8L), simplify = FALSE)
  payload_chunks <- list()
  # Data starts after 64-byte header + 131,072-byte index table = 131,136
  curr_offset <- 131136
  max_tile_len <- 0L

  for (i in seq_len(nrow(sub_df))) {
    raw_tile <- sub_df$raw_bytes[[i]]
    t_len <- length(raw_tile)
    if (t_len > 0L) {
      l_row <- sub_df$row[i] - bundle_row
      l_col <- sub_df$col[i] - bundle_col
      # Compact Cache V2 uses row-major index: 128 * local_row + local_col
      idx <- 128L * l_row + l_col + 1L
      # TileOffset points directly to first byte of tile image (after 4B prefix)
      data_offset <- curr_offset + 4
      index_records[[idx]] <- c(.int_to_le_raw(data_offset, 5L), .int_to_le_raw(t_len, 3L))
      payload_chunks[[length(payload_chunks) + 1L]] <- c(.int_to_le_raw(t_len, 4L), raw_tile)
      curr_offset <- curr_offset + 4 + t_len
      if (t_len > max_tile_len) max_tile_len <- t_len
    }
  }

  index_raw <- unlist(index_records, use.names = FALSE)
  payload_raw <- if (length(payload_chunks) > 0L) unlist(payload_chunks, use.names = FALSE) else raw(0L)
  file_size <- 64 + length(index_raw) + length(payload_raw)

  # 64-byte Compact Cache V2 header
  header_v2 <- c(
    .int_to_le_raw(3, 4L),
    .int_to_le_raw(16384, 4L),
    .int_to_le_raw(max_tile_len, 4L),
    .int_to_le_raw(5, 4L),
    .int_to_le_raw(0, 8L),
    .int_to_le_raw(file_size, 8L),
    .int_to_le_raw(40, 8L),
    .int_to_le_raw(20 + 131072, 4L),
    .int_to_le_raw(3, 4L),
    .int_to_le_raw(16, 4L),
    .int_to_le_raw(16384, 4L),
    .int_to_le_raw(5, 4L),
    .int_to_le_raw(131072, 4L)
  )
  writeBin(c(header_v2, index_raw, payload_raw), bundle_path)
}

#' Stage a Compact Cache V1 .tpk directory tree
#' @noRd
.stage_tpk_v1_archive <- function(staging_dir, tile_grid, zoom, bbox_4326, bbox_3857, title) {
  layers_dir <- file.path(staging_dir, "v101", "Layers")
  alllayers_dir <- file.path(layers_dir, "_alllayers")
  esriinfo_dir <- file.path(staging_dir, "esriinfo")
  ms_dir <- file.path(staging_dir, "servicedescriptions", "mapserver")

  dir.create(alllayers_dir, recursive = TRUE, showWarnings = FALSE)
  dir.create(esriinfo_dir, recursive = TRUE, showWarnings = FALSE)
  dir.create(ms_dir, recursive = TRUE, showWarnings = FALSE)

  # Write bundle + bundlx per zoom level
  for (z in zoom) {
    lvl_dir <- file.path(alllayers_dir, sprintf("L%02d", z))
    dir.create(lvl_dir, recursive = TRUE, showWarnings = FALSE)
    z_df <- tile_grid[tile_grid$z == z, , drop = FALSE]
    z_df$b_row <- (z_df$row %/% 128L) * 128L
    z_df$b_col <- (z_df$col %/% 128L) * 128L
    bundles <- unique(z_df[, c("b_row", "b_col"), drop = FALSE])
    for (b in seq_len(nrow(bundles))) {
      br <- bundles$b_row[b]
      bc <- bundles$b_col[b]
      sub_df <- z_df[z_df$b_row == br & z_df$b_col == bc, , drop = FALSE]
      .write_compact_v1_bundle(sub_df, br, bc, lvl_dir)
    }
  }

  # Build LODInfo XML entries (0..22 standard Web Mercator pyramid)
  lod_xml <- vapply(0:22, function(z) {
    res <- (2 * .WEBMERC_ORIGIN) / (256 * (2^z))
    scale <- res * (96 / 0.0254)
    sprintf(
      "      <LODInfo xsi:type=\"typens:LODInfo\">\n        <LevelID>%d</LevelID>\n        <Scale>%.8f</Scale>\n        <Resolution>%.10f</Resolution>\n      </LODInfo>",
      z, scale, res
    )
  }, character(1))

  lods_json <- vapply(0:22, function(z) {
    res <- (2 * .WEBMERC_ORIGIN) / (256 * (2^z))
    scale <- res * (96 / 0.0254)
    sprintf("        { \"level\": %d, \"resolution\": %.10f, \"scale\": %.8f }", z, res, scale)
  }, character(1))

  min_lod <- min(zoom)
  max_lod <- max(zoom)
  min_scale <- ((2 * .WEBMERC_ORIGIN) / (256 * (2^min_lod))) * (96 / 0.0254)
  max_scale <- ((2 * .WEBMERC_ORIGIN) / (256 * (2^max_lod))) * (96 / 0.0254)

  wkt_3857 <- 'PROJCS[&quot;WGS_1984_Web_Mercator_Auxiliary_Sphere&quot;,GEOGCS[&quot;GCS_WGS_1984&quot;,DATUM[&quot;D_WGS_1984&quot;,SPHEROID[&quot;WGS_1984&quot;,6378137.0,298.257223563]],PRIMEM[&quot;Greenwich&quot;,0.0],UNIT[&quot;Degree&quot;,0.0174532925199433]],PROJECTION[&quot;Mercator_Auxiliary_Sphere&quot;],PARAMETER[&quot;False_Easting&quot;,0.0],PARAMETER[&quot;False_Northing&quot;,0.0],PARAMETER[&quot;Central_Meridian&quot;,0.0],PARAMETER[&quot;Standard_Parallel_1&quot;,0.0],PARAMETER[&quot;Auxiliary_Sphere_Type&quot;,0.0],UNIT[&quot;Meter&quot;,1.0],AUTHORITY[&quot;EPSG&quot;,3857]]'

  conf_xml <- paste0(
    "<?xml version=\"1.0\" encoding=\"utf-8\" ?>\n",
    "<CacheInfo xsi:type=\"typens:CacheInfo\" xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\" xmlns:xs=\"http://www.w3.org/2001/XMLSchema\" xmlns:typens=\"http://www.esri.com/schemas/ArcGIS/10.1\">\n",
    "  <TileCacheInfo xsi:type=\"typens:TileCacheInfo\">\n",
    "    <SpatialReference xsi:type=\"typens:ProjectedCoordinateSystem\">\n",
    "      <WKT>", wkt_3857, "</WKT>\n",
    "      <XOrigin>-20037700</XOrigin>\n",
    "      <YOrigin>-30241100</YOrigin>\n",
    "      <XYScale>10000</XYScale>\n",
    "      <ZOrigin>-100000</ZOrigin>\n",
    "      <ZScale>10000</ZScale>\n",
    "      <MOrigin>-100000</MOrigin>\n",
    "      <MScale>10000</MScale>\n",
    "      <XYTolerance>0.001</XYTolerance>\n",
    "      <ZTolerance>0.001</ZTolerance>\n",
    "      <MTolerance>0.001</MTolerance>\n",
    "      <HighPrecision>true</HighPrecision>\n",
    "      <WKID>102100</WKID>\n",
    "      <LatestWKID>3857</LatestWKID>\n",
    "    </SpatialReference>\n",
    "    <TileOrigin xsi:type=\"typens:PointN\">\n",
    "      <X>-20037508.342787</X>\n",
    "      <Y>20037508.342787</Y>\n",
    "    </TileOrigin>\n",
    "    <TileCols>256</TileCols>\n",
    "    <TileRows>256</TileRows>\n",
    "    <DPI>96</DPI>\n",
    "    <PreciseDPI>96</PreciseDPI>\n",
    "    <LODInfos xsi:type=\"typens:ArrayOfLODInfo\">\n",
    paste(lod_xml, collapse = "\n"), "\n",
    "    </LODInfos>\n",
    "  </TileCacheInfo>\n",
    "  <TileImageInfo xsi:type=\"typens:TileImageInfo\">\n",
    "    <CacheTileFormat>PNG</CacheTileFormat>\n",
    "    <CompressionQuality>0</CompressionQuality>\n",
    "    <Antialiasing>false</Antialiasing>\n",
    "  </TileImageInfo>\n",
    "  <CacheStorageInfo xsi:type=\"typens:CacheStorageInfo\">\n",
    "    <StorageFormat>esriMapCacheStorageModeCompact</StorageFormat>\n",
    "    <PacketSize>128</PacketSize>\n",
    "  </CacheStorageInfo>\n",
    "</CacheInfo>\n"
  )
  writeLines(conf_xml, file.path(layers_dir, "conf.xml"), useBytes = TRUE)

  conf_cdi <- sprintf(
    paste0(
      "<?xml version=\"1.0\" encoding=\"utf-8\" ?>\n",
      "<EnvelopeN xsi:type=\"typens:EnvelopeN\" xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\" xmlns:xs=\"http://www.w3.org/2001/XMLSchema\" xmlns:typens=\"http://www.esri.com/schemas/ArcGIS/10.1\">\n",
      "  <XMin>%.6f</XMin>\n",
      "  <YMin>%.6f</YMin>\n",
      "  <XMax>%.6f</XMax>\n",
      "  <YMax>%.6f</YMax>\n",
      "  <SpatialReference xsi:type=\"typens:ProjectedCoordinateSystem\">\n",
      "    <WKT>%s</WKT>\n",
      "    <XOrigin>-20037700</XOrigin>\n",
      "    <YOrigin>-30241100</YOrigin>\n",
      "    <XYScale>10000</XYScale>\n",
      "    <ZOrigin>-100000</ZOrigin>\n",
      "    <ZScale>10000</ZScale>\n",
      "    <MOrigin>-100000</MOrigin>\n",
      "    <MScale>10000</MScale>\n",
      "    <XYTolerance>0.001</XYTolerance>\n",
      "    <ZTolerance>0.001</ZTolerance>\n",
      "    <MTolerance>0.001</MTolerance>\n",
      "    <HighPrecision>true</HighPrecision>\n",
      "    <WKID>102100</WKID>\n",
      "    <LatestWKID>3857</LatestWKID>\n",
      "  </SpatialReference>\n",
      "</EnvelopeN>\n"
    ),
    bbox_3857["xmin"], bbox_3857["ymin"], bbox_3857["xmax"], bbox_3857["ymax"], wkt_3857
  )
  writeLines(conf_cdi, file.path(layers_dir, "conf.cdi"), useBytes = TRUE)

  pkinfo_xml <- paste0(
    "<?xml version=\"1.0\" encoding=\"utf-8\" ?>\n",
    "<pkinfo Culture=\"en-US\">\n",
    "  <ID>3F77AED6-0ABF-4244-AD4A-62F35FEAE480</ID>\n",
    "  <name>Layers</name>\n",
    "  <version>10.2</version>\n",
    "  <size>-1</size>\n",
    "  <created></created>\n",
    "  <type>Tile Package</type>\n",
    "  <servable>false</servable>\n",
    "  <packagelocation></packagelocation>\n",
    "  <pkinfolocation></pkinfolocation>\n",
    "</pkinfo>\n"
  )
  writeLines(pkinfo_xml, file.path(esriinfo_dir, "item.pkinfo"), useBytes = TRUE)

  iteminfo_xml <- sprintf(
    paste0(
      "<?xml version=\"1.0\" encoding=\"utf-8\" ?>\n",
      "<ESRI_ItemInformation Culture=\"en-US\">\n",
      "  <name>Layers</name>\n",
      "  <guid>3F77AED6-0ABF-4244-AD4A-62F35FEAE480</guid>\n",
      "  <catalogpath></catalogpath>\n",
      "  <snippet>Vantor offline satellite basemap for CAPI fieldwork.</snippet>\n",
      "  <description>Offline satellite basemap generated by the vantorr R package.</description>\n",
      "  <summary>Vantor offline satellite basemap for CAPI fieldwork.</summary>\n",
      "  <title>%s</title>\n",
      "  <tags>vantor,basemap,survey,fieldwork</tags>\n",
      "  <type>Tile Package</type>\n",
      "  <typekeywords>\n",
      "    <typekeyword>Data</typekeyword>\n",
      "    <typekeyword>Tile Package</typekeyword>\n",
      "    <typekeyword>ArcMap</typekeyword>\n",
      "    <typekeyword>ArcGlobe</typekeyword>\n",
      "    <typekeyword>tpk</typekeyword>\n",
      "  </typekeywords>\n",
      "  <thumbnail></thumbnail>\n",
      "  <documentation></documentation>\n",
      "  <url></url>\n",
      "  <datalastmodifiedtime></datalastmodifiedtime>\n",
      "  <extent>\n",
      "    <xmin>%.6f</xmin>\n",
      "    <ymin>%.6f</ymin>\n",
      "    <xmax>%.6f</xmax>\n",
      "    <ymax>%.6f</ymax>\n",
      "  </extent>\n",
      "  <spatialreference>WGS_1984_Web_Mercator_Auxiliary_Sphere</spatialreference>\n",
      "  <accessinformation>Vantor</accessinformation>\n",
      "  <licenseinfo></licenseinfo>\n",
      "</ESRI_ItemInformation>\n"
    ),
    title, bbox_4326["xmin"], bbox_4326["ymin"], bbox_4326["xmax"], bbox_4326["ymax"]
  )
  writeLines(iteminfo_xml, file.path(esriinfo_dir, "iteminfo.xml"), useBytes = TRUE)

  safe_title <- gsub("\"", "\\\\\"", title)
  ms_json_str <- sprintf(
    paste0(
      "{\n",
      "  \"name\": \"MapServer\",\n",
      "  \"contents\": {\n",
      "    \"currentVersion\": 10.22,\n",
      "    \"serviceDescription\": \"Offline satellite basemap generated by vantorr\",\n",
      "    \"mapName\": \"Layers\",\n",
      "    \"description\": \"%s\",\n",
      "    \"copyrightText\": \"Vantor\",\n",
      "    \"supportsDynamicLayers\": false,\n",
      "    \"layers\": [\n",
      "      {\n",
      "        \"id\": 0,\n",
      "        \"name\": \"%s\",\n",
      "        \"parentLayerId\": -1,\n",
      "        \"defaultVisibility\": true,\n",
      "        \"subLayerIds\": null,\n",
      "        \"minScale\": 0,\n",
      "        \"maxScale\": 0\n",
      "      }\n",
      "    ],\n",
      "    \"tables\": [],\n",
      "    \"spatialReference\": { \"wkid\": 102100, \"latestWkid\": 3857 },\n",
      "    \"singleFusedMapCache\": true,\n",
      "    \"tileInfo\": {\n",
      "      \"rows\": 256,\n",
      "      \"cols\": 256,\n",
      "      \"dpi\": 96,\n",
      "      \"format\": \"PNG\",\n",
      "      \"compressionQuality\": 0,\n",
      "      \"origin\": { \"x\": -20037508.342787, \"y\": 20037508.342787 },\n",
      "      \"spatialReference\": { \"wkid\": 102100, \"latestWkid\": 3857 },\n",
      "      \"lods\": [\n%s\n      ]\n",
      "    },\n",
      "    \"units\": \"esriMeters\",\n",
      "    \"supportedImageFormatTypes\": \"PNG32,PNG24,PNG,JPG,DIB,TIFF,EMF,PS,PDF,GIF,SVG,SVGZ,BMP\",\n",
      "    \"capabilities\": \"Map,Query,Data\",\n",
      "    \"supportedQueryFormats\": \"JSON, AMF\",\n",
      "    \"exportTilesAllowed\": false,\n",
      "    \"maxRecordCount\": 1000,\n",
      "    \"maxImageHeight\": 4096,\n",
      "    \"maxImageWidth\": 4096,\n",
      "    \"fullExtent\": {\n",
      "      \"xmin\": %.6f, \"ymin\": %.6f, \"xmax\": %.6f, \"ymax\": %.6f,\n",
      "      \"spatialReference\": { \"wkid\": 102100, \"latestWkid\": 3857 }\n",
      "    },\n",
      "    \"initialExtent\": {\n",
      "      \"xmin\": %.6f, \"ymin\": %.6f, \"xmax\": %.6f, \"ymax\": %.6f,\n",
      "      \"spatialReference\": { \"wkid\": 102100, \"latestWkid\": 3857 }\n",
      "    },\n",
      "    \"minScale\": %.8f,\n",
      "    \"maxScale\": %.8f\n",
      "  },\n",
      "  \"resourceInfo\": {\n",
      "    \"geoFullExtent\": {\n",
      "      \"xmin\": %.6f, \"ymin\": %.6f, \"xmax\": %.6f, \"ymax\": %.6f\n",
      "    },\n",
      "    \"geoInitialExtent\": {\n",
      "      \"xmin\": %.6f, \"ymin\": %.6f, \"xmax\": %.6f, \"ymax\": %.6f\n",
      "    },\n",
      "    \"cacheInfo\": {\n",
      "      \"storageInfo\": {\n",
      "        \"packetSize\": 128,\n",
      "        \"storageFormat\": \"compact\"\n",
      "      },\n",
      "      \"wmtsScaleFactor\": 0.00028\n",
      "    }\n",
      "  },\n",
      "  \"resources\": [\n",
      "    {\n",
      "      \"name\": \"layers\",\n",
      "      \"resources\": []\n",
      "    },\n",
      "    {\n",
      "      \"name\": \"tables\",\n",
      "      \"resources\": []\n",
      "    }\n",
      "  ]\n",
      "}\n"
    ),
    safe_title, safe_title,
    paste(lods_json, collapse = ",\n"),
    bbox_3857["xmin"], bbox_3857["ymin"], bbox_3857["xmax"], bbox_3857["ymax"],
    bbox_3857["xmin"], bbox_3857["ymin"], bbox_3857["xmax"], bbox_3857["ymax"],
    min_scale, max_scale,
    bbox_4326["xmin"], bbox_4326["ymin"], bbox_4326["xmax"], bbox_4326["ymax"],
    bbox_4326["xmin"], bbox_4326["ymin"], bbox_4326["xmax"], bbox_4326["ymax"]
  )
  writeLines(ms_json_str, file.path(ms_dir, "mapserver.json"), useBytes = TRUE)
}

#' Stage a Compact Cache V2 .tpkx directory tree (Apache-2.0 Open Spec)
#' @noRd
.stage_tpkx_archive <- function(staging_dir, tile_grid, zoom, bbox_4326, bbox_3857, title) {
  tile_root <- file.path(staging_dir, "tile")
  dir.create(tile_root, recursive = TRUE, showWarnings = FALSE)

  for (z in zoom) {
    lvl_dir <- file.path(tile_root, sprintf("L%02d", z))
    dir.create(lvl_dir, recursive = TRUE, showWarnings = FALSE)
    z_df <- tile_grid[tile_grid$z == z, , drop = FALSE]
    z_df$b_row <- (z_df$row %/% 128L) * 128L
    z_df$b_col <- (z_df$col %/% 128L) * 128L
    bundles <- unique(z_df[, c("b_row", "b_col"), drop = FALSE])
    for (b in seq_len(nrow(bundles))) {
      br <- bundles$b_row[b]
      bc <- bundles$b_col[b]
      sub_df <- z_df[z_df$b_row == br & z_df$b_col == bc, , drop = FALSE]
      .write_compact_v2_bundle(sub_df, br, bc, lvl_dir)
    }
  }

  lods_str <- vapply(0:22, function(z) {
    res <- (2 * .WEBMERC_ORIGIN) / (256 * (2^z))
    scale <- res * (96 / 0.0254)
    sprintf("      { \"level\": %d, \"resolution\": %.10f, \"scale\": %.8f }", z, res, scale)
  }, character(1))

  min_lod <- min(zoom)
  max_lod <- max(zoom)
  min_scale <- ((2 * .WEBMERC_ORIGIN) / (256 * (2^min_lod))) * (96 / 0.0254)
  max_scale <- ((2 * .WEBMERC_ORIGIN) / (256 * (2^max_lod))) * (96 / 0.0254)
  safe_title <- gsub("\"", "\\\\\"", title)

  root_json_str <- sprintf(
    paste0(
      "{\n",
      "  \"name\": \"%s\",\n",
      "  \"version\": \"1.0\",\n",
      "  \"serviceDescription\": \"Offline satellite basemap generated by vantorr\",\n",
      "  \"tileBundlesPath\": \"./tile\",\n",
      "  \"minLOD\": %d,\n",
      "  \"maxLOD\": %d,\n",
      "  \"minScale\": %.8f,\n",
      "  \"maxScale\": %.8f,\n",
      "  \"resampling\": true,\n",
      "  \"units\": \"esriMeters\",\n",
      "  \"spatialReference\": {\n",
      "    \"wkid\": 102100, \"latestWkid\": 3857,\n",
      "    \"xyTolerance\": 0.001, \"zTolerance\": 0.001, \"mTolerance\": 0.001,\n",
      "    \"falseX\": -20037700, \"falseY\": -30241100, \"xyUnits\": 10000,\n",
      "    \"falseZ\": -100000, \"zUnits\": 10000, \"falseM\": -100000, \"mUnits\": 10000\n",
      "  },\n",
      "  \"initialExtent\": {\n",
      "    \"xmin\": %.6f, \"ymin\": %.6f, \"xmax\": %.6f, \"ymax\": %.6f,\n",
      "    \"spatialReference\": { \"wkid\": 102100, \"latestWkid\": 3857 }\n",
      "  },\n",
      "  \"fullExtent\": {\n",
      "    \"xmin\": %.6f, \"ymin\": %.6f, \"xmax\": %.6f, \"ymax\": %.6f,\n",
      "    \"spatialReference\": { \"wkid\": 102100, \"latestWkid\": 3857 }\n",
      "  },\n",
      "  \"storageInfo\": {\n",
      "    \"storageFormat\": \"esriMapCacheStorageModeCompactV2\",\n",
      "    \"packetSize\": 128\n",
      "  },\n",
      "  \"tileImageInfo\": {\n",
      "    \"format\": \"PNG\",\n",
      "    \"compressionQuality\": 0\n",
      "  },\n",
      "  \"tileInfo\": {\n",
      "    \"rows\": 256,\n",
      "    \"cols\": 256,\n",
      "    \"dpi\": 96,\n",
      "    \"origin\": { \"x\": -20037508.342787, \"y\": 20037508.342787 },\n",
      "    \"spatialReference\": { \"wkid\": 102100, \"latestWkid\": 3857 },\n",
      "    \"lods\": [\n%s\n    ]\n",
      "  }\n",
      "}\n"
    ),
    safe_title, min_lod, max_lod, min_scale, max_scale,
    bbox_3857["xmin"], bbox_3857["ymin"], bbox_3857["xmax"], bbox_3857["ymax"],
    bbox_3857["xmin"], bbox_3857["ymin"], bbox_3857["xmax"], bbox_3857["ymax"],
    paste(lods_str, collapse = ",\n")
  )
  writeLines(root_json_str, file.path(staging_dir, "root.json"), useBytes = TRUE)

  iteminfo_json_str <- sprintf(
    paste0(
      "{\n",
      "  \"version\": \"1.0\",\n",
      "  \"name\": \"%s\",\n",
      "  \"title\": \"%s\",\n",
      "  \"type\": \"Tile Package\",\n",
      "  \"typeKeywords\": [\"Tile Package\", \"tpkx\", \"Vantor\"],\n",
      "  \"description\": \"Offline satellite basemap generated by the vantorr R package.\",\n",
      "  \"summary\": \"Vantor offline satellite basemap for CAPI fieldwork.\",\n",
      "  \"snippet\": \"Vantor offline satellite basemap.\",\n",
      "  \"tags\": [\"vantor\", \"basemap\", \"survey\", \"fieldwork\"],\n",
      "  \"credits\": \"Vantor\",\n",
      "  \"created\": %.0f,\n",
      "  \"spatialReference\": \"WGS_1984_Web_Mercator_Auxiliary_Sphere\",\n",
      "  \"extent\": [[%.6f, %.6f], [%.6f, %.6f]]\n",
      "}\n"
    ),
    safe_title, safe_title,
    round(as.numeric(Sys.time()) * 1000),
    bbox_4326["xmin"], bbox_4326["ymin"], bbox_4326["xmax"], bbox_4326["ymax"]
  )
  writeLines(iteminfo_json_str, file.path(staging_dir, "iteminfo.json"), useBytes = TRUE)
}

