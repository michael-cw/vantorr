#' Get Maxar / Vantor WMS Basemap as GeoTIFF or Spatial Raster Object
#'
#' Retrieves a Maxar/Vantor Vivid Mosaic basemap (30 cm or 15 cm) via OGC WMS
#' for a specified area of interest. Supports spatial inputs from `sf`, `terra`,
#' and `stars`, as well as numeric bounding boxes, and can return a file path,
#' `terra::SpatRaster`, or `stars` object.
#'
#' @param bbox A spatial object (`sf`, `sfc`, `bbox`, `SpatRaster`, `SpatVector`,
#'   `SpatExtent`, `stars`, `stars_proxy`) or a numeric vector of length 4:
#'   `c(xmin, ymin, xmax, ymax)`. Automatically transformed to the target `crs`.
#' @param token Character string. Optional OAuth2 bearer token from `get_maxar_token()`.
#'   If `NULL`, resolved via `api_key`, session cache (`vantor_auth()`), or
#'   environment variables.
#' @param output_file Character string. Path where the GeoTIFF file will be saved.
#'   Defaults to `tempfile(pattern = "vantor_basemap_", fileext = ".tif")` in
#'   compliance with CRAN policies.
#' @param width Integer. Width of the output image in pixels. If `NULL`, calculated
#'   from `resolution`.
#' @param height Integer. Height of the output image in pixels. If `NULL`, calculated
#'   from `resolution`.
#' @param resolution Numeric. Target ground resolution in meters. Default is `0.3` (30 cm).
#'   Used to calculate `width` and `height` if not explicitly provided.
#' @param product Character string. Vivid product name. Default is `"VIVID_STANDARD_30"`.
#'   Options include `"VIVID_STANDARD_30"` and `"VIVID_ADVANCED_15"`.
#' @param crs Character string. Output CRS requested from WMS (`"EPSG:3857"` or
#'   `"EPSG:4326"`). Default is `"EPSG:3857"`.
#' @param cql_filter Character string. Optional custom CQL filter. If `NULL`,
#'   constructed automatically from `product` and `shoreline_masking`.
#' @param shoreline_masking Logical. Apply shoreline masking. Default is `FALSE`.
#' @param api_key Character string. Optional Maxar/Vantor API key (`maxar-api-key`).
#' @param return_type Character string. Return format: `"file"` (default, returns
#'   the file path invisibly), `"SpatRaster"` (returns a `terra::SpatRaster`),
#'   `"stars"` (returns an in-memory `stars` object), or `"stars_proxy"`.
#' @param verbose Logical. Print progress messages. Default is
#'   `getOption("vantorr.verbose", TRUE)`.
#'
#' @return Depending on `return_type`, the path to the saved GeoTIFF file
#'   (invisibly when `return_type = "file"`), a `SpatRaster`, or a `stars` object.
#'   Returns `NULL` invisibly (with an informative message) if the API request fails.
#'
#' @examples
#' \donttest{
#' if (vantor_has_auth()) {
#'   # Define area of interest using an sf bounding box (WGS84)
#'   aoi <- sf::st_bbox(
#'     c(xmin = -105.005, ymin = 39.735, xmax = -105.000, ymax = 39.740),
#'     crs = sf::st_crs(4326)
#'   )
#'
#'   # Retrieve basemap to a temporary GeoTIFF file
#'   tif_file <- get_maxar_wms_basemap(
#'     bbox = aoi,
#'     resolution = 0.5,
#'     verbose = FALSE
#'   )
#' }
#' }
#'
#' @export
get_maxar_wms_basemap <- function(bbox,
                                  token = NULL,
                                  output_file = tempfile(pattern = "vantor_basemap_", fileext = ".tif"),
                                  width = NULL,
                                  height = NULL,
                                  resolution = 0.3,
                                  product = "VIVID_STANDARD_30",
                                  crs = c("EPSG:3857", "EPSG:4326"),
                                  cql_filter = NULL,
                                  shoreline_masking = FALSE,
                                  api_key = NULL,
                                  return_type = c("file", "SpatRaster", "stars", "stars_proxy"),
                                  verbose = getOption("vantorr.verbose", TRUE)) {

  if (missing(bbox) || is.null(bbox)) {
    cli::cli_abort("{.arg bbox} is required.")
  }

  crs <- match.arg(crs)
  return_type <- match.arg(return_type)

  # Resolve authentication (supports explicit token, api_key, or session/env vars)
  auth <- .resolve_auth(token = token, api_key = api_key, verbose = verbose)

  # Compute metric extent in EPSG:3857 for accurate pixel dimension calculations
  bbox_3857 <- as.numeric(vantor_as_bbox(bbox, target_crs = 3857))

  if (is.null(width) || is.null(height)) {
    if (!is.numeric(resolution) || length(resolution) != 1L || resolution <= 0) {
      cli::cli_abort("{.arg resolution} must be a single positive number in meters.")
    }

    x_extent <- abs(bbox_3857[3] - bbox_3857[1])
    y_extent <- abs(bbox_3857[4] - bbox_3857[2])

    width <- max(1L, as.integer(round(x_extent / resolution)))
    height <- max(1L, as.integer(round(y_extent / resolution)))

    if (verbose) {
      cli::cli_alert_info(
        "Calculated image dimensions: {width} x {height} pixels ({round(x_extent, 1)} x {round(y_extent, 1)} m at {resolution} m resolution)."
      )
    }

    max_dim <- 8192L
    if (width > max_dim || height > max_dim) {
      scale_factor <- min(max_dim / width, max_dim / height)
      width <- max(1L, as.integer(round(width * scale_factor)))
      height <- max(1L, as.integer(round(height * scale_factor)))

      if (verbose) {
        cli::cli_alert_warning(
          "Dimensions capped at: {width} x {height} pixels (max: {max_dim}). Use {.fn get_maxar_wms_tiles} for larger areas."
        )
      }
    }
  }

  # Format bbox parameter according to WMS 1.3.0 axis ordering rules
  if (identical(crs, "EPSG:4326")) {
    bbox_4326 <- as.numeric(vantor_as_bbox(bbox, target_crs = 4326))
    # WMS 1.3.0 EPSG:4326 requires [ymin, xmin, ymax, xmax] (lat, lon)
    wms_bbox_str <- paste(bbox_4326[c(2, 1, 4, 3)], collapse = ",")
  } else {
    wms_bbox_str <- paste(bbox_3857, collapse = ",")
  }

  wms_url <- "https://api.maxar.com/basemaps/v1/ogc/wms"

  if (is.null(cql_filter)) {
    cql_filter <- sprintf("productName='%s'", product)
    if (isTRUE(shoreline_masking)) {
      cql_filter <- paste0(cql_filter, " OR productName='BASE_MOSAIC'")
    }
  }

  request <- create_maxar_request(
    url = wms_url,
    token = if (identical(auth$type, "bearer")) auth$value else NULL,
    api_key = if (identical(auth$type, "api_key")) auth$value else NULL
  ) |>
    httr2::req_url_query(
      service = "WMS",
      request = "GetMap",
      version = "1.3.0",
      bbox = wms_bbox_str,
      crs = crs,
      layers = "Maxar:Imagery",
      width = width,
      height = height,
      format = "image/geotiff",
      cql_filter = cql_filter,
      shoreline_masking = tolower(as.character(shoreline_masking))
    ) |>
    httr2::req_error(is_error = function(resp) FALSE)

  if (verbose) {
    cli::cli_alert_info("Requesting WMS basemap ({product}, {crs})...")
  }

  response <- tryCatch(
    httr2::req_perform(request),
    error = function(e) {
      cli::cli_alert_danger("Error connecting to Vantor WMS endpoint: {conditionMessage(e)}")
      NULL
    }
  )

  if (is.null(response)) {
    return(invisible(NULL))
  }

  if (httr2::resp_status(response) != 200L) {
    error_msg <- tryCatch(
      httr2::resp_body_string(response),
      error = function(e) httr2::resp_status_desc(response)
    )
    cli::cli_alert_danger(
      "WMS request failed with status {httr2::resp_status(response)}: {substr(error_msg, 1, 300)}"
    )
    return(invisible(NULL))
  }

  content_type <- httr2::resp_header(response, "Content-Type")
  if (is.null(content_type) || !grepl("image/(geo)?tiff", content_type, ignore.case = TRUE)) {
    error_body <- tryCatch(httr2::resp_body_string(response), error = function(e) "")
    cli::cli_alert_danger(
      "Unexpected WMS response Content-Type ({content_type}): {substr(error_body, 1, 300)}"
    )
    return(invisible(NULL))
  }

  out_dir <- dirname(output_file)
  if (!dir.exists(out_dir)) {
    dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
  }

  writeBin(httr2::resp_body_raw(response), output_file)

  if (verbose) {
    file_size <- file.size(output_file) / 1024 / 1024
    cli::cli_alert_success("GeoTIFF saved: {.file {output_file}} ({round(file_size, 2)} MB)")
  }

  .format_raster_return(output_file, return_type = return_type, invisible_file = TRUE)
}


#' Convert Lat/Lon Bounding Box to Web Mercator
#'
#' Helper function to convert a bounding box or spatial object from `EPSG:4326`
#' (lat/lon) to `EPSG:3857` (Web Mercator).
#'
#' @param bbox_4326 Numeric vector of length 4 (`c(lon_min, lat_min, lon_max, lat_max)`)
#'   or any spatial object supported by [vantor_as_bbox()].
#'
#' @return Numeric vector of length 4 with bounding box coordinates in `EPSG:3857`:
#'   `c(x_min, y_min, x_max, y_max)`.
#'
#' @examples
#' # Denver area in lat/lon
#' bbox_latlon <- c(-105.01, 39.72, -104.96, 39.75)
#' bbox_mercator <- convert_bbox_to_3857(bbox_latlon)
#'
#' @export
convert_bbox_to_3857 <- function(bbox_4326) {
  if (is.numeric(bbox_4326)) {
    if (length(bbox_4326) != 4L) {
      cli::cli_abort("{.arg bbox_4326} must be a vector of length 4: {.code c(lon_min, lat_min, lon_max, lat_max)}.")
    }
    bb <- vantor_as_bbox(bbox_4326, target_crs = 3857, source_crs = 4326)
  } else {
    bb <- vantor_as_bbox(bbox_4326, target_crs = 3857)
  }
  as.numeric(bb)
}


#' Get Multiple WMS Basemap Tiles for Large Areas
#'
#' Splits a large area of interest into a regular grid of tiles, downloads each
#' tile via [get_maxar_wms_basemap()], and optionally merges them into a single
#' GeoTIFF, `terra::SpatRaster`, or `stars` object.
#'
#' @param bbox A spatial object (`sf`, `sfc`, `bbox`, `SpatRaster`, `SpatVector`,
#'   `SpatExtent`, `stars`, `stars_proxy`) or a numeric vector of length 4.
#' @param token Character string. Optional OAuth2 bearer token.
#' @param api_key Character string. Optional Maxar/Vantor API key.
#' @param tile_size Numeric. Size of each tile side in meters. Default is `2500` (2.5 km).
#' @param output_dir Character string. Directory for individual tile files.
#'   Defaults to a new directory inside [tempdir()] in compliance with CRAN policy.
#' @param merge_tiles Logical. Merge downloaded tiles into a single mosaic.
#'   Default is `TRUE` (requires the `terra` package).
#' @param final_output Character string. File path for the merged GeoTIFF.
#'   Defaults to `tempfile(pattern = "vantor_merged_", fileext = ".tif")`.
#' @param return_type Character string. One of `"file"`, `"SpatRaster"`,
#'   `"stars"`, or `"stars_proxy"`.
#' @param verbose Logical. Print progress messages. Default is
#'   `getOption("vantorr.verbose", TRUE)`.
#' @param ... Additional arguments passed to [get_maxar_wms_basemap()].
#'
#' @return If `merge_tiles = TRUE`, returns the merged output formatted according
#'   to `return_type`. If `merge_tiles = FALSE`, returns a character vector of
#'   downloaded tile file paths.
#'
#' @examples
#' \donttest{
#' if (vantor_has_auth()) {
#'   large_aoi <- sf::st_bbox(
#'     c(xmin = -105.02, ymin = 39.72, xmax = -104.98, ymax = 39.75),
#'     crs = sf::st_crs(4326)
#'   )
#'   merged_path <- get_maxar_wms_tiles(
#'     bbox = large_aoi,
#'     tile_size = 2000,
#'     resolution = 1.0,
#'     verbose = FALSE
#'   )
#' }
#' }
#'
#' @export
get_maxar_wms_tiles <- function(bbox,
                                token = NULL,
                                api_key = NULL,
                                tile_size = 2500,
                                output_dir = tempfile(pattern = "vantor_tiles_"),
                                merge_tiles = TRUE,
                                final_output = tempfile(pattern = "vantor_merged_", fileext = ".tif"),
                                return_type = c("file", "SpatRaster", "stars", "stars_proxy"),
                                verbose = getOption("vantorr.verbose", TRUE),
                                ...) {

  return_type <- match.arg(return_type)
  bbox_3857 <- as.numeric(vantor_as_bbox(bbox, target_crs = 3857))

  if (!dir.exists(output_dir)) {
    dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)
  }

  x_range <- seq(bbox_3857[1], bbox_3857[3], by = tile_size)
  y_range <- seq(bbox_3857[2], bbox_3857[4], by = tile_size)

  if ( utils::tail(x_range, 1) < bbox_3857[3] ) x_range <- c(x_range, bbox_3857[3])
  if ( utils::tail(y_range, 1) < bbox_3857[4] ) y_range <- c(y_range, bbox_3857[4])

  tiles <- expand.grid(
    x_idx = seq_len(length(x_range) - 1L),
    y_idx = seq_len(length(y_range) - 1L)
  )

  n_tiles <- nrow(tiles)
  if (verbose) {
    cli::cli_alert_info("Downloading {n_tiles} tile{?s} to {.file {output_dir}}...")
  }

  tile_files <- character(n_tiles)

  for (i in seq_len(n_tiles)) {
    xi <- tiles$x_idx[i]
    yi <- tiles$y_idx[i]
    tile_bbox <- sf::st_bbox(
      c(xmin = x_range[xi], ymin = y_range[yi],
        xmax = x_range[xi + 1L], ymax = y_range[yi + 1L]),
      crs = sf::st_crs(3857)
    )

    tile_file <- file.path(output_dir, sprintf("tile_%03d.tif", i))

    if (verbose) {
      cli::cli_alert_info("Downloading tile {i}/{n_tiles}...")
    }

    res_file <- get_maxar_wms_basemap(
      bbox = tile_bbox,
      token = token,
      api_key = api_key,
      output_file = tile_file,
      return_type = "file",
      verbose = FALSE,
      ...
    )

    if (is.null(res_file)) {
      cli::cli_alert_danger("Tile {i}/{n_tiles} failed to download; aborting tile merge.")
      return(invisible(NULL))
    }

    tile_files[i] <- tile_file
  }

  if (isTRUE(merge_tiles) && length(tile_files) > 1L) {
    if (!requireNamespace("terra", quietly = TRUE)) {
      cli::cli_abort("Package {.pkg terra} is required when {.code merge_tiles = TRUE}.")
    }

    if (verbose) {
      cli::cli_alert_info("Merging {length(tile_files)} tiles...")
    }

    r_coll <- terra::sprc(lapply(tile_files, terra::rast))
    merged <- terra::merge(r_coll)
    terra::writeRaster(merged, final_output, overwrite = TRUE)

    if (verbose) {
      cli::cli_alert_success("Merged GeoTIFF saved: {.file {final_output}}")
    }

    return(.format_raster_return(final_output, return_type = return_type, invisible_file = TRUE))
  } else if (isTRUE(merge_tiles) && length(tile_files) == 1L) {
    file.copy(tile_files[1], final_output, overwrite = TRUE)
    if (verbose) {
      cli::cli_alert_success("Single tile GeoTIFF saved: {.file {final_output}}")
    }
    return(.format_raster_return(final_output, return_type = return_type, invisible_file = TRUE))
  }

  if (verbose) {
    cli::cli_alert_success("Downloaded {length(tile_files)} tile{?s} to {.file {output_dir}}.")
  }

  invisible(tile_files)
}
