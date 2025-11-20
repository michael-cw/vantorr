#' Get Maxar WMS Basemap as GeoTIFF
#'
#' This function retrieves a Maxar Vivid Mosaic 30 cm basemap as a GeoTIFF file
#' for a specified bounding box in EPSG:3857 (Web Mercator) projection.
#'
#' @param bbox Numeric vector of length 4 with bounding box coordinates in EPSG:3857
#'              Format: c(xmin, ymin, xmax, ymax)
#' @param token Character string. Bearer token from get_maxar_token() function
#' @param output_file Character string. Path where the GeoTIFF file will be saved.
#'                    Default is "maxar_basemap.tif"
#' @param width Integer. Width of the output image in pixels. If NULL, calculated from resolution.
#' @param height Integer. Height of the output image in pixels. If NULL, calculated from resolution.
#' @param resolution Numeric. Target resolution in meters. Default is 0.3 (30 cm).
#'                   Used to calculate width/height if not provided.
#' @param product Character string. Vivid product name. Default is "VIVID_STANDARD_30".
#'                Options: "VIVID_STANDARD_30", "VIVID_ADVANCED_15"
#' @param shoreline_masking Logical. Apply shoreline masking. Default is FALSE.
#' @param verbose Logical. Print progress messages. Default is TRUE.
#'
#' @return The path to the saved GeoTIFF file (invisibly)
#'
#' @examples
#' \dontrun{
#' # Get authentication token first
#' token_response <- get_maxar_token("your_email@example.com", "your_password")
#'
#' # Define bounding box in EPSG:3857 (Web Mercator)
#' # Example: Area around Denver, Colorado
#' bbox_3857 <- c(-11700000, 4830000, -11690000, 4840000)
#'
#' # Retrieve basemap as GeoTIFF
#' tif_file <- get_maxar_wms_basemap(
#'   bbox = bbox_3857,
#'   token = token_response$access_token,
#'   output_file = "denver_basemap.tif"
#' )
#'
#' # Load the resulting GeoTIFF
#' raster <- terra::rast(tif_file)
#' terra::plot(raster)
#' }
#'
#' @export
get_maxar_wms_basemap <- function(bbox,
                                  token,
                                  output_file = "maxar_basemap.tif",
                                  width = NULL,
                                  height = NULL,
                                  resolution = 0.3,
                                  product = "VIVID_STANDARD_30",
                                  shoreline_masking = FALSE,
                                  verbose = TRUE) {

  # Validate inputs
  if (missing(bbox) || missing(token)) {
    stop("Both bbox and token are required")
  }

  if (length(bbox) != 4) {
    stop("bbox must be a vector of length 4: c(xmin, ymin, xmax, ymax)")
  }

  # Calculate image dimensions if not provided
  if (is.null(width) || is.null(height)) {
    # Calculate extent in meters
    x_extent <- bbox[3] - bbox[1]
    y_extent <- bbox[4] - bbox[2]

    # Calculate pixels needed for target resolution
    width <- as.integer(abs(x_extent) / resolution)
    height <- as.integer(abs(y_extent) / resolution)

    if (verbose) {
      cli::cli_alert_info(sprintf("Calculated image dimensions: %d x %d pixels\n", width, height))
      cli::cli_alert_info(sprintf("For %.2f m resolution over %.1f x %.1f meters\n",
                                  resolution, x_extent, y_extent))
    }

    # Limit maximum dimensions to prevent excessive file sizes
    max_dim <- 8192
    if (width > max_dim || height > max_dim) {
      scale_factor <- min(max_dim / width, max_dim / height)
      width <- as.integer(width * scale_factor)
      height <- as.integer(height * scale_factor)

      if (verbose) {
        cli::cli_alert_info(sprintf("Dimensions limited to: %d x %d pixels (max: %d)\n",
                                    width, height, max_dim))
      }
    }
  }

  # WMS endpoint
  wms_url <- "https://api.maxar.com/basemaps/v1/ogc/wms"

  # Build CQL filter
  cql_filter <- sprintf("productName='%s'", product)

  # Add BASE_MOSAIC if shoreline masking is enabled
  if (shoreline_masking) {
    cql_filter <- paste0(cql_filter, " OR productName='BASE_MOSAIC'")
  }

  # Create the request
  request <- httr2::request(wms_url) |>
    httr2::req_headers(
      Authorization = paste("Bearer", token)
    ) |>
    httr2::req_url_query(
      service = "WMS",
      request = "GetMap",
      version = "1.3.0",
      bbox = paste(bbox, collapse = ","),
      crs = "EPSG:3857",
      layers = "Maxar:Imagery",
      width = width,
      height = height,
      format = "image/geotiff",
      cql_filter = cql_filter,
      shoreline_masking = tolower(as.character(shoreline_masking))
    ) |>
    httr2::req_error(is_error = function(resp) FALSE)  # Handle errors manually

  if (verbose) {
    cli::cli_alert_info("Requesting WMS basemap from Maxar...\n")
    cli::cli_alert_info(sprintf("Product: %s\n", product))
    cli::cli_alert_info(sprintf("Bounding box (EPSG:3857): %.2f, %.2f, %.2f, %.2f\n",
                                bbox[1], bbox[2], bbox[3], bbox[4]))
  }

  # Perform the request
  tryCatch({
    response <- httr2::req_perform(request)

    # Check status
    if (httr2::resp_status(response) != 200) {
      # Try to get error message from response
      error_msg <- tryCatch(
        httr2::resp_body_string(response),
        error = function(e) httr2::resp_status_desc(response)
      )
      cli::cli_abort(paste("WMS request failed with status:",
                           httr2::resp_status(response),
                           "\nError:", error_msg))
    }

    # Check if response is actually a GeoTIFF
    content_type <- httr2::resp_header(response, "Content-Type")
    if (!grepl("image/(geo)?tiff", content_type, ignore.case = TRUE)) {
      # Might be an error message in XML or text
      error_body <- httr2::resp_body_string(response)
      cli::cli_abort(paste("Unexpected content type:", content_type,
                           "\nResponse:", substr(error_body, 1, 500)))
    }

    # Save the GeoTIFF
    httr2::resp_body_raw(response) |>
      writeBin(output_file)

    if (verbose) {
      file_size <- file.size(output_file) / 1024 / 1024  # Convert to MB
      cli::cli_alert_info(sprintf("GeoTIFF saved successfully: %s (%.2f MB)",
                                  output_file, file_size))
    }

    return(invisible(output_file))

  }, error = function(e) {
    cli::cli_abort(paste("Error retrieving WMS basemap:", e$message))
  })
}


#' Convert Lat/Lon Bounding Box to Web Mercator
#'
#' Helper function to convert a bounding box from EPSG:4326 (lat/lon) to EPSG:3857 (Web Mercator)
#'
#' @param bbox_4326 Numeric vector of length 4 with bounding box in EPSG:4326
#'                  Format: c(lon_min, lat_min, lon_max, lat_max)
#'
#' @return Numeric vector of length 4 with bounding box in EPSG:3857
#'         Format: c(x_min, y_min, x_max, y_max)
#'
#' @examples
#' \dontrun{
#' # Denver area in lat/lon
#' bbox_latlon <- c(-105.01, 39.72, -104.96, 39.75)
#'
#' # Convert to Web Mercator
#' bbox_mercator <- convert_bbox_to_3857(bbox_latlon)
#' }
#'
#' @export
convert_bbox_to_3857 <- function(bbox_4326) {
  if (length(bbox_4326) != 4) {
    stop("bbox_4326 must be a vector of length 4: c(lon_min, lat_min, lon_max, lat_max)")
  }

  # Create sf bbox object
  bbox_sf <- sf::st_bbox(c(xmin = bbox_4326[1],
                       ymin = bbox_4326[2],
                       xmax = bbox_4326[3],
                       ymax = bbox_4326[4]),
                     crs = sf::st_crs(4326))

  # Convert to polygon then transform to EPSG:3857
  bbox_poly <- sf::st_as_sfc(bbox_sf)
  bbox_3857 <- sf::st_transform(bbox_poly, 3857)
  bbox_3857 <- sf::st_bbox(bbox_3857)

  return(as.numeric(bbox_3857))
}


#' #' Get Multiple Tiles for Large Areas
#' #'
#' #' Retrieves multiple tiles for large areas and optionally merges them into a single GeoTIFF
#' #'
#' #' @param bbox Numeric vector of length 4 with bounding box in EPSG:3857
#' #' @param token Character string. Bearer token
#' #' @param tile_size Numeric. Size of each tile in meters. Default is 5000 (5km)
#' #' @param output_dir Character string. Directory for tile outputs. Default is "tiles"
#' #' @param merge_tiles Logical. Merge tiles into single GeoTIFF. Default is TRUE
#' #' @param final_output Character string. Name for merged output. Default is "merged_basemap.tif"
#' #' @param ... Additional arguments passed to get_maxar_wms_basemap()
#' #'
#' #' @return Path to the merged GeoTIFF (if merge_tiles=TRUE) or vector of tile paths
#' #'
#' #' @export
#' get_maxar_wms_tiles <- function(bbox,
#'                                 token,
#'                                 tile_size = 5000,
#'                                 output_dir = "tiles",
#'                                 merge_tiles = TRUE,
#'                                 final_output = "merged_basemap.tif",
#'                                 ...) {
#'
#'   # Create output directory
#'   dir.create(output_dir, showWarnings = FALSE, recursive = TRUE)
#'
#'   # Calculate tile grid
#'   x_range <- seq(bbox[1], bbox[3], by = tile_size)
#'   y_range <- seq(bbox[2], bbox[4], by = tile_size)
#'
#'   if (tail(x_range, 1) < bbox[3]) x_range <- c(x_range, bbox[3])
#'   if (tail(y_range, 1) < bbox[4]) y_range <- c(y_range, bbox[4])
#'
#'   # Generate tiles
#'   tiles <- expand.grid(x = x_range[-length(x_range)],
#'                        y = y_range[-length(y_range)])
#'
#'   cat(sprintf("Downloading %d tiles...\n", nrow(tiles)))
#'
#'   tile_files <- character(nrow(tiles))
#'
#'   # Download each tile
#'   for (i in 1:nrow(tiles)) {
#'     tile_bbox <- c(
#'       tiles$x[i],
#'       tiles$y[i],
#'       min(tiles$x[i] + tile_size, bbox[3]),
#'       min(tiles$y[i] + tile_size, bbox[4])
#'     )
#'
#'     tile_file <- file.path(output_dir, sprintf("tile_%03d.tif", i))
#'
#'     cat(sprintf("Downloading tile %d/%d...\n", i, nrow(tiles)))
#'
#'     get_maxar_wms_basemap(
#'       bbox = tile_bbox,
#'       token = token,
#'       output_file = tile_file,
#'       verbose = FALSE,
#'       ...
#'     )
#'
#'     tile_files[i] <- tile_file
#'   }
#'
#'   # Merge tiles if requested
#'   if (merge_tiles && length(tile_files) > 1) {
#'     cat("Merging tiles...\n")
#'
#'     # Load all tiles
#'     rasters <- lapply(tile_files, terra::rast)
#'
#'     # Merge
#'     if (length(rasters) > 1) {
#'       merged <- do.call(terra::merge, rasters)
#'     } else {
#'       merged <- rasters[[1]]
#'     }
#'
#'     # Save merged raster
#'     terra::writeRaster(merged, final_output, overwrite = TRUE)
#'
#'     cat(sprintf("Merged GeoTIFF saved: %s\n", final_output))
#'
#'     # Optionally clean up tiles
#'     # unlink(tile_files)
#'
#'     return(invisible(final_output))
#'   } else {
#'     return(invisible(tile_files))
#'   }
#' }
#'
#'
#' # Example usage with complete workflow
#' #' @examples
#' #' \dontrun{
#' #' library(httr2)
#' #' library(sf)
#' #' library(terra)
#' #'
#' #' # Step 1: Authenticate
#' #' token_response <- get_maxar_token(
#' #'   username = "your_email@example.com",
#' #'   password = "your_password"
#' #' )
#' #'
#' #' # Step 2: Define area of interest
#' #' # Option A: Direct Web Mercator coordinates
#' #' bbox_3857 <- c(-11700000, 4830000, -11690000, 4840000)
#' #'
#' #' # Option B: Convert from lat/lon
#' #' bbox_latlon <- c(-105.01, 39.72, -104.96, 39.75)  # Denver area
#' #' bbox_3857 <- convert_bbox_to_3857(bbox_latlon)
#' #'
#' #' # Step 3: Download single basemap
#' #' tif_file <- get_maxar_wms_basemap(
#' #'   bbox = bbox_3857,
#' #'   token = token_response$access_token,
#' #'   output_file = "vivid_30cm_basemap.tif",
#' #'   resolution = 0.3,  # 30 cm
#' #'   product = "VIVID_STANDARD_30"
#' #' )
#' #'
#' #' # Step 4: Load and visualize
#' #' raster <- terra::rast(tif_file)
#' #' terra::plotRGB(raster, r = 1, g = 2, b = 3, stretch = "lin")
#' #'
#' #' # For large areas, use tiled approach
#' #' large_bbox <- c(-11710000, 4820000, -11680000, 4850000)
#' #' merged_file <- get_maxar_wms_tiles(
#' #'   bbox = large_bbox,
#' #'   token = token_response$access_token,
#' #'   tile_size = 5000,  # 5km tiles
#' #'   resolution = 0.3,
#' #'   merge_tiles = TRUE
#' #' )
#' #' }
