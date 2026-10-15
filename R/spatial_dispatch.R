#' Coerce Spatial Objects to a Bounding Box in a Target CRS
#'
#' S3 generic function that extracts a bounding box from spatial objects across
#' major R geography packages (`sf`, `terra`, `stars`) or numeric vectors and
#' transforms it to a target Coordinate Reference System (CRS).
#'
#' @param x A spatial object (`sf`, `sfc`, `bbox`, `SpatRaster`, `SpatVector`,
#'   `SpatExtent`, `stars`, `stars_proxy`) or a numeric vector of length 4:
#'   `c(xmin, ymin, xmax, ymax)`.
#' @param target_crs Integer or character string. Target EPSG code or CRS
#'   definition. Default is `3857` (Web Mercator).
#' @param source_crs Integer or character string. Source CRS used only when `x`
#'   is a plain numeric vector without a `"crs"` attribute. If `NULL`,
#'   heuristically detects `EPSG:4326` if coordinates fall within valid
#'   longitude/latitude bounds (`[-180, 180]` and `[-90, 90]`), or `EPSG:3857`
#'   otherwise.
#' @param ... Additional arguments passed to methods.
#'
#' @return An `sf` `bbox` object (`c(xmin, ymin, xmax, ymax)`) in `target_crs`.
#'
#' @examples
#' # From a lat/lon numeric vector (automatically detected as EPSG:4326)
#' vantor_as_bbox(c(-105.01, 39.72, -104.96, 39.75), target_crs = 3857)
#'
#' # From an sf bbox object
#' bb <- sf::st_bbox(c(xmin = -77.04, ymin = 38.88, xmax = -77.03, ymax = 38.89),
#'                   crs = sf::st_crs(4326))
#' vantor_as_bbox(bb, target_crs = 3857)
#'
#' @export
vantor_as_bbox <- function(x, target_crs = 3857, source_crs = NULL, ...) {
  UseMethod("vantor_as_bbox")
}

#' @export
vantor_as_bbox.bbox <- function(x, target_crs = 3857, source_crs = NULL, ...) {
  src_crs <- sf::st_crs(x)
  if (is.na(src_crs)) {
    if (!is.null(source_crs)) {
      src_crs <- sf::st_crs(source_crs)
    } else {
      src_crs <- .guess_numeric_crs(as.numeric(x))
    }
    x <- sf::st_bbox(
      c(xmin = unname(x["xmin"]), ymin = unname(x["ymin"]),
        xmax = unname(x["xmax"]), ymax = unname(x["ymax"])),
      crs = src_crs
    )
  }

  target_crs_obj <- sf::st_crs(target_crs)
  if (src_crs != target_crs_obj) {
    poly <- sf::st_as_sfc(x)
    poly_trans <- sf::st_transform(poly, target_crs_obj)
    x <- sf::st_bbox(poly_trans)
  }
  x
}

#' @export
vantor_as_bbox.sf <- function(x, target_crs = 3857, source_crs = NULL, ...) {
  vantor_as_bbox.bbox(sf::st_bbox(x), target_crs = target_crs, source_crs = source_crs, ...)
}

#' @export
vantor_as_bbox.sfc <- function(x, target_crs = 3857, source_crs = NULL, ...) {
  vantor_as_bbox.bbox(sf::st_bbox(x), target_crs = target_crs, source_crs = source_crs, ...)
}

#' @export
vantor_as_bbox.sfg <- function(x, target_crs = 3857, source_crs = NULL, ...) {
  sfc_obj <- sf::st_sfc(x, crs = if (!is.null(source_crs)) sf::st_crs(source_crs) else sf::NA_crs_)
  vantor_as_bbox.bbox(sf::st_bbox(sfc_obj), target_crs = target_crs, source_crs = source_crs, ...)
}

#' @export
vantor_as_bbox.numeric <- function(x, target_crs = 3857, source_crs = NULL, ...) {
  if (length(x) != 4L || anyNA(x)) {
    cli::cli_abort("{.arg bbox} must be a non-NA numeric vector of length 4: {.code c(xmin, ymin, xmax, ymax)}.")
  }

  # Handle named vectors or positional c(xmin, ymin, xmax, ymax)
  if (!is.null(names(x)) && all(c("xmin", "ymin", "xmax", "ymax") %in% names(x))) {
    vals <- c(
      xmin = unname(x["xmin"]),
      ymin = unname(x["ymin"]),
      xmax = unname(x["xmax"]),
      ymax = unname(x["ymax"])
    )
  } else {
    vals <- c(xmin = x[1], ymin = x[2], xmax = x[3], ymax = x[4])
  }

  if (vals["xmin"] >= vals["xmax"] || vals["ymin"] >= vals["ymax"]) {
    cli::cli_abort("Invalid bounding box: {.code xmin < xmax} and {.code ymin < ymax} must hold.")
  }

  if (is.null(source_crs)) {
    attr_crs <- attr(x, "crs")
    if (!is.null(attr_crs)) {
      src_crs <- sf::st_crs(attr_crs)
    } else {
      src_crs <- .guess_numeric_crs(vals)
    }
  } else {
    src_crs <- sf::st_crs(source_crs)
  }

  bb <- sf::st_bbox(vals, crs = src_crs)
  vantor_as_bbox.bbox(bb, target_crs = target_crs)
}

#' @export
vantor_as_bbox.SpatRaster <- function(x, target_crs = 3857, source_crs = NULL, ...) {
  if (!requireNamespace("terra", quietly = TRUE)) {
    cli::cli_abort("Package {.pkg terra} is required to extract bounding boxes from {.cls SpatRaster} objects.")
  }
  ext <- as.vector(terra::ext(x))
  crs_wkt <- terra::crs(x)
  src_crs <- if (nzchar(crs_wkt)) sf::st_crs(crs_wkt) else if (!is.null(source_crs)) sf::st_crs(source_crs) else sf::NA_crs_
  bb <- sf::st_bbox(
    c(xmin = unname(ext["xmin"]), ymin = unname(ext["ymin"]),
      xmax = unname(ext["xmax"]), ymax = unname(ext["ymax"])),
    crs = src_crs
  )
  vantor_as_bbox.bbox(bb, target_crs = target_crs, source_crs = source_crs, ...)
}

#' @export
vantor_as_bbox.SpatVector <- function(x, target_crs = 3857, source_crs = NULL, ...) {
  if (!requireNamespace("terra", quietly = TRUE)) {
    cli::cli_abort("Package {.pkg terra} is required to extract bounding boxes from {.cls SpatVector} objects.")
  }
  ext <- as.vector(terra::ext(x))
  crs_wkt <- terra::crs(x)
  src_crs <- if (nzchar(crs_wkt)) sf::st_crs(crs_wkt) else if (!is.null(source_crs)) sf::st_crs(source_crs) else sf::NA_crs_
  bb <- sf::st_bbox(
    c(xmin = unname(ext["xmin"]), ymin = unname(ext["ymin"]),
      xmax = unname(ext["xmax"]), ymax = unname(ext["ymax"])),
    crs = src_crs
  )
  vantor_as_bbox.bbox(bb, target_crs = target_crs, source_crs = source_crs, ...)
}

#' @export
vantor_as_bbox.SpatExtent <- function(x, target_crs = 3857, source_crs = NULL, ...) {
  if (!requireNamespace("terra", quietly = TRUE)) {
    cli::cli_abort("Package {.pkg terra} is required to extract bounding boxes from {.cls SpatExtent} objects.")
  }
  ext <- as.vector(x)
  vals <- c(
    xmin = unname(ext["xmin"]), ymin = unname(ext["ymin"]),
    xmax = unname(ext["xmax"]), ymax = unname(ext["ymax"])
  )
  vantor_as_bbox.numeric(vals, target_crs = target_crs, source_crs = source_crs, ...)
}

#' @export
vantor_as_bbox.stars <- function(x, target_crs = 3857, source_crs = NULL, ...) {
  vantor_as_bbox.bbox(sf::st_bbox(x), target_crs = target_crs, source_crs = source_crs, ...)
}

#' @export
vantor_as_bbox.stars_proxy <- function(x, target_crs = 3857, source_crs = NULL, ...) {
  vantor_as_bbox.bbox(sf::st_bbox(x), target_crs = target_crs, source_crs = source_crs, ...)
}

#' Internal Heuristic for Numeric Bbox CRS
#' @noRd
.guess_numeric_crs <- function(vals) {
  # If within standard geographic bounds and extent is small enough not to be meters near (0,0)
  if (all(vals[c(1, 3)] >= -180 & vals[c(1, 3)] <= 180) &&
      all(vals[c(2, 4)] >= -90 & vals[c(2, 4)] <= 90)) {
    sf::st_crs(4326)
  } else {
    sf::st_crs(3857)
  }
}

#' Convert File Path to Requested Spatial Raster Class (Internal)
#' @noRd
.format_raster_return <- function(file_path,
                                  return_type = c("file", "SpatRaster", "stars", "stars_proxy"),
                                  invisible_file = TRUE) {
  return_type <- match.arg(return_type)

  if (identical(return_type, "file")) {
    if (invisible_file) {
      return(invisible(file_path))
    }
    return(file_path)
  }

  if (identical(return_type, "SpatRaster")) {
    if (!requireNamespace("terra", quietly = TRUE)) {
      cli::cli_abort("Package {.pkg terra} must be installed when {.code return_type = \"SpatRaster\"}.")
    }
    return(terra::rast(file_path))
  }

  if (return_type %in% c("stars", "stars_proxy")) {
    if (!requireNamespace("stars", quietly = TRUE)) {
      cli::cli_abort("Package {.pkg stars} must be installed when {.code return_type = \"{return_type}\"}.")
    }
    return(stars::read_stars(file_path, proxy = identical(return_type, "stars_proxy")))
  }

  file_path
}
