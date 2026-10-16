test_that("vantor_survey_grid creates square and hexagonal alphanumeric grids", {
  aoi <- sf::st_bbox(
    c(xmin = -105.01, ymin = 39.73, xmax = -104.99, ymax = 39.74),
    crs = sf::st_crs(4326)
  )

  sq_grid <- vantor_survey_grid(aoi, cell_size = 500, shape = "square", verbose = FALSE)
  expect_s3_class(sq_grid, "sf")
  expect_true(all(c("cell_id", "row_idx", "col_idx", "centroid_lon", "centroid_lat", "area_m2") %in% names(sq_grid)))
  expect_equal(sq_grid$cell_id[1], "A1")
  expect_true(all(sq_grid$area_m2 > 0))

  hex_grid <- vantor_survey_grid(aoi, cell_size = 500, shape = "hexagonal", verbose = FALSE)
  expect_s3_class(hex_grid, "sf")
  expect_true(nrow(hex_grid) >= 1L)

  expect_error(vantor_survey_grid(aoi, cell_size = -10), "positive number")
})

test_that("vantor_vintage_report computes vintage metrics and flags outdated cells", {
  aoi <- sf::st_bbox(
    c(xmin = -105.01, ymin = 39.73, xmax = -105.00, ymax = 39.74),
    crs = sf::st_crs(4326)
  )
  grid_sf <- vantor_survey_grid(aoi, cell_size = 600, verbose = FALSE)

  mock_sl <- sf::st_sf(
    acq_time = "2020-01-15T18:00:00Z",
    gsd = 0.30,
    ona = 14.2,
    sensor = "WV03",
    catid = "1040010012345600",
    geometry = sf::st_as_sfc(aoi)
  )

  rep_sf <- vantor_vintage_report(
    aoi = grid_sf,
    seamlines = mock_sl,
    max_age_days = 730,
    reference_date = as.Date("2026-01-15"),
    verbose = FALSE
  )

  expect_s3_class(rep_sf, "sf")
  expect_equal(rep_sf$earliest_acq_date[1], "2020-01-15")
  expect_equal(rep_sf$sensors[1], "WV03")
  expect_true(all(rep_sf$vintage_warning))
})

test_that("vantor_plus_code encodes 10 and 11 character Open Location Codes", {
  code10 <- vantor_plus_code(lon = -104.995, lat = 39.740, code_length = 10L)
  expect_type(code10, "character")
  expect_equal(nchar(code10), 11L) # 10 digits + '+'
  expect_match(code10, "^[23456789CFGHJMPQRVWX]{8}\\+[23456789CFGHJMPQRVWX]{2}$")

  code11 <- vantor_plus_code(lon = c(-104.995, NA), lat = c(39.740, NA), code_length = 11L)
  expect_length(code11, 2L)
  expect_equal(nchar(code11[1]), 12L)
  expect_true(is.na(code11[2]))
})

test_that("vantor_export_capi enriches samples and writes CSV and GeoJSON", {
  aoi <- sf::st_bbox(
    c(xmin = -105.01, ymin = 39.73, xmax = -105.00, ymax = 39.74),
    crs = sf::st_crs(4326)
  )
  grid_sf <- vantor_survey_grid(aoi, cell_size = 600, verbose = FALSE)

  pts <- sf::st_as_sf(
    data.frame(
      hh_id = c("HH_01", "HH_02"),
      lon = c(-105.006, -105.003),
      lat = c(39.736, 39.733)
    ),
    coords = c("lon", "lat"),
    crs = 4326
  )

  tmp_csv <- tempfile(fileext = ".csv")
  tmp_json <- tempfile(fileext = ".geojson")
  tmp_suso <- tempfile(fileext = ".tab")
  on.exit(unlink(c(tmp_csv, tmp_json, tmp_suso)), add = TRUE)

  out_default <- vantor_export_capi(
    samples = pts,
    id_col = "hh_id",
    grid = grid_sf,
    verbose = FALSE
  )
  expect_true(all(c("longitude", "latitude") %in% names(out_default)))

  out_sf <- vantor_export_capi(
    samples = pts,
    id_col = "hh_id",
    grid = grid_sf,
    output_csv = tmp_csv,
    output_geojson = tmp_json,
    output_suso_tab = tmp_suso,
    suso_gps_var = "hh_gps",
    verbose = FALSE
  )

  expect_s3_class(out_sf, "sf")
  expect_true(all(c("sample_id", "hh_gps__Latitude", "hh_gps__Longitude", "odk_geopoint", "plus_code", "google_maps_url", "cell_id") %in% names(out_sf)))
  expect_false(any(c("longitude", "latitude") %in% names(out_sf)))
  expect_true(file.exists(tmp_csv))
  expect_true(file.exists(tmp_json))
  expect_true(file.exists(tmp_suso))

  suso_df <- utils::read.delim(tmp_suso, check.names = FALSE)
  expect_true(all(c("sample_id", "cell_id", "plus_code", "hh_gps__Latitude", "hh_gps__Longitude", "_target_area") %in% names(suso_df)))
  expect_false(any(c("longitude", "latitude", "_quantity") %in% names(suso_df)))
})

test_that("vantor_field_atlas and vantor_offline_package create expected files", {
  aoi <- sf::st_bbox(
    c(xmin = -105.01, ymin = 39.73, xmax = -105.00, ymax = 39.74),
    crs = sf::st_crs(4326)
  )
  grid_sf <- vantor_survey_grid(aoi, cell_size = 800, verbose = FALSE)

  pts <- sf::st_as_sf(
    data.frame(sample_id = "S001", lon = -105.005, lat = 39.735),
    coords = c("lon", "lat"),
    crs = 4326
  )

  tmp_pdf <- tempfile(fileext = ".pdf")
  on.exit(unlink(tmp_pdf), add = TRUE)

  res_pdf <- vantor_field_atlas(
    grid = grid_sf,
    points = pts,
    output_file = tmp_pdf,
    verbose = FALSE
  )

  expect_equal(res_pdf, tmp_pdf)
  expect_true(file.exists(tmp_pdf))
  expect_gt(file.size(tmp_pdf), 1000)

  bundle <- vantor_offline_package(
    grid = grid_sf,
    samples = pts,
    verbose = FALSE
  )
  on.exit(unlink(bundle$output_dir, recursive = TRUE), add = TRUE)

  expect_true(file.exists(bundle$gpkg_path))
  expect_true(file.exists(bundle$csv_path))
  expect_true(file.exists(bundle$suso_tab_path))
  expect_equal(sort(bundle$layers), c("survey_grid", "survey_samples"))
})

test_that("vantor_write_tpk generates valid Compact Cache V1 (.tpk) and V2 (.tpkx) archives", {
  skip_if_not_installed("terra")

  r <- terra::rast(
    xmin = -11689500, xmax = -11688500,
    ymin = 4826500, ymax = 4827500,
    nrows = 64, ncols = 64, nlyrs = 3,
    crs = "EPSG:3857"
  )
  terra::values(r) <- sample(40:230, terra::ncell(r) * 3, replace = TRUE)

  tmp_tpk <- tempfile(fileext = ".tpk")
  tmp_tpkx <- tempfile(fileext = ".tpkx")
  on.exit(unlink(c(tmp_tpk, tmp_tpkx)), add = TRUE)

  res_tpk <- vantor_write_tpk(
    raster = r,
    output_file = tmp_tpk,
    zoom = 14:15,
    zip_for_suso = TRUE,
    verbose = FALSE
  )
  on.exit(unlink(attr(res_tpk, "suso_zip_path")), add = TRUE)
  expect_true(file.exists(res_tpk))
  expect_equal(attr(res_tpk, "format"), "tpk")
  expect_gt(attr(res_tpk, "n_tiles"), 0L)
  expect_true(grepl("_tpk\\.zip$", attr(res_tpk, "suso_zip_path")))
  expect_true(file.exists(attr(res_tpk, "suso_zip_path")))

  tpk_contents <- utils::unzip(res_tpk, list = TRUE)
  expect_true("v101/Layers/conf.xml" %in% tpk_contents$Name)
  expect_true("v101/Layers/conf.cdi" %in% tpk_contents$Name)
  expect_true("esriinfo/iteminfo.xml" %in% tpk_contents$Name)
  expect_true("esriinfo/item.pkinfo" %in% tpk_contents$Name)
  expect_true("servicedescriptions/mapserver/mapserver.json" %in% tpk_contents$Name)
  bundlx_rows <- tpk_contents[grepl("\\.bundlx$", tpk_contents$Name), , drop = FALSE]
  expect_gt(nrow(bundlx_rows), 0L)
  expect_true(all(bundlx_rows$Length == 81952))

  # Verify mapserver.json contains the 'contents' wrapper required by Survey Solutions HQ
  ex_dir <- tempfile(pattern = "tpk_check_")
  dir.create(ex_dir)
  on.exit(unlink(ex_dir, recursive = TRUE), add = TRUE)
  utils::unzip(res_tpk, files = "servicedescriptions/mapserver/mapserver.json", exdir = ex_dir)
  ms_txt <- paste(readLines(file.path(ex_dir, "servicedescriptions/mapserver/mapserver.json"), warn = FALSE), collapse = "\n")
  expect_match(ms_txt, "\"contents\"\\s*:\\s*\\{")
  expect_match(ms_txt, "\"fullExtent\"\\s*:\\s*\\{")
  expect_match(ms_txt, "\"minScale\"\\s*:")
  expect_match(ms_txt, "\"maxScale\"\\s*:")

  res_tpkx <- vantor_write_tpk(
    raster = r,
    output_file = tmp_tpkx,
    zoom = 14:15,
    verbose = FALSE
  )
  expect_true(file.exists(res_tpkx))
  expect_equal(attr(res_tpkx, "format"), "tpkx")

  tpkx_contents <- utils::unzip(res_tpkx, list = TRUE)
  expect_true("root.json" %in% tpkx_contents$Name)
  expect_true("iteminfo.json" %in% tpkx_contents$Name)
  bundle_rows <- tpkx_contents[grepl("\\.bundle$", tpkx_contents$Name), , drop = FALSE]
  expect_gt(nrow(bundle_rows), 0L)
  expect_true(all(bundle_rows$Length > 131136))
})

test_that("vantor_offline_package supports Survey Solutions TPK and zipped Shapefiles", {
  skip_if_not_installed("terra")

  aoi <- sf::st_bbox(
    c(xmin = -105.01, ymin = 39.73, xmax = -105.00, ymax = 39.74),
    crs = sf::st_crs(4326)
  )
  grid_sf <- vantor_survey_grid(aoi, cell_size = 800, verbose = FALSE)
  pts <- sf::st_as_sf(
    data.frame(sample_id = "S001", lon = -105.005, lat = 39.735),
    coords = c("lon", "lat"),
    crs = 4326
  )

  # Supply a WGS84 raster to verify automatic reprojection to EPSG:3857
  r_4326 <- terra::rast(
    xmin = -105.01, xmax = -105.00,
    ymin = 39.73, ymax = 39.74,
    nrows = 32, ncols = 32, nlyrs = 3,
    crs = "EPSG:4326"
  )
  terra::values(r_4326) <- 120

  bundle <- vantor_offline_package(
    grid = grid_sf,
    samples = pts,
    raster = r_4326,
    create_tpk = "tpkx",
    tpk_zoom = 14,
    create_shapefiles = TRUE,
    verbose = FALSE
  )
  on.exit(unlink(bundle$output_dir, recursive = TRUE), add = TRUE)

  expect_true(file.exists(bundle$tpk_path))
  expect_true(grepl("\\.tpkx$", bundle$tpk_path))
  expect_true(file.exists(bundle$shapefile_zip_path))
  expect_true(file.exists(bundle$raster_path))

  # Verify the saved GeoTIFF was reprojected to EPSG:3857 for Survey Solutions
  r_out <- terra::rast(bundle$raster_path)
  expect_equal(as.character(terra::crs(r_out, describe = TRUE)$code), "3857")
})

