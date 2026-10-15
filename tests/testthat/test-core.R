test_that("vantor_as_bbox works with numeric vectors and sf objects", {
  # Lat/lon numeric vector (auto-detected as EPSG:4326)
  bb_3857 <- vantor_as_bbox(c(-105.01, 39.72, -104.96, 39.75), target_crs = 3857)
  expect_s3_class(bb_3857, "bbox")
  expect_equal(sf::st_crs(bb_3857)$epsg, 3857)
  expect_true(bb_3857["xmin"] < -11600000)

  # sf bbox object
  sf_bb <- sf::st_bbox(
    c(xmin = -77.040, ymin = 38.888, xmax = -77.030, ymax = 38.892),
    crs = sf::st_crs(4326)
  )
  bb_out <- vantor_as_bbox(sf_bb, target_crs = 3857)
  expect_s3_class(bb_out, "bbox")
  expect_equal(sf::st_crs(bb_out)$epsg, 3857)

  # sfc polygon object
  sfc_poly <- sf::st_as_sfc(sf_bb)
  bb_sfc <- vantor_as_bbox(sfc_poly, target_crs = 4326)
  expect_equal(as.numeric(bb_sfc), as.numeric(sf_bb), tolerance = 1e-6)

  # convert_bbox_to_3857 helper backwards compatibility
  vec_3857 <- convert_bbox_to_3857(c(-105.01, 39.72, -104.96, 39.75))
  expect_type(vec_3857, "double")
  expect_length(vec_3857, 4)

  # Invalid bbox errors cleanly
  expect_error(vantor_as_bbox(c(1, 2, 3)), "length 4")
  expect_error(vantor_as_bbox(c(10, 20, 5, 25)), "Invalid bounding box")
})

test_that("vantor_as_bbox works with terra and stars objects if installed", {
  skip_if_not_installed("terra")
  r <- terra::rast(xmin = -105.01, xmax = -104.96, ymin = 39.72, ymax = 39.75, crs = "EPSG:4326")
  bb_terra <- vantor_as_bbox(r, target_crs = 3857)
  expect_s3_class(bb_terra, "bbox")
  expect_equal(sf::st_crs(bb_terra)$epsg, 3857)

  ext_obj <- terra::ext(r)
  bb_ext <- vantor_as_bbox(ext_obj, target_crs = 3857, source_crs = 4326)
  expect_equal(as.numeric(bb_terra), as.numeric(bb_ext), tolerance = 1e-5)
})

test_that("authentication session management works cleanly", {
  vantor_auth_clear(verbose = FALSE)

  # Set API key
  res <- vantor_auth(api_key = "test_key_123", verbose = FALSE)
  expect_equal(res$type, "api_key")
  expect_true(vantor_has_auth())

  req <- create_maxar_request("https://api.maxar.com/discovery/v1")
  expect_s3_class(req, "httr2_request")
  expect_equal(req$headers[["maxar-api-key"]], "test_key_123")

  vantor_auth_clear(verbose = FALSE)
})
