test_that("vantor_wmts_url builds expected WMTS GetTile URL template", {
  vantor_auth_clear(verbose = FALSE)

  url <- vantor_wmts_url(
    product = "VIVID_STANDARD_30",
    api_key = "test_key_123",
    verbose = FALSE
  )

  expect_type(url, "character")
  expect_match(url, "https://api.maxar.com/basemaps/v1/ogc/gwc/service/wmts", fixed = TRUE)
  expect_match(url, "tileMatrix=EPSG:3857:{z}&tileCol={x}&tileRow={y}", fixed = TRUE)
  expect_match(url, "maxar_api_key=test_key_123", fixed = TRUE)
  expect_match(url, "VIVID_STANDARD_30", fixed = TRUE)
})

test_that("vantor_wmts_url errors informatively when only bearer token is set", {
  old_key <- Sys.getenv("VANTOR_API_KEY")
  old_mkey <- Sys.getenv("MAXAR_API_KEY")
  on.exit({
    Sys.setenv(VANTOR_API_KEY = old_key, MAXAR_API_KEY = old_mkey)
    vantor_auth_clear(verbose = FALSE)
  })

  Sys.setenv(VANTOR_API_KEY = "", MAXAR_API_KEY = "")
  vantor_auth_clear(verbose = FALSE)
  vantor_auth(token = "some_bearer_token", verbose = FALSE)

  expect_error(
    vantor_wmts_url(verbose = FALSE),
    "require a Vantor API key"
  )
})

test_that("leaflet helpers add layers to a leaflet widget", {
  skip_if_not_installed("leaflet")

  m <- leaflet::leaflet() |>
    add_vantor_basemap(
      product = "VIVID_STANDARD_30",
      service = "WMTS",
      api_key = "test_key_123",
      show_seamlines = TRUE,
      verbose = FALSE
    ) |>
    add_vantor_imagery(
      feature_id = "10300100EBEDF600",
      api_key = "test_key_123",
      verbose = FALSE
    )

  expect_s3_class(m, "leaflet")
  expect_true(length(m$x$calls) >= 3L)

  # Test WMS mode as well
  m_wms <- leaflet::leaflet() |>
    addVantorBasemap(
      product = "VIVID_ADVANCED_15",
      service = "WMS",
      api_key = "test_key_123",
      verbose = FALSE
    )
  expect_s3_class(m_wms, "leaflet")
})

test_that("seamline popup formatter produces valid HTML strings", {
  poly <- sf::st_sfc(sf::st_polygon(list(rbind(
    c(-105.01, 39.73), c(-105.00, 39.73),
    c(-105.00, 39.74), c(-105.01, 39.74),
    c(-105.01, 39.73)
  ))), crs = 4326)
  sf_obj <- sf::st_sf(
    acq_time = "2024-06-15",
    sensor = "WV03",
    gsd = 0.30,
    ona = 12.4,
    catid = "1040010099999900",
    geometry = poly
  )

  popups <- .format_seamline_popup(sf_obj)
  expect_type(popups, "character")
  expect_length(popups, 1L)
  expect_match(popups, "2024-06-15", fixed = TRUE)
  expect_match(popups, "WV03", fixed = TRUE)
  expect_match(popups, "1040010099999900", fixed = TRUE)
})

test_that("Shiny UI module generators return valid shiny tag lists", {
  skip_if_not_installed("shiny")
  skip_if_not_installed("leaflet")

  auth_tags <- vantor_auth_ui("test_auth")
  expect_s3_class(auth_tags, "shiny.tag.list")

  explorer_tags <- vantor_explorer_ui("test_explorer")
  expect_s3_class(explorer_tags, "shiny.tag.list")

  app <- run_vantor_demo_app(verbose = FALSE)
  expect_s3_class(app, "shiny.appobj")
})

test_that("vantor_get_seamlines and vantor_get_usage validate inputs", {
  old_key <- Sys.getenv("VANTOR_API_KEY")
  old_mkey <- Sys.getenv("MAXAR_API_KEY")
  old_user <- Sys.getenv("VANTOR_USERNAME")
  old_pass <- Sys.getenv("VANTOR_PASSWORD")
  on.exit({
    Sys.setenv(
      VANTOR_API_KEY = old_key,
      MAXAR_API_KEY = old_mkey,
      VANTOR_USERNAME = old_user,
      VANTOR_PASSWORD = old_pass
    )
    vantor_auth_clear(verbose = FALSE)
  })

  Sys.setenv(VANTOR_API_KEY = "", MAXAR_API_KEY = "", VANTOR_USERNAME = "", VANTOR_PASSWORD = "")
  vantor_auth_clear(verbose = FALSE)

  expect_error(vantor_get_seamlines(), "bbox")
  expect_error(vantor_get_seamlines(c(-105, 39.7, -104.9, 39.8), verbose = FALSE), "No Vantor/Maxar authentication")
  expect_error(vantor_get_usage(verbose = FALSE), "No Vantor/Maxar authentication")
})
