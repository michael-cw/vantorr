#' Vantor Authentication Shiny Module UI
#'
#' Creates a compact, namespace-safe Shiny UI panel for configuring Vantor /
#' Maxar API credentials (API Key or OAuth2 Username/Password), displaying
#' connection status, and optionally checking account credit usage.
#'
#' @param id Character string. The Shiny module namespace ID.
#' @param show_usage Logical. Include a button and output area to query account
#'   credit usage via [vantor_get_usage()]. Default is `TRUE`.
#'
#' @return A `shiny.tag.list` UI definition.
#'
#' @examples
#' if (requireNamespace("shiny", quietly = TRUE)) {
#'   ui <- shiny::fluidPage(
#'     vantor_auth_ui("auth")
#'   )
#' }
#'
#' @export
vantor_auth_ui <- function(id, show_usage = TRUE) {
  if (!requireNamespace("shiny", quietly = TRUE)) {
    cli::cli_abort("Package {.pkg shiny} is required for {.fn vantor_auth_ui}. Install it with {.code install.packages(\"shiny\")}.")
  }

  ns <- shiny::NS(id)

  shiny::tagList(
    shiny::wellPanel(
      shiny::tags$h4("Vantor API Authentication"),
      shiny::uiOutput(ns("auth_status_badge")),
      shiny::radioButtons(
        inputId = ns("auth_mode"),
        label = "Authentication Method",
        choices = c("API Key (Recommended for Leaflet)" = "api_key",
                    "OAuth2 (Username & Password)" = "oauth"),
        selected = "api_key",
        inline = TRUE
      ),
      shiny::conditionalPanel(
        condition = sprintf("input['%s'] == 'api_key'", ns("auth_mode")),
        shiny::passwordInput(
          inputId = ns("api_key"),
          label = "Vantor / Maxar API Key",
          placeholder = "Enter API key or leave blank to use env var"
        )
      ),
      shiny::conditionalPanel(
        condition = sprintf("input['%s'] == 'oauth'", ns("auth_mode")),
        shiny::textInput(
          inputId = ns("username"),
          label = "Username (Email)",
          placeholder = "user@example.com"
        ),
        shiny::passwordInput(
          inputId = ns("password"),
          label = "Password"
        )
      ),
      shiny::div(
        style = "display:flex; gap:8px; flex-wrap:wrap;",
        shiny::actionButton(
          inputId = ns("connect_btn"),
          label = "Authenticate",
          class = "btn-primary btn-sm"
        ),
        shiny::actionButton(
          inputId = ns("clear_btn"),
          label = "Clear",
          class = "btn-default btn-sm"
        ),
        if (isTRUE(show_usage)) {
          shiny::actionButton(
            inputId = ns("usage_btn"),
            label = "Check Usage",
            class = "btn-info btn-sm"
          )
        }
      ),
      if (isTRUE(show_usage)) {
        shiny::uiOutput(ns("usage_summary"))
      }
    )
  )
}


#' Vantor Authentication Shiny Module Server
#'
#' Server logic for the [vantor_auth_ui()] Shiny module. Manages session
#' credentials, provides reactive authentication state for downstream modules
#' (such as [vantor_explorer_server()]), and automatically refreshes OAuth2
#' bearer tokens in the background so long-running Shiny sessions do not expire.
#'
#' @param id Character string. The Shiny module namespace ID.
#' @param auto_refresh_ms Numeric. Interval in milliseconds to check and
#'   automatically refresh expiring OAuth2 tokens via `shiny::invalidateLater()`.
#'   Default is `300000` (5 minutes).
#' @param verbose Logical. Print status messages to the R console. Default is
#'   `getOption("vantorr.verbose", TRUE)`.
#'
#' @return A `shiny::reactive` expression returning a list with elements:
#'   `authenticated` (logical), `type` (`"api_key"`, `"bearer"`, or `"none"`),
#'   `api_key` (character or `NULL`), and `updated_at` (`POSIXct`).
#'
#' @examples
#' if (requireNamespace("shiny", quietly = TRUE)) {
#'   server <- function(input, output, session) {
#'     auth_state <- vantor_auth_server("auth", verbose = FALSE)
#'   }
#' }
#'
#' @export
vantor_auth_server <- function(id,
                               auto_refresh_ms = 300000,
                               verbose = getOption("vantorr.verbose", TRUE)) {
  if (!requireNamespace("shiny", quietly = TRUE)) {
    cli::cli_abort("Package {.pkg shiny} is required for {.fn vantor_auth_server}. Install it with {.code install.packages(\"shiny\")}.")
  }

  shiny::moduleServer(id, function(input, output, session) {
    # Initialize from existing session or environment variables if available
    init_has_auth <- vantor_has_auth(verbose = FALSE)
    if (init_has_auth && is.null(.vantorr_env$api_key) && is.null(.vantorr_env$access_token)) {
      vantor_auth(verbose = FALSE)
    }

    state <- shiny::reactiveValues(
      authenticated = vantor_has_auth(verbose = FALSE),
      type = if (!is.null(.vantorr_env$api_key)) {
        "api_key"
      } else if (!is.null(.vantorr_env$access_token)) {
        "bearer"
      } else {
        "none"
      },
      api_key = .vantorr_env$api_key,
      usage_info = NULL,
      updated_at = Sys.time()
    )

    # Periodic background check to refresh OAuth2 tokens before expiration
    shiny::observe({
      shiny::invalidateLater(auto_refresh_ms, session)
      if (!is.null(.vantorr_env$access_token) &&
          !is.null(.vantorr_env$expires_at) &&
          !is.null(.vantorr_env$refresh_token)) {
        # Refresh if expiring within 10 minutes
        if (Sys.time() >= (.vantorr_env$expires_at - 600)) {
          refreshed <- refresh_maxar_token(verbose = verbose)
          if (!is.null(refreshed)) {
            state$updated_at <- Sys.time()
          }
        }
      }
    })

    shiny::observeEvent(input$connect_btn, {
      if (identical(input$auth_mode, "api_key")) {
        key_val <- trimws(input$api_key %||% "")
        res <- if (nzchar(key_val)) {
          vantor_auth(api_key = key_val, verbose = verbose)
        } else {
          vantor_auth(verbose = verbose)
        }
      } else {
        user_val <- trimws(input$username %||% "")
        pass_val <- input$password %||% ""
        res <- if (nzchar(user_val) && nzchar(pass_val)) {
          vantor_auth(username = user_val, password = pass_val, verbose = verbose)
        } else {
          vantor_auth(verbose = verbose)
        }
      }

      if (!is.null(res)) {
        state$authenticated <- TRUE
        state$type <- res$type
        state$api_key <- .vantorr_env$api_key
        state$updated_at <- Sys.time()
        shiny::showNotification(
          paste0("Vantor authenticated (", res$type, ")."),
          type = "message"
        )
      } else {
        state$authenticated <- vantor_has_auth(verbose = FALSE)
        shiny::showNotification(
          "Authentication failed. Check credentials or console logs.",
          type = "error"
        )
      }
    })

    shiny::observeEvent(input$clear_btn, {
      vantor_auth_clear(verbose = verbose)
      state$authenticated <- FALSE
      state$type <- "none"
      state$api_key <- NULL
      state$usage_info <- NULL
      state$updated_at <- Sys.time()
      shiny::showNotification("Cleared Vantor session credentials.", type = "warning")
    })

    shiny::observeEvent(input$usage_btn, {
      has_bearer <- (!is.null(.vantorr_env$access_token) && nzchar(.vantorr_env$access_token)) ||
        (nzchar(Sys.getenv("VANTOR_USERNAME", unset = Sys.getenv("MAXAR_USERNAME", unset = ""))) &&
         nzchar(Sys.getenv("VANTOR_PASSWORD", unset = Sys.getenv("MAXAR_PASSWORD", unset = ""))))

      if (!has_bearer) {
        shiny::showNotification(
          "The Usage API is an Admin endpoint and requires OAuth2 (Username & Password) authentication rather than an API Key.",
          type = "warning",
          duration = 8
        )
        return()
      }

      shiny::withProgress(message = "Querying Vantor Usage API...", value = 0.5, {
        usage_res <- tryCatch(
          vantor_get_usage(verbose = verbose),
          error = function(e) {
            shiny::showNotification(conditionMessage(e), type = "error")
            NULL
          }
        )
        state$usage_info <- usage_res
      })
    })

    output$auth_status_badge <- shiny::renderUI({
      if (isTRUE(state$authenticated)) {
        badge_label <- if (identical(state$type, "api_key")) {
          "Connected (API Key)"
        } else {
          "Connected (OAuth2 Bearer)"
        }
        shiny::tags$p(
          style = "color:#1b7e3c; font-weight:600; margin-bottom:8px;",
          "\u2713 ", badge_label
        )
      } else {
        shiny::tags$p(
          style = "color:#a94442; font-weight:600; margin-bottom:8px;",
          "\u26a0 Not Authenticated"
        )
      }
    })

    output$usage_summary <- shiny::renderUI({
      u <- state$usage_info
      if (is.null(u)) return(NULL)
      if (!is.null(u$summary_df) && nrow(u$summary_df) > 0L) {
        df <- u$summary_df[1, , drop = FALSE]
        shiny::tags$div(
          style = "margin-top:10px; font-size:12px; background:#f8f9fa; padding:8px; border-radius:4px;",
          shiny::tags$strong("Activation: "), df$activation_number, shiny::tags$br(),
          shiny::tags$strong("Credits Used: "),
          sprintf("%.1f / %.1f (%.1f%%)", df$total_credits_used, df$credit_limit, df$percentage_used)
        )
      } else {
        shiny::tags$div(
          style = "margin-top:10px; font-size:12px;",
          "Usage overview retrieved (no active credit table returned)."
        )
      }
    })

    shiny::reactive({
      list(
        authenticated = state$authenticated,
        type = state$type,
        api_key = state$api_key,
        updated_at = state$updated_at
      )
    })
  })
}


#' Vantor Interactive Imagery Explorer Shiny Module UI
#'
#' Creates a drop-in, namespace-safe Shiny UI component featuring an interactive
#' `leaflet` map with Vantor Vivid Mosaic basemap switching (`VIVID_STANDARD_30`
#' and `VIVID_ADVANCED_15`), seamline cutline overlay, WFS seamline vintage
#' inspection, live AOI bounding box metrics, and one-click GeoTIFF export.
#'
#' @param id Character string. The Shiny module namespace ID.
#' @param height Character string. CSS height of the `leaflet` map widget.
#'   Default is `"550px"`.
#'
#' @return A `shiny.tag.list` UI definition.
#'
#' @examples
#' if (requireNamespace("shiny", quietly = TRUE) &&
#'     requireNamespace("leaflet", quietly = TRUE)) {
#'   ui <- shiny::fluidPage(
#'     vantor_explorer_ui("explorer")
#'   )
#' }
#'
#' @export
vantor_explorer_ui <- function(id, height = "550px") {
  if (!requireNamespace("shiny", quietly = TRUE)) {
    cli::cli_abort("Package {.pkg shiny} is required for {.fn vantor_explorer_ui}. Install it with {.code install.packages(\"shiny\")}.")
  }
  if (!requireNamespace("leaflet", quietly = TRUE)) {
    cli::cli_abort("Package {.pkg leaflet} is required for {.fn vantor_explorer_ui}. Install it with {.code install.packages(\"leaflet\")}.")
  }

  ns <- shiny::NS(id)

  shiny::tagList(
    shiny::fluidRow(
      shiny::column(
        width = 4,
        shiny::wellPanel(
          shiny::tags$h4("Vantor Imagery Controls"),
          shiny::selectInput(
            inputId = ns("product"),
            label = "Vivid Mosaic Product",
            choices = c(
              "Vivid Standard 30 cm" = "VIVID_STANDARD_30",
              "Vivid Advanced 15 cm" = "VIVID_ADVANCED_15"
            ),
            selected = "VIVID_STANDARD_30"
          ),
          shiny::selectInput(
            inputId = ns("service"),
            label = "Streaming Protocol",
            choices = c("WMTS (Cached Tiles)" = "WMTS", "WMS (Dynamic)" = "WMS"),
            selected = "WMTS"
          ),
          shiny::checkboxInput(
            inputId = ns("show_seamlines"),
            label = "Overlay Seamlines (WMS)",
            value = FALSE
          ),
          shiny::actionButton(
            inputId = ns("inspect_wfs"),
            label = "Inspect Seamline Dates (WFS)",
            class = "btn-info btn-sm btn-block",
            style = "margin-bottom:12px;"
          ),
          shiny::tags$hr(),
          shiny::tags$h5("Export Current Map Viewport"),
          shiny::selectInput(
            inputId = ns("resolution"),
            label = "Output Resolution (meters)",
            choices = c(
              "0.15 m (15 cm)" = 0.15,
              "0.30 m (30 cm)" = 0.30,
              "0.50 m (50 cm)" = 0.50,
              "1.00 m (1 m)" = 1.00,
              "2.00 m (2 m)" = 2.00
            ),
            selected = 0.50
          ),
          shiny::uiOutput(ns("aoi_summary")),
          shiny::downloadButton(
            outputId = ns("download_geotiff"),
            label = "Download GeoTIFF",
            class = "btn-success btn-sm btn-block"
          )
        )
      ),
      shiny::column(
        width = 8,
        leaflet::leafletOutput(ns("map"), height = height)
      )
    )
  )
}


#' Vantor Interactive Imagery Explorer Shiny Module Server
#'
#' Server logic for the [vantor_explorer_ui()] Shiny module. Renders an
#' interactive `leaflet` map with Vantor Vivid basemaps, updates layers via
#' `leaflet::leafletProxy()`, queries WFS seamlines for the current viewport,
#' and provides a `shiny::downloadHandler()` with progress bar integration for
#' exporting GeoTIFFs via [get_maxar_wms_basemap()] or [get_maxar_wms_tiles()].
#'
#' @param id Character string. The Shiny module namespace ID.
#' @param initial_bbox Numeric vector of length 4 (`c(xmin, ymin, xmax, ymax)`
#'   in `EPSG:4326`) or any spatial object supported by [vantor_as_bbox()]
#'   defining the initial map view. Defaults to downtown Denver, CO.
#' @param auth_reactive Optional `shiny::reactive` returned by
#'   [vantor_auth_server()] to automatically refresh map layers when credentials
#'   change.
#' @param verbose Logical. Print status messages to the R console. Default is
#'   `getOption("vantorr.verbose", TRUE)`.
#'
#' @return A list of `shiny::reactive` expressions:
#'   - `bbox`: Reactive `sf::bbox` (`EPSG:4326`) of the current map viewport
#'   - `seamlines`: Reactive `sf` object of WFS seamlines when inspected
#'   - `product`: Reactive selected Vivid product name
#'   - `resolution`: Reactive selected ground resolution in meters
#'
#' @examples
#' if (requireNamespace("shiny", quietly = TRUE) &&
#'     requireNamespace("leaflet", quietly = TRUE)) {
#'   server <- function(input, output, session) {
#'     explorer <- vantor_explorer_server("explorer", verbose = FALSE)
#'   }
#' }
#'
#' @export
vantor_explorer_server <- function(id,
                                   initial_bbox = c(xmin = -105.01, ymin = 39.73, xmax = -104.98, ymax = 39.75),
                                   auth_reactive = NULL,
                                   verbose = getOption("vantorr.verbose", TRUE)) {
  if (!requireNamespace("shiny", quietly = TRUE)) {
    cli::cli_abort("Package {.pkg shiny} is required for {.fn vantor_explorer_server}. Install it with {.code install.packages(\"shiny\")}.")
  }
  if (!requireNamespace("leaflet", quietly = TRUE)) {
    cli::cli_abort("Package {.pkg leaflet} is required for {.fn vantor_explorer_server}. Install it with {.code install.packages(\"leaflet\")}.")
  }

  init_bb <- as.numeric(vantor_as_bbox(initial_bbox, target_crs = 4326))

  shiny::moduleServer(id, function(input, output, session) {
    rv <- shiny::reactiveValues(
      seamlines = NULL
    )

    # Current map viewport as an sf::bbox in EPSG:4326
    current_bbox <- shiny::reactive({
      b <- input$map_bounds
      if (is.null(b)) {
        return(sf::st_bbox(
          c(xmin = init_bb[1], ymin = init_bb[2], xmax = init_bb[3], ymax = init_bb[4]),
          crs = sf::st_crs(4326)
        ))
      }
      sf::st_bbox(
        c(xmin = b$west, ymin = b$south, xmax = b$east, ymax = b$north),
        crs = sf::st_crs(4326)
      )
    })

    output$map <- leaflet::renderLeaflet({
      m <- leaflet::leaflet() |>
        leaflet::addTiles(group = "OpenStreetMap") |>
        leaflet::fitBounds(
          lng1 = init_bb[1], lat1 = init_bb[2],
          lng2 = init_bb[3], lat2 = init_bb[4]
        )

      # Add Vantor basemap if an API key is available
      api_key <- .vantorr_env$api_key
      if (is.null(api_key) || !nzchar(api_key)) {
        env_key <- Sys.getenv("VANTOR_API_KEY", unset = Sys.getenv("MAXAR_API_KEY", unset = ""))
        if (nzchar(env_key)) api_key <- env_key
      }

      if (!is.null(api_key) && nzchar(api_key)) {
        m <- add_vantor_basemap(
          map = m,
          product = "VIVID_STANDARD_30",
          service = "WMTS",
          api_key = api_key,
          group = "Vantor Vivid",
          layerId = "vantor_base",
          verbose = verbose
        ) |>
          leaflet::addLayersControl(
            baseGroups = c("Vantor Vivid", "OpenStreetMap"),
            overlayGroups = c("Vantor Seamlines", "Seamline Polygons (WFS)"),
            options = leaflet::layersControlOptions(collapsed = FALSE)
          )
      }

      m
    })

    # Update map layers when product, service, seamline checkbox, or auth changes
    shiny::observe({
      if (is.function(auth_reactive)) {
        auth_reactive()
      }
      prod <- input$product %||% "VIVID_STANDARD_30"
      srv <- input$service %||% "WMTS"
      show_sl <- isTRUE(input$show_seamlines)

      api_key <- .vantorr_env$api_key
      if (is.null(api_key) || !nzchar(api_key)) {
        env_key <- Sys.getenv("VANTOR_API_KEY", unset = Sys.getenv("MAXAR_API_KEY", unset = ""))
        if (nzchar(env_key)) api_key <- env_key
      }

      if (is.null(api_key) || !nzchar(api_key)) {
        return()
      }

      proxy <- leaflet::leafletProxy("map", session = session) |>
        leaflet::clearGroup("Vantor Vivid") |>
        leaflet::clearGroup("Vantor Seamlines") |>
        add_vantor_basemap(
          product = prod,
          service = srv,
          api_key = api_key,
          group = "Vantor Vivid",
          layerId = "vantor_base",
          show_seamlines = show_sl,
          verbose = FALSE
        )
    })

    # Inspect WFS Seamlines in current viewport
    shiny::observeEvent(input$inspect_wfs, {
      if (!vantor_has_auth(verbose = FALSE)) {
        shiny::showNotification("Please authenticate with Vantor API first.", type = "warning")
        return()
      }
      bb <- current_bbox()
      prod <- input$product %||% "VIVID_STANDARD_30"

      shiny::withProgress(message = "Fetching WFS seamline metadata...", value = 0.4, {
        sl_sf <- vantor_get_seamlines(
          bbox = bb,
          product = prod,
          verbose = verbose
        )
        rv$seamlines <- sl_sf
      })

      if (!is.null(rv$seamlines) && nrow(rv$seamlines) > 0L) {
        popups <- .format_seamline_popup(rv$seamlines)
        leaflet::leafletProxy("map", session = session) |>
          leaflet::clearGroup("Seamline Polygons (WFS)") |>
          leaflet::addPolygons(
            data = rv$seamlines,
            group = "Seamline Polygons (WFS)",
            color = "#00E5FF",
            weight = 2,
            fillOpacity = 0.12,
            popup = popups
          )
        shiny::showNotification(
          sprintf("Loaded %d seamline feature(s). Click polygons on the map to inspect dates.", nrow(rv$seamlines)),
          type = "message"
        )
      } else {
        shiny::showNotification("No WFS seamlines returned for current viewport.", type = "warning")
      }
    })

    output$aoi_summary <- shiny::renderUI({
      bb <- current_bbox()
      bb_3857 <- as.numeric(vantor_as_bbox(bb, target_crs = 3857))
      width_m <- abs(bb_3857[3] - bb_3857[1])
      height_m <- abs(bb_3857[4] - bb_3857[2])
      area_km2 <- (width_m * height_m) / 1e6

      shiny::tags$div(
        style = "font-size:12px; margin-bottom:10px; background:#f8f9fa; padding:8px; border-radius:4px;",
        shiny::tags$strong("Viewport BBox (WGS84):"), shiny::tags$br(),
        sprintf("W: %.4f, S: %.4f", bb["xmin"], bb["ymin"]), shiny::tags$br(),
        sprintf("E: %.4f, N: %.4f", bb["xmax"], bb["ymax"]), shiny::tags$br(),
        shiny::tags$strong("Approx. Area: "), sprintf("%.2f km\u00b2", area_km2)
      )
    })

    output$download_geotiff <- shiny::downloadHandler(
      filename = function() {
        prod <- input$product %||% "VIVID_STANDARD_30"
        sprintf("vantor_%s_%s.tif", tolower(prod), format(Sys.time(), "%Y%m%d_%H%M%S"))
      },
      content = function(file) {
        bb <- current_bbox()
        res_m <- as.numeric(input$resolution %||% 0.5)
        prod <- input$product %||% "VIVID_STANDARD_30"

        bb_3857 <- as.numeric(vantor_as_bbox(bb, target_crs = 3857))
        px_w <- abs(bb_3857[3] - bb_3857[1]) / res_m
        px_h <- abs(bb_3857[4] - bb_3857[2]) / res_m

        shiny::withProgress(message = "Downloading Vantor GeoTIFF...", value = 0.1, {
          if (px_w > 8192 || px_h > 8192) {
            get_maxar_wms_tiles(
              bbox = bb,
              resolution = res_m,
              product = prod,
              final_output = file,
              return_type = "file",
              progress_callback = function(i, n) {
                shiny::incProgress(0.8 / n, detail = sprintf("Tile %d of %d", i, n))
              },
              verbose = verbose
            )
          } else {
            shiny::incProgress(0.3, detail = "Requesting WMS GeoTIFF...")
            get_maxar_wms_basemap(
              bbox = bb,
              resolution = res_m,
              product = prod,
              output_file = file,
              return_type = "file",
              verbose = verbose
            )
          }
          shiny::incProgress(0.9, detail = "Complete")
        })
      }
    )

    list(
      bbox = current_bbox,
      seamlines = shiny::reactive(rv$seamlines),
      product = shiny::reactive(input$product),
      resolution = shiny::reactive(as.numeric(input$resolution))
    )
  })
}


#' Launch Interactive Vantor Explorer Demo Shiny App
#'
#' Launches a standalone Shiny application combining [vantor_auth_ui()] and
#' [vantor_explorer_ui()] for interactively browsing Vantor Vivid basemaps,
#' inspecting seamline acquisition dates, monitoring account usage, and
#' exporting GeoTIFFs.
#'
#' @param initial_bbox Numeric vector of length 4 (`c(xmin, ymin, xmax, ymax)`
#'   in `EPSG:4326`) or any spatial object supported by [vantor_as_bbox()].
#'   Defaults to downtown Denver, CO.
#' @param verbose Logical. Print status messages. Default is
#'   `getOption("vantorr.verbose", TRUE)`.
#'
#' @return A `shiny.appobj` object.
#'
#' @examples
#' if (interactive()) {
#'   run_vantor_demo_app()
#' }
#'
#' @export
run_vantor_demo_app <- function(initial_bbox = c(xmin = -105.01, ymin = 39.73, xmax = -104.98, ymax = 39.75),
                                verbose = getOption("vantorr.verbose", TRUE)) {
  if (!requireNamespace("shiny", quietly = TRUE)) {
    cli::cli_abort("Package {.pkg shiny} is required for {.fn run_vantor_demo_app}. Install it with {.code install.packages(\"shiny\")}.")
  }
  if (!requireNamespace("leaflet", quietly = TRUE)) {
    cli::cli_abort("Package {.pkg leaflet} is required for {.fn run_vantor_demo_app}. Install it with {.code install.packages(\"leaflet\")}.")
  }

  if (verbose) {
    cli::cli_alert_info("Initializing Vantor Interactive Explorer Shiny application...")
  }

  ui <- shiny::fluidPage(
    shiny::titlePanel("vantorr: Vantor Satellite Imagery Explorer"),
    vantor_auth_ui("auth"),
    vantor_explorer_ui("explorer", height = "600px")
  )

  server <- function(input, output, session) {
    auth_state <- vantor_auth_server("auth", verbose = verbose)
    vantor_explorer_server(
      "explorer",
      initial_bbox = initial_bbox,
      auth_reactive = auth_state,
      verbose = verbose
    )
  }

  shiny::shinyApp(ui = ui, server = server)
}
