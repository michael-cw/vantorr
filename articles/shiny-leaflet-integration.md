# Interactive Leaflet Maps and Shiny Apps with vantorr

## Overview

The `vantorr` package makes it straightforward to stream high-resolution
Vantor / Maxar satellite imagery into interactive `leaflet` web maps and
`shiny` applications, while connecting map interactions directly to R
spatial workflows (`sf`, `terra`, and `stars`).

This vignette covers:

1.  **Authentication for Browser Tile Streaming**: Setting up a Vantor
    API key (`VANTOR_API_KEY`).
2.  **Interactive `leaflet` Maps**: Adding Vantor Vivid mosaics (`WMTS`
    / `WMS`), seamline overlays (`WMS` and interactive `WFS` polygons),
    and archive strips.
3.  **Building a Custom Shiny App**: Writing a custom Shiny app with a
    `leaflet` map that inspects imagery vintage and downloads GeoTIFFs
    for the current viewport.
4.  **Using Drop-in `vantorr` Shiny Modules**: Embedding
    [`vantor_auth_ui()`](https://michael-cw.github.io/vantorr/reference/vantor_auth_ui.md)
    and
    [`vantor_explorer_ui()`](https://michael-cw.github.io/vantorr/reference/vantor_explorer_ui.md)
    into any Shiny dashboard in just a few lines of code.

------------------------------------------------------------------------

## 1. Authentication for Browser Tile Streaming

Vantor Hub endpoints support two authentication mechanisms with distinct
scopes:

- **OGC & Non-Administrative APIs**
  ([`get_maxar_wms_basemap()`](https://michael-cw.github.io/vantorr/reference/get_maxar_wms_basemap.md),
  [`get_maxar_wms_tiles()`](https://michael-cw.github.io/vantorr/reference/get_maxar_wms_tiles.md),
  [`vantor_get_seamlines()`](https://michael-cw.github.io/vantorr/reference/vantor_get_seamlines.md)):
  Work with **either** an **API Key** (`maxar-api-key`) or an **OAuth2
  Bearer Token** (`Authorization: Bearer <token>`).
- **Account Services / Admin APIs**
  ([`vantor_get_usage()`](https://michael-cw.github.io/vantorr/reference/vantor_get_usage.md)):
  Require an **OAuth2 Bearer Token**
  (`vantor_auth(username = "...", password = "...")`), as Vantor’s Admin
  APIs do not accept API keys.
- **Browser-Based `leaflet` Tile Layers**
  ([`add_vantor_basemap()`](https://michael-cw.github.io/vantorr/reference/add_vantor_basemap.md),
  `add_vantor_seamlines(mode = "wms")`,
  [`add_vantor_imagery()`](https://michael-cw.github.io/vantorr/reference/add_vantor_imagery.md)):
  Require an **API Key** (`maxar_api_key` URL query parameter), because
  web browsers rendering `<img>` tile requests cannot attach custom HTTP
  `Authorization: Bearer` headers.

To use `leaflet` tile layers seamlessly, configure your API key in
`.Renviron` (you can also set `VANTOR_USERNAME` and `VANTOR_PASSWORD`
alongside it if you use
[`vantor_get_usage()`](https://michael-cw.github.io/vantorr/reference/vantor_get_usage.md)):

``` r

# In your ~/.Renviron file:
VANTOR_API_KEY="your_vantor_api_key"
```

Or register it at the start of your R session or Shiny app:

``` r

library(vantorr)

# Set a placeholder API key for demonstration
vantor_auth(api_key = "example_api_key", verbose = FALSE)
```

------------------------------------------------------------------------

## 2. Interactive `leaflet` Maps

### Generating OGC WMTS Tile URL Templates

If you want the raw OGC Web Map Tile Service (`WMTS`) URL template
(`EPSG:3857`) for use in `leaflet`, `mapgl`, or `tmap`, use
[`vantor_wmts_url()`](https://michael-cw.github.io/vantorr/reference/vantor_wmts_url.md):

``` r

wmts_template <- vantor_wmts_url(product = "VIVID_STANDARD_30")
wmts_template
#> [1] "https://api.maxar.com/basemaps/v1/ogc/gwc/service/wmts?service=WMTS&request=GetTile&version=1.0.0&layer=Maxar:Imagery&tileMatrixSet=EPSG:3857&tileMatrix=EPSG:3857:{z}&tileCol={x}&tileRow={y}&format=image%2Fjpeg&cql_filter=productName%3D%27VIVID_STANDARD_30%27&maxar_api_key=example_api_key"
```

### Adding Vantor Vivid Basemaps and Seamlines

[`add_vantor_basemap()`](https://michael-cw.github.io/vantorr/reference/add_vantor_basemap.md)
(or its camelCase alias
[`addVantorBasemap()`](https://michael-cw.github.io/vantorr/reference/add_vantor_basemap.md))
adds Vantor Vivid Mosaic imagery (`"VIVID_STANDARD_30"` at 30 cm or
`"VIVID_ADVANCED_15"` at 15 cm) to a `leaflet` or `leafletProxy` widget.
By default it uses pre-cached `WMTS` tiles (`service = "WMTS"`) for fast
panning and zooming, and can optionally overlay seamlines
(`show_seamlines = TRUE`):

``` r

library(leaflet)

map_widget <- leaflet() |>
  setView(lng = -104.995, lat = 39.740, zoom = 15) |>
  add_vantor_basemap(
    product = "VIVID_STANDARD_30",
    service = "WMTS",
    show_seamlines = TRUE,
    group = "Vantor Vivid 30cm",
    verbose = FALSE
  ) |>
  addLayersControl(
    baseGroups = "Vantor Vivid 30cm",
    overlayGroups = "Vantor Seamlines"
  )
```

### Inspecting Seamline Acquisition Dates via WFS (`mode = "wfs"`)

For survey fieldwork and machine learning labeling, knowing the exact
**acquisition date** (`acq_time`), **ground sample distance** (`gsd`),
and **sensor** (`sensor`) of each mosaic strip is critical.

[`add_vantor_seamlines()`](https://michael-cw.github.io/vantorr/reference/add_vantor_seamlines.md)
supports two modes:

- `mode = "wms"` (default): Renders a lightweight transparent raster
  cutline overlay across all zoom levels.
- `mode = "wfs"`: Queries the Vantor OGC WFS endpoint via
  [`vantor_get_seamlines()`](https://michael-cw.github.io/vantorr/reference/vantor_get_seamlines.md)
  for a given `bbox` and adds interactive `sf` polygons with clickable
  HTML metadata popups:

``` r

aoi <- sf::st_bbox(
  c(xmin = -105.01, ymin = 39.73, xmax = -104.98, ymax = 39.75),
  crs = sf::st_crs(4326)
)

leaflet() |>
  fitBounds(-105.01, 39.73, -104.98, 39.75) |>
  add_vantor_basemap(product = "VIVID_STANDARD_30") |>
  add_vantor_seamlines(
    product = "VIVID_STANDARD_30",
    mode = "wfs",
    bbox = aoi
  )
```

### Streaming Specific Archive Strips (`add_vantor_imagery()`)

To stream an individual archive strip by its catalog `feature_id` or a
custom CQL filter (e.g., filtering by `acquisitionDate` and
`cloudCover`), use
[`add_vantor_imagery()`](https://michael-cw.github.io/vantorr/reference/add_vantor_imagery.md):

``` r

archive_map <- leaflet() |>
  setView(lng = -104.995, lat = 39.740, zoom = 15) |>
  add_vantor_imagery(
    feature_id = "10300100EBEDF600",
    group = "Archive Strip",
    verbose = FALSE
  )
```

------------------------------------------------------------------------

## 3. Building a Custom Shiny Application with a Leaflet Map

Below is a complete, minimal Shiny application built from scratch using
`leaflet` and `vantorr`. It allows the user to:

1.  Switch between **Vivid Standard (30 cm)** and **Vivid Advanced (15
    cm)** basemaps.
2.  Toggle the **WMS Seamlines** cutline overlay.
3.  Click **Fetch WFS Seamlines** to load interactive `sf` seamline
    polygons for the current map viewport (`input$map_bounds`) and
    display their metadata table.
4.  Click **Download Viewport GeoTIFF** to export the visible map extent
    via
    [`get_maxar_wms_basemap()`](https://michael-cw.github.io/vantorr/reference/get_maxar_wms_basemap.md).

``` r

library(shiny)
library(leaflet)
library(sf)
library(vantorr)

# Ensure VANTOR_API_KEY is set in .Renviron or call vantor_auth(api_key = "...")

ui <- fluidPage(
  titlePanel("Custom Vantor Leaflet + Shiny App"),
  sidebarLayout(
    sidebarPanel(
      width = 4,
      selectInput(
        "product",
        "Vivid Product:",
        choices = c(
          "Vivid Standard 30 cm" = "VIVID_STANDARD_30",
          "Vivid Advanced 15 cm" = "VIVID_ADVANCED_15"
        )
      ),
      checkboxInput("seamlines", "Show WMS Seamlines", value = FALSE),
      actionButton(
        "load_wfs",
        "Fetch WFS Seamlines in View",
        class = "btn-info btn-block"
      ),
      hr(),
      numericInput("res", "Export Resolution (m):", value = 0.5, min = 0.15, max = 5, step = 0.15),
      downloadButton("download_tif", "Download Viewport GeoTIFF", class = "btn-success btn-block"),
      hr(),
      h5("Seamline Vintage Summary"),
      tableOutput("seamline_table")
    ),
    mainPanel(
      width = 8,
      leafletOutput("map", height = "600px")
    )
  )
)

server <- function(input, output, session) {
  seamlines_rv <- reactiveVal(NULL)

  # Helper reactive: convert Leaflet map bounds to an sf::bbox (EPSG:4326)
  viewport_bbox <- reactive({
    req(input$map_bounds)
    b <- input$map_bounds
    sf::st_bbox(
      c(xmin = b$west, ymin = b$south, xmax = b$east, ymax = b$north),
      crs = sf::st_crs(4326)
    )
  })

  # 1. Initialize the Leaflet map
  output$map <- renderLeaflet({
    leaflet() |>
      addTiles(group = "OpenStreetMap") |>
      setView(lng = -104.995, lat = 39.740, zoom = 15) |>
      add_vantor_basemap(
        product = "VIVID_STANDARD_30",
        group = "Vantor Vivid",
        verbose = FALSE
      ) |>
      addLayersControl(
        baseGroups = c("Vantor Vivid", "OpenStreetMap"),
        overlayGroups = c("Vantor Seamlines", "WFS Polygons"),
        options = layersControlOptions(collapsed = FALSE)
      )
  })

  # 2. Update basemap and WMS seamlines via leafletProxy()
  observe({
    leafletProxy("map", session = session) |>
      clearGroup("Vantor Vivid") |>
      clearGroup("Vantor Seamlines") |>
      add_vantor_basemap(
        product = input$product,
        group = "Vantor Vivid",
        show_seamlines = input$seamlines,
        verbose = FALSE
      )
  })

  # 3. Query WFS vector seamlines for the current viewport on button click
  observeEvent(input$load_wfs, {
    bb <- viewport_bbox()
    withProgress(message = "Fetching WFS seamlines...", value = 0.5, {
      sl <- vantor_get_seamlines(bbox = bb, product = input$product)
      seamlines_rv(sl)
    })

    if (!is.null(seamlines_rv()) && nrow(seamlines_rv()) > 0) {
      leafletProxy("map", session = session) |>
        clearGroup("WFS Polygons") |>
        addPolygons(
          data = seamlines_rv(),
          group = "WFS Polygons",
          color = "#00E5FF",
          weight = 2,
          fillOpacity = 0.15
        )
    }
  })

  # 4. Render summary table of seamline acquisition dates and sensors
  output$seamline_table <- renderTable({
    sl <- seamlines_rv()
    req(sl)
    df <- sf::st_drop_geometry(sl)
    cols <- intersect(c("acq_time", "sensor", "gsd", "catid"), names(df))
    head(df[, cols, drop = FALSE], 10)
  })

  # 5. Download GeoTIFF for the current map viewport
  output$download_tif <- downloadHandler(
    filename = function() {
      paste0("vantor_viewport_", Sys.Date(), ".tif")
    },
    content = function(file) {
      withProgress(message = "Downloading GeoTIFF...", value = 0.4, {
        get_maxar_wms_basemap(
          bbox = viewport_bbox(),
          output_file = file,
          resolution = input$res,
          product = input$product,
          return_type = "file"
        )
      })
    }
  )
}

shinyApp(ui, server)
```

------------------------------------------------------------------------

## 4. Using Drop-In `vantorr` Shiny Modules

To avoid writing boilerplate UI and server code in every application,
`vantorr` provides two pre-built, namespace-safe Shiny modules:

- **`vantor_auth_ui(id)` / `vantor_auth_server(id)`**: Manages API Key
  or OAuth2 login, displays connection status badges, queries account
  credit usage via
  [`vantor_get_usage()`](https://michael-cw.github.io/vantorr/reference/vantor_get_usage.md),
  and automatically refreshes OAuth2 tokens in the background via
  [`shiny::invalidateLater()`](https://rdrr.io/pkg/shiny/man/invalidateLater.html).
- **`vantor_explorer_ui(id)` / `vantor_explorer_server(id)`**: Provides
  the complete interactive `leaflet` map, product/protocol switcher, WFS
  seamline date inspector with HTML popups, live area ($`\text{km}^2`$)
  calculator, and multi-tile GeoTIFF downloader
  ([`get_maxar_wms_tiles()`](https://michael-cw.github.io/vantorr/reference/get_maxar_wms_tiles.md)
  with progress bars).

``` r

library(shiny)
library(vantorr)

ui <- fluidPage(
  titlePanel("Fieldwork AOI Selector"),
  vantor_auth_ui("vantor_login"),
  vantor_explorer_ui("vantor_map", height = "550px"),
  verbatimTextOutput("selected_bbox")
)

server <- function(input, output, session) {
  # 1. Authentication module (auto-refreshes OAuth2 tokens)
  auth_state <- vantor_auth_server("vantor_login")

  # 2. Explorer module (returns reactive bbox, seamlines, product, and resolution)
  explorer <- vantor_explorer_server(
    "vantor_map",
    auth_reactive = auth_state
  )

  # 3. Use the reactive bounding box in your own downstream Shiny logic
  output$selected_bbox <- renderPrint({
    explorer$bbox()
  })
}

shinyApp(ui, server)
```

Or launch the built-in standalone demonstration app directly from an
interactive R session with a single function call:

``` r

if (interactive()) {
  run_vantor_demo_app()
}
```
