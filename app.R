# =========================================================
# UGANDA LIBRARIES EXPLORER
# =========================================================


# ---------------------------------------------------------
# Libraries
# ---------------------------------------------------------

library(shiny)
library(bslib)
library(dplyr)
library(readr)
library(leaflet)
library(sf)
library(bsicons)
library(htmltools)


# =========================================================
# LOAD LIBRARY DATA
# =========================================================

libraries <- read_csv(
  "data/processed/libraries.csv",
  show_col_types = FALSE
)


# =========================================================
# LOAD ADMINISTRATIVE BOUNDARIES
# =========================================================

regions <- st_read(
  "data/processed/regions.geojson",
  quiet = TRUE
)

districts <- st_read(
  "data/processed/districts.geojson",
  quiet = TRUE
)


# =========================================================
# PREPARE REGIONS
# =========================================================

regions <- regions %>%
  select(
    region_name = adm1_name,
    geometry
  ) %>%
  mutate(
    region_name = trimws(region_name)
  ) %>%
  st_make_valid()


# =========================================================
# PREPARE DISTRICTS
# =========================================================

districts <- districts %>%
  select(
    district_name = d,
    geometry
  ) %>%
  mutate(
    district_name = trimws(district_name)
  ) %>%
  st_make_valid()


# =========================================================
# ASSIGN DISTRICTS TO REGIONS
# =========================================================

districts <- st_join(
  districts,
  regions %>%
    select(region_name),
  join = st_intersects,
  largest = TRUE
)


# =========================================================
# CREATE SIMPLIFIED MAP GEOMETRIES
# =========================================================
#
# Original geometries are retained for spatial analysis.
# Simplified geometries are used only for Leaflet display.
#
# =========================================================

regions_map <- regions %>%
  st_transform(3857) %>%
  st_simplify(
    dTolerance = 500,
    preserveTopology = TRUE
  ) %>%
  st_transform(4326)


districts_map <- districts %>%
  st_transform(3857) %>%
  st_simplify(
    dTolerance = 500,
    preserveTopology = TRUE
  ) %>%
  st_transform(4326)


# =========================================================
# PREPARE LIBRARY DATA
# =========================================================

libraries <- libraries %>%
  mutate(
    Latitude = as.numeric(Latitude),
    Longitude = as.numeric(Longitude),
    District = trimws(District),
    `Type of Library` = trimws(`Type of Library`)
  ) %>%
  filter(
    !is.na(Latitude),
    !is.na(Longitude)
  )


# =========================================================
# CONVERT LIBRARIES TO SF POINTS
# =========================================================

library_points <- st_as_sf(
  libraries,
  coords = c("Longitude", "Latitude"),
  crs = 4326,
  remove = FALSE
)


# =========================================================
# ASSIGN LIBRARIES TO REGIONS
# =========================================================

library_points <- st_join(
  library_points,
  regions %>%
    select(region_name),
  join = st_within,
  left = TRUE
)


# =========================================================
# ASSIGN LIBRARIES TO DISTRICTS
# =========================================================

library_points <- st_join(
  library_points,
  districts %>%
    select(district_name),
  join = st_within,
  left = TRUE
)


# =========================================================
# CREATE LIBRARY POPUP CONTENT
# =========================================================

library_points <- library_points %>%
  mutate(
    popup_html = paste0(
      "<strong>",
      htmlEscape(
        coalesce(
          `Name of the library`,
          "Unnamed library"
        )
      ),
      "</strong><br><br>",

      "<strong>Region:</strong> ",
      htmlEscape(
        coalesce(
          region_name,
          "Not available"
        )
      ),
      "<br>",

      "<strong>District:</strong> ",
      htmlEscape(
        coalesce(
          district_name,
          District,
          "Not available"
        )
      ),
      "<br>",

      "<strong>Village/Cell:</strong> ",
      htmlEscape(
        coalesce(
          `Name of the Village/Cell`,
          "Not available"
        )
      ),
      "<br>",

      "<strong>Type:</strong> ",
      htmlEscape(
        coalesce(
          `Type of Library`,
          "Not available"
        )
      )
    )
  )


# =========================================================
# REGION LIST
# =========================================================

region_list <- regions %>%
  st_drop_geometry() %>%
  distinct(region_name) %>%
  arrange(region_name) %>%
  pull(region_name)


# =========================================================
# DISTRICT LIST
# =========================================================

district_list <- districts %>%
  st_drop_geometry() %>%
  distinct(district_name) %>%
  filter(
    !is.na(district_name),
    district_name != ""
  ) %>%
  arrange(district_name) %>%
  pull(district_name)


# =========================================================
# USER INTERFACE
# =========================================================

ui <- tagList(

  # =======================================================
  # CUSTOM CSS
  # =======================================================

  tags$head(

    tags$style(HTML("

      /* =================================================
         COMPACT TOP NAVIGATION
         ================================================= */

      .navbar {
        padding-top: 0.25rem !important;
        padding-bottom: 0.25rem !important;
        min-height: 48px !important;
      }

      .navbar-brand {
        padding-top: 0.25rem !important;
        padding-bottom: 0.25rem !important;
        margin-right: 1rem !important;
        font-size: 1rem !important;
      }

      .navbar-nav .nav-link {
        padding-top: 0.35rem !important;
        padding-bottom: 0.35rem !important;
      }

      .navbar-nav .nav-link.active {
        padding-top: 0.35rem !important;
        padding-bottom: 0.35rem !important;
      }


      /* =================================================
         DASHBOARD CONTENT
         ================================================= */

      .dashboard-content {
        display: flex;
        flex-direction: column;
        gap: 12px;
        width: 100%;
      }


      /* =================================================
         STATISTICS ROW
         ================================================= */

      .stats-row {
        display: grid;
        grid-template-columns: repeat(4, minmax(0, 1fr));
        gap: 12px;
        width: 100%;
      }


      /* =================================================
         COMPACT STATISTICS CARDS
         ================================================= */

      .stats-row .bslib-value-box {
        height: 105px !important;
        min-height: 105px !important;
        max-height: 105px !important;
        margin: 0 !important;
      }

      .stats-row .bslib-value-box .value-box-area {
        padding: 0.6rem 1rem !important;
      }

      .stats-row .bslib-value-box .value-box-title {
        font-size: 0.85rem !important;
        margin-bottom: 0.1rem !important;
      }

      .stats-row .bslib-value-box .value-box-value {
        font-size: 1.8rem !important;
      }

      .stats-row .bslib-value-box .value-box-showcase {
        max-height: 50px !important;
      }


      /* =================================================
         MAP
         ================================================= */

      .leaflet-container {
        background: #f5f5f5;
      }


      /* =================================================
         RESPONSIVE LAYOUT
         ================================================= */

      @media (max-width: 1000px) {

        .stats-row {
          grid-template-columns: repeat(
            2,
            minmax(0, 1fr)
          );
        }

      }


      @media (max-width: 600px) {

        .stats-row {
          grid-template-columns: 1fr;
        }

      }

    "))

  ),


  # =======================================================
  # NAVIGATION BAR
  # =======================================================

  page_navbar(

    title = "Uganda Libraries Explorer",

    theme = bs_theme(
      version = 5,
      bootswatch = "flatly"
    ),


    # =====================================================
    # OVERVIEW
    # =====================================================

    nav_panel(

      "Overview",


      # ===================================================
      # SIDEBAR
      # ===================================================

      layout_sidebar(

        sidebar = sidebar(

          title = "Explore",

          width = "280px",


          # ------------------------------------------------
          # REGION SELECTOR
          # ------------------------------------------------

          selectInput(

            inputId = "region",

            label = "Select a region",

            choices = c(
              "All Regions",
              region_list
            ),

            selected = "All Regions",

            width = "100%"
          ),


          # ------------------------------------------------
          # DISTRICT SELECTOR
          # ------------------------------------------------

          selectInput(

            inputId = "district",

            label = "Select a district",

            choices = c(
              "All Districts",
              district_list
            ),

            selected = "All Districts",

            width = "100%"
          ),


          hr(),


          # ------------------------------------------------
          # MAP LEGEND
          # ------------------------------------------------

          strong("Map legend"),

          br(),
          br(),


          # Region boundary

          tags$div(

            style = paste0(
              "display:flex;",
              "align-items:center;",
              "margin-bottom:10px;"
            ),

            tags$span(

              style = paste0(
                "display:inline-block;",
                "width:28px;",
                "height:0;",
                "border-top:3px solid #333;",
                "margin-right:8px;"
              )

            ),

            "Region boundary"
          ),


          # District boundary

          tags$div(

            style = paste0(
              "display:flex;",
              "align-items:center;",
              "margin-bottom:15px;"
            ),

            tags$span(

              style = paste0(
                "display:inline-block;",
                "width:28px;",
                "height:0;",
                "border-top:1px dashed #777;",
                "margin-right:8px;"
              )

            ),

            "District boundary"
          ),


          # Community library

          tags$div(

            style = paste0(
              "display:flex;",
              "align-items:center;",
              "margin-bottom:10px;"
            ),

            tags$span(

              style = paste0(
                "display:inline-block;",
                "width:14px;",
                "height:14px;",
                "border-radius:50%;",
                "background:#4CAF50;",
                "border:1px solid #2E7D32;",
                "margin-right:8px;"
              )

            ),

            "Community Library"
          ),


          # Public library

          tags$div(

            style = paste0(
              "display:flex;",
              "align-items:center;"
            ),

            tags$span(

              style = paste0(
                "display:inline-block;",
                "width:14px;",
                "height:14px;",
                "border-radius:50%;",
                "background:#2196F3;",
                "border:1px solid #1565C0;",
                "margin-right:8px;"
              )

            ),

            "Public Library"
          )
        ),


        # =================================================
        # MAIN DASHBOARD CONTENT
        # =================================================

        div(

          class = "dashboard-content",


          # =================================================
          # STATISTICS ROW
          # =================================================

          div(

            class = "stats-row",


            # ------------------------------------------------
            # TOTAL LIBRARIES
            # ------------------------------------------------

            value_box(

              title = "Total Libraries",

              value = uiOutput(
                "total_libraries_value"
              ),

              showcase = bs_icon(
                "building"
              )
            ),


            # ------------------------------------------------
            # COMMUNITY LIBRARIES
            # ------------------------------------------------

            value_box(

              title = "Community Libraries",

              value = uiOutput(
                "community_libraries_value"
              ),

              showcase = bs_icon(
                "people"
              )
            ),


            # ------------------------------------------------
            # PUBLIC LIBRARIES
            # ------------------------------------------------

            value_box(

              title = "Public Libraries",

              value = uiOutput(
                "public_libraries_value"
              ),

              showcase = bs_icon(
                "building"
              )
            ),


            # ------------------------------------------------
            # DISTRICTS
            # ------------------------------------------------

            value_box(

              title = "Districts",

              value = uiOutput(
                "districts_value"
              ),

              showcase = bs_icon(
                "geo-alt"
              )
            )
          ),


          # =================================================
          # MAP CARD
          # =================================================

          card(

            full_screen = TRUE,

            card_header(

              textOutput(
                "map_title",
                inline = TRUE
              )
            ),

            leafletOutput(
              "library_map",
              height = "700px"
            )
          )
        )
      )
    )
  )
)


# =========================================================
# SERVER
# =========================================================

server <- function(
  input,
  output,
  session
) {


  # =======================================================
  # FILTERED LIBRARIES
  # =======================================================

  filtered_libraries <- reactive({

    data <- library_points


    # -----------------------------------------------------
    # REGION FILTER
    # -----------------------------------------------------

    if (

      !is.null(input$region) &&

      input$region != "All Regions"

    ) {

      data <- data %>%
        filter(
          region_name == input$region
        )

    }


    # -----------------------------------------------------
    # DISTRICT FILTER
    # -----------------------------------------------------

    if (

      !is.null(input$district) &&

      input$district != "All Districts"

    ) {

      data <- data %>%
        filter(
          district_name == input$district
        )

    }


    data

  })


  # =======================================================
  # UPDATE DISTRICT SELECTOR
  # =======================================================

  observeEvent(

    input$region,

    {

      if (

        is.null(input$region) ||

        input$region == "All Regions"

      ) {

        available_districts <-
          district_list

      } else {

        available_districts <-

          districts %>%

          st_drop_geometry() %>%

          filter(
            region_name == input$region
          ) %>%

          distinct(
            district_name
          ) %>%

          filter(
            !is.na(district_name),
            district_name != ""
          ) %>%

          arrange(
            district_name
          ) %>%

          pull(
            district_name
          )

      }


      updateSelectInput(

        session,

        "district",

        choices = c(
          "All Districts",
          available_districts
        ),

        selected = "All Districts"

      )

    },

    ignoreInit = FALSE

  )


  # =======================================================
  # TOTAL LIBRARIES
  # =======================================================

  output$total_libraries_value <- renderUI({

    tags$span(
      nrow(
        filtered_libraries()
      )
    )

  })


  # =======================================================
  # COMMUNITY LIBRARIES
  # =======================================================

  output$community_libraries_value <- renderUI({

    data <- filtered_libraries()

    count <- sum(

      data$`Type of Library` ==
        "Community Library",

      na.rm = TRUE

    )

    tags$span(count)

  })


  # =======================================================
  # PUBLIC LIBRARIES
  # =======================================================

  output$public_libraries_value <- renderUI({

    data <- filtered_libraries()

    count <- sum(

      data$`Type of Library` ==
        "Public Library",

      na.rm = TRUE

    )

    tags$span(count)

  })


  # =======================================================
  # DISTRICTS
  # =======================================================
  #
  # This represents administrative districts in the
  # selected geographic area, NOT districts containing
  # libraries.
  #
  # Uganda   -> 136
  # Northern -> 38
  # Kaabong  -> 1
  #
  # =======================================================

  output$districts_value <- renderUI({

    # -----------------------------------------------------
    # SPECIFIC DISTRICT
    # -----------------------------------------------------

    if (

      !is.null(input$district) &&

      input$district != "All Districts"

    ) {

      tags$span(1)


    # -----------------------------------------------------
    # SPECIFIC REGION
    # -----------------------------------------------------

    } else if (

      !is.null(input$region) &&

      input$region != "All Regions"

    ) {

      count <-

        districts %>%

        st_drop_geometry() %>%

        filter(
          region_name == input$region
        ) %>%

        distinct(
          district_name
        ) %>%

        filter(
          !is.na(district_name),
          district_name != ""
        ) %>%

        nrow()


      tags$span(count)


    # -----------------------------------------------------
    # ALL UGANDA
    # -----------------------------------------------------

    } else {

      count <-

        districts %>%

        st_drop_geometry() %>%

        distinct(
          district_name
        ) %>%

        filter(
          !is.na(district_name),
          district_name != ""
        ) %>%

        nrow()


      tags$span(count)

    }

  })


  # =======================================================
  # MAP TITLE
  # =======================================================

  output$map_title <- renderText({

    if (

      !is.null(input$district) &&

      input$district != "All Districts"

    ) {

      paste(
        "Libraries in",
        input$district
      )


    } else if (

      !is.null(input$region) &&

      input$region != "All Regions"

    ) {

      paste(
        "Libraries in",
        input$region,
        "Region"
      )


    } else {

      "Libraries across Uganda"

    }

  })


  # =======================================================
  # INITIAL MAP
  # =======================================================

  output$library_map <- renderLeaflet({

    leaflet() %>%

      addTiles(
        group = "OpenStreetMap"
      ) %>%

      setView(
        lng = 32.5,
        lat = 1.37,
        zoom = 6
      )

  })


  # =======================================================
  # UPDATE MAP
  # =======================================================

  observe({

    data <- filtered_libraries()

    proxy <- leafletProxy(
      "library_map"
    )


    # =====================================================
    # CLEAR PREVIOUS CONTENT
    # =====================================================

    proxy %>%

      clearGroup("Regions") %>%

      clearGroup("Districts") %>%

      clearGroup("Selected Region") %>%

      clearGroup("Selected District") %>%

      clearGroup("Community Libraries") %>%

      clearGroup("Public Libraries")


    # =====================================================
    # REGION BOUNDARIES
    # =====================================================

    proxy %>%

      addPolygons(

        data = regions_map,

        group = "Regions",

        layerId = ~region_name,

        fill = FALSE,

        color = "#333333",

        weight = 3,

        opacity = 0.8,

        popup = ~paste0(

          "<strong>",

          htmlEscape(region_name),

          " Region</strong>"
        ),

        highlightOptions = highlightOptions(

          weight = 4,

          color = "#000000",

          bringToFront = TRUE
        )
      )


    # =====================================================
    # DISTRICT BOUNDARIES
    # =====================================================

    proxy %>%

      addPolygons(

        data = districts_map,

        group = "Districts",

        layerId = ~paste0(
          "district_",
          district_name
        ),

        fill = TRUE,

        fillColor = "#FFFFFF",

        fillOpacity = 0.05,

        color = "#777777",

        weight = 1,

        opacity = 0.7,

        dashArray = "4",

        popup = ~paste0(

          "<strong>",

          htmlEscape(
            district_name
          ),

          "</strong><br>",

          "Region: ",

          htmlEscape(
            region_name
          )
        ),

        highlightOptions = highlightOptions(

          weight = 3,

          color = "#555555",

          fillOpacity = 0.15,

          bringToFront = TRUE
        )
      )


    # =====================================================
    # SELECTED REGION
    # =====================================================

    if (

      !is.null(input$region) &&

      input$region != "All Regions"

    ) {

      selected_region <-

        regions_map %>%

        filter(
          region_name == input$region
        )


      if (
        nrow(selected_region) > 0
      ) {

        proxy %>%

          addPolygons(

            data = selected_region,

            group = "Selected Region",

            fill = TRUE,

            fillColor = "#FFD54F",

            fillOpacity = 0.08,

            color = "#E65100",

            weight = 4,

            opacity = 0.9
          )

      }

    }


    # =====================================================
    # SELECTED DISTRICT
    # =====================================================

    if (

      !is.null(input$district) &&

      input$district != "All Districts"

    ) {

      selected_district <-

        districts_map %>%

        filter(
          district_name == input$district
        )


      if (
        nrow(selected_district) > 0
      ) {

        proxy %>%

          addPolygons(

            data = selected_district,

            group = "Selected District",

            fill = TRUE,

            fillColor = "#FF9800",

            fillOpacity = 0.20,

            color = "#E65100",

            weight = 4,

            opacity = 1
          )

      }

    }


    # =====================================================
    # COMMUNITY LIBRARIES
    # =====================================================

    community <-

      data %>%

      filter(
        `Type of Library` ==
          "Community Library"
      )


    if (
      nrow(community) > 0
    ) {

      proxy %>%

        addCircleMarkers(

          data = community,

          lng = ~Longitude,

          lat = ~Latitude,

          radius = 7,

          stroke = TRUE,

          weight = 1,

          color = "#2E7D32",

          fillColor = "#4CAF50",

          fillOpacity = 0.85,

          popup = ~popup_html,

          group = "Community Libraries",

          clusterOptions =
            markerClusterOptions(

              showCoverageOnHover =
                FALSE,

              zoomToBoundsOnClick =
                TRUE,

              spiderfyOnMaxZoom =
                TRUE
            )
        )

    }


    # =====================================================
    # PUBLIC LIBRARIES
    # =====================================================

    public <-

      data %>%

      filter(
        `Type of Library` ==
          "Public Library"
      )


    if (
      nrow(public) > 0
    ) {

      proxy %>%

        addCircleMarkers(

          data = public,

          lng = ~Longitude,

          lat = ~Latitude,

          radius = 8,

          stroke = TRUE,

          weight = 1,

          color = "#1565C0",

          fillColor = "#2196F3",

          fillOpacity = 0.85,

          popup = ~popup_html,

          group = "Public Libraries",

          clusterOptions =
            markerClusterOptions(

              showCoverageOnHover =
                FALSE,

              zoomToBoundsOnClick =
                TRUE,

              spiderfyOnMaxZoom =
                TRUE
            )
        )

    }


    # =====================================================
    # LAYER CONTROL
    # =====================================================

    proxy %>%

      addLayersControl(

        overlayGroups = c(
          "Regions",
          "Districts",
          "Community Libraries",
          "Public Libraries"
        ),

        options =
          layersControlOptions(
            collapsed = FALSE
          )
      )


    # =====================================================
    # MAP VIEW
    # =====================================================

    if (

      !is.null(input$district) &&

      input$district != "All Districts"

    ) {

      selected_polygon <-

        districts %>%

        filter(
          district_name == input$district
        )


      if (
        nrow(selected_polygon) > 0
      ) {

        bbox <- st_bbox(
          selected_polygon
        )


        proxy %>%

          fitBounds(

            lng1 = as.numeric(
              bbox["xmin"]
            ),

            lat1 = as.numeric(
              bbox["ymin"]
            ),

            lng2 = as.numeric(
              bbox["xmax"]
            ),

            lat2 = as.numeric(
              bbox["ymax"]
            )
          )

      }


    } else if (

      !is.null(input$region) &&

      input$region != "All Regions"

    ) {

      selected_region <-

        regions %>%

        filter(
          region_name == input$region
        )


      if (
        nrow(selected_region) > 0
      ) {

        bbox <- st_bbox(
          selected_region
        )


        proxy %>%

          fitBounds(

            lng1 = as.numeric(
              bbox["xmin"]
            ),

            lat1 = as.numeric(
              bbox["ymin"]
            ),

            lng2 = as.numeric(
              bbox["xmax"]
            ),

            lat2 = as.numeric(
              bbox["ymax"]
            )
          )

      }


    } else {

      proxy %>%

        setView(

          lng = 32.5,

          lat = 1.37,

          zoom = 6
        )

    }

  })


  # =======================================================
  # MAP POLYGON CLICK
  # =======================================================

  observeEvent(

    input$library_map_shape_click,

    {

      click <-
        input$library_map_shape_click


      if (
        is.null(click$id)
      ) {

        return()

      }


      # ---------------------------------------------------
      # REGION CLICK
      # ---------------------------------------------------

      if (

        click$id %in%
        region_list

      ) {

        updateSelectInput(

          session,

          "region",

          selected = click$id

        )

        return()

      }


      # ---------------------------------------------------
      # DISTRICT CLICK
      # ---------------------------------------------------

      if (

        startsWith(
          click$id,
          "district_"
        )

      ) {

        clicked_district <-

          sub(
            "^district_",
            "",
            click$id
          )


        clicked_region <-

          districts %>%

          st_drop_geometry() %>%

          filter(
            district_name ==
              clicked_district
          ) %>%

          pull(
            region_name
          ) %>%

          first()


        if (
          !is.na(clicked_region)
        ) {

          updateSelectInput(

            session,

            "region",

            selected =
              clicked_region

          )


          updateSelectInput(

            session,

            "district",

            selected =
              clicked_district

          )

        }

      }

    }

  )

}


# =========================================================
# RUN APPLICATION
# =========================================================

shinyApp(
  ui = ui,
  server = server
)
