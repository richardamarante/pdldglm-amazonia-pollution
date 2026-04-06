# library(cc.map)
library(shiny)
devtools::load_all()

# Fill data ---------------------------------------------------------------

fill_colour <- qs::qread(system.file("data_colour.qs", package = "cc.map"))
names(fill_colour)[1] <- "ID_color"
stories <- qs::qread(system.file("stories.qs", package = "cc.map"))
stories <- tibble::as_tibble(stories)

# UI / server -------------------------------------------------------------

map_js_UI <- function(id) {
  shiny::tagList(
    actionButton(shiny::NS(id, "button"), "new tileset + color"),
    actionButton(shiny::NS(id, "button1"), "new tileset + color + selection"),
    actionButton(shiny::NS(id, "button2"), "just change color"),
    actionButton(shiny::NS(id, "button3"), "add a selection"),
    actionButton(shiny::NS(id, "button4"), "change viewstate"),
    actionButton(shiny::NS(id, "button5"), "remove choropleth"),
    actionButton(shiny::NS(id, "button6"), "add heatmap"),
    actionButton(shiny::NS(id, "button7"), "change heatmap filter"),
    actionButton(shiny::NS(id, "button8"), "Remove heatmap layer"),
    shiny::uiOutput(outputId = shiny::NS(id, "map_ph"))
  )
}

map_js_server <- function(id) {

  shiny::moduleServer(id, function(input, output, session) {

    output$map_ph <- shiny::renderUI({
      cc.map::map_input(
        shiny::NS(id, "map"),
        username = "curbcut",
        token = 'pk.eyJ1IjoiY3VyYmN1dCIsImEiOiJjbGprYnVwOTQwaDAzM2xwaWdjbTB6bzdlIn0.Ks1cOI6v2i8jiIjk38s_kg',
        map_style_id = "mapbox://styles/curbcut/cljkciic3002h01qveq5z1wrp",
        longitude = -73.5,
        latitude = 45.5,
        zoom = 9,
        inst_prefix  = "mtl",
        stories = stories,
        stories_min_zoom = 13)
    })
    map_choropleth(session = session, map_ID = "map",
                   tileset = "mtl_CMA_auto_zoom",
                   fill_colour = fill_colour,
                   select_id = "2466023_4")


    # Observe change in viewstate. ignore NULL as it gets triggered everytime an
    # output is sent from the map to shiny
    shiny::observeEvent(get_map_viewstate("map"), {
      print(get_map_viewstate("map"))
    }, ignoreNULL = TRUE)

    # Observe click
    shiny::observeEvent(get_map_click("map"), {
      print(get_map_click("map"))
    }, ignoreNULL = TRUE)

    # Add a tileset to the map with fill colours
    observeEvent(input$button, {
      map_choropleth(session = session, map_ID = "map",
                     tileset = "mtl_CMA_auto_zoom",
                     fill_colour = fill_colour)
    })
    # Add a tileset to the map with fill colours
    observeEvent(input$button1, {
      map_choropleth(session = session, map_ID = "map",
                     tileset = "mtl_CMA_auto_zoom",
                     fill_colour = fill_colour,
                     select_id = "2466023_4")
    })

    # Just update fill colours
    observeEvent(input$button2, {
      fl_c <- fill_colour
      fl_c$fill <- sample(c("#C85A5A", "#E4ACAC", "#E8E8E8", "#B0D5DF", "#64ACBE"),
                          nrow(fl_c), replace = TRUE)

      map_choropleth_update_fill_colour(session = session, map_ID = "map",
                                        fill_colour = fl_c)
    })

    # Select a random census tract ID (click on first button first to get a CT tileset)
    observeEvent(input$button3, {
      map_choropleth_update_selection(session = session, map_ID = "map",
                                      select_id = "2466023_4")#sample(fill_colour$ID[120:500], 1))
    })

    # Update the viewstate
    observeEvent(input$button4, {
      map_viewstate(session = session,
                    map_ID = "map",
                    longitude = -73.5172,
                    latitude = 45.5613,
                    zoom = 15)
    })

    # Remove the choropleth
    observeEvent(input$button5, {
      map_choropleth_remove(session = session,
                            map_ID = "map")
    })

    # Add a heatmap layer
    observeEvent(input$button6, {
      map_heatmap(session = session,
                  map_ID = "map",
                  tileset = "mtl_crash_2021")
    })
    # Change filter
    observeEvent(input$button7, {
      map_heatmap_update_filter(session = session,
                                map_ID = "map",
                                filter = list("==", list("get", "ped"), TRUE))
    })
    # Remove heatmap
    observeEvent(input$button8, {
      map_heatmap_remove(session = session,
                         map_ID = "map")
    })

  })
}


ui <- fluidPage(
  theme = bslib::bs_theme(version = "4"),
  titlePanel("reactR mapbox-gl"),
  map_js_UI(id = "test_module")
)

server <- function(input, output, session) {

  map_js_server(id = "test_module")

}

shinyApp(ui, server)




# UI / server -------------------------------------------------------------

map_js_UI <- function(id) {
  shiny::tagList(
    shiny::uiOutput(outputId = shiny::NS(id, "map_ph"))
  )
}

map_js_server <- function(id) {

  shiny::moduleServer(id, function(input, output, session) {

    output$map_ph <- shiny::renderUI({
      cc.map::map_input(
        shiny::NS(id, "map"),
        username = "curbcut",
        token = 'pk.eyJ1IjoiY3VyYmN1dCIsImEiOiJjbGprYnVwOTQwaDAzM2xwaWdjbTB6bzdlIn0.Ks1cOI6v2i8jiIjk38s_kg',
        map_style_id = "mapbox://styles/curbcut/cljkciic3002h01qveq5z1wrp",
        longitude = -73.5,
        latitude = 45.5,
        zoom = 9,
        inst_prefix  = "mtl",
        stories_min_zoom = 13)
    })
    map_heatmap(session = session, map_ID = "map",
                tileset = "mtl_syntheco",
                colours = c("rgba(179, 179, 187, 0)", "rgb(196, 205, 225)", "rgb(152, 168, 203)",
                            "rgb(108, 131, 181)", "rgb(76, 92, 127)"),
                stroke_color = "transparent", min_zoom = 16)

  })
}


ui <- fluidPage(
  theme = bslib::bs_theme(version = "4"),
  titlePanel("reactR mapbox-gl"),
  map_js_UI(id = "test_module")
)

server <- function(input, output, session) {

  map_js_server(id = "test_module")

}

shinyApp(ui, server)

