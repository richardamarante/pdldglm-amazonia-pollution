#' Add a heatmap layer to a map
#'
#' This function adds a heatmap layer to a map. It generates a configuration
#' list that includes the tileset, radius, filter, and pickable options for
#' the heatmap, and sends this configuration to the server to update the map.
#'
#' @param session <`shiny::session`> The Shiny session object.
#' @param map_ID <`character`> A unique identifier for the map input.
#' @param tileset <`character`> The tileset to be used for the heatmap.
#' @param radius <`list`> The definition of radius based on the zoom level,
#' it is a nested list including 'interpolate', 'linear', zoom levels and
#' corresponding radius values. It defaults to a list that specifies the
#' radius at different zoom levels. It is a list defining the radius of influence
#' of data points in pixels. It is structured as an interpolation expression
#' following Mapbox's style specification, and it allows the radius to change
#' according to the zoom level of the map. The default values are set such
#' that the radius is 1 at zoom level 0, 8 at zoom level 10, and 30 at zoom
#' level 15. You can adjust these values according to your needs.
#' @param filter <`list`> A list specifying the filter for heatmap data,
#' defaults to 'all' which includes all data. It  is a list specifying which data
#' to include in the heatmap. By default, it's set to "all", which means all
#' data in the tileset will be included. It can be customized to include specific
#' subsets of the data. For the `crash` data in Montreal, tileset have a `ped`
#' and `cyc` column including a boolean. To only filter pedestrian, this would
#' be the correct argument: `list("==", list("get", "ped"), TRUE)`
#' @param pickable <`logical`> Should there be hovered effect, indicating the
#' layer can be pickable? Defaults to FALSE.
#' @param colours <`character vector`> Vector of length 5, with rgb or rgba
#' values.
#' @param stroke_color <`character`> Color of the stroke around every point.
#' @param min_zoom <`numeric`> When do the points start to appear?
#'
#' @return No return value. The function sends an update message to the Shiny
#' server to update the map.
#'
#' @export
map_heatmap <- function(session, map_ID, tileset,
                        radius = list("interpolate", list("linear"),
                                      list("zoom"), 0, 1, 10, 3, 12, 10, 15, 30),
                        filter = list("all"), pickable = FALSE,
                        colours, stroke_color = "white", min_zoom = 13) {

  # Create an empty configuration list
  configuration <- list()
  configuration$heatmap <- list()

  # Add the fill colour to the configuration list and transfer it to JSON
  configuration$heatmap$tileset <- tileset
  configuration$heatmap$radius <- jsonlite::toJSON(radius, auto_unbox = T)
  configuration$heatmap$filter <- jsonlite::toJSON(filter, auto_unbox = T)
  configuration$heatmap$pickable <- pickable
  configuration$heatmap$colours <- colours
  configuration$heatmap$strokeColor <- stroke_color
  configuration$heatmap$minzoom <- min_zoom

  # Send the configuration list to the server
  update_map(session = session, map_ID = map_ID, configuration = configuration)
}

#' Update the filter of a heatmap layer on a map
#'
#' This function updates the filter of a heatmap layer on a map. It generates a
#' configuration list that includes the new filter for the heatmap data and sends
#' this configuration to the server to update the map.
#'
#' @param session <`shiny::session`> The Shiny session object.
#' @param map_ID <`character`> A unique identifier for the map input.
#' @param filter <`list`> A list specifying the filter for heatmap data,
#' defaults to 'all' which includes all data. It  is a list specifying which data
#' to include in the heatmap. By default, it's set to "all", which means all
#' data in the tileset will be included. It can be customized to include specific
#' subsets of the data. For the `crash` data in Montreal, tileset have a `ped`
#' and `cyc` column including a boolean. To only filter pedestrian, this would
#' be the correct argument: `list("==", list("get", "ped"), TRUE)`
#'
#' @return No return value. The function sends an update message to the Shiny
#' server to update the map.
#'
#' @export
map_heatmap_update_filter <- function(session, map_ID, filter) {

  # Create an empty configuration list
  configuration <- list()
  configuration$heatmap <- list()

  # Change the filter of the heatmap tileset on the map
  configuration$heatmap$filter <- jsonlite::toJSON(filter, auto_unbox = T)

  # Send the configuration list to the server
  update_map(session = session, map_ID = map_ID, configuration = configuration)
}

#' Update the radius of a heatmap layer on a map
#'
#' This function updates the radius of a heatmap layer on a map. It generates a
#' configuration list that includes the new radius for the heatmap data and sends
#' this configuration to the server to update the map.
#'
#' @param session <`shiny::session`> The Shiny session object.
#' @param map_ID <`character`> A unique identifier for the map input.
#' @param radius <`numeric or list`> A numeric value specifying the radius for heatmap data,
#' defining the size of the heatmap's circles. It can be customized to create
#' specific visual effects. Instead, it can also be the  definition of radius
#' based on the zoom level. A nested list including 'interpolate', 'linear', zoom levels and
#' corresponding radius values. It defaults to a list that specifies the
#' radius at different zoom levels. It is a list defining the radius of influence
#' of data points in pixels. It is structured as an interpolation expression
#' following Mapbox's style specification, and it allows the radius to change
#' according to the zoom level of the map. The default values are set such
#' that the radius is 1 at zoom level 0, 8 at zoom level 10, and 30 at zoom
#' level 15. You can adjust these values according to your needs.
#'
#' @return No return value. The function sends an update message to the Shiny
#' server to update the map.
#'
#' @export
map_heatmap_update_radius <- function(session, map_ID, radius) {

  # Create an empty configuration list
  configuration <- list()
  configuration$heatmap <- list()

  # Change the radius of the heatmap tileset on the map
  configuration$heatmap$radius <- jsonlite::toJSON(radius, auto_unbox = T)

  # Send the configuration list to the server
  update_map(session = session, map_ID = map_ID, configuration = configuration)
}


#' Remove a heatmap layer from a map
#'
#' This function removes a heatmap layer from a map. It generates a configuration
#' list that includes a 'remove' command for the heatmap, and sends this
#' configuration to the server to update the map.
#'
#' @param session <`shiny::session`> The Shiny session object.
#' @param map_ID <`character`> A unique identifier for the map input.
#'
#' @return No return value. The function sends an update message to the Shiny
#' server to update the map.
#'
#' @export
map_heatmap_remove <- function(session, map_ID) {

  # Create an empty configuration list
  configuration <- list()
  configuration$heatmap <- list()

  # Send a 'remove' character. This will ensure the removal of the heatmap
  # map.
  configuration$heatmap$tileset <- "remove"

  # Send the configuration list to the server
  update_map(session = session, map_ID = map_ID, configuration = configuration)
}
