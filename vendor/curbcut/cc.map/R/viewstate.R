#' Update a map's view state
#'
#' This function updates a map's view state, including the longitude, latitude, and
#' zoom level. It generates a configuration list that includes the new view state
#' and a timestamp to ensure changes are recognized each time it's updated. This
#' configuration is sent to the server to update the map.
#'
#' @param session <`shiny::session`> The Shiny session object.
#' @param map_ID <`character`> A unique identifier for the map input.
#' @param longitude <`numeric`> The new longitude for the map's center.
#' @param latitude <`numeric`> The new latitude for the map's center.
#' @param zoom <`numeric`> The new zoom level for the map.
#'
#' @return No return value. The function sends an update message to the Shiny
#' server to update the map.
#'
#' @export
map_viewstate <- function(session, map_ID, longitude, latitude, zoom) {

  # Create an empty configuration list
  configuration <- list()
  configuration$viewstate <- list()

  # Update longitude, latitude and zoom of the map. Add a timestamp to force
  # re-triggers everytime, even though the arguments are the same.
  configuration$viewstate$longitude <- longitude
  configuration$viewstate$latitude <- latitude
  configuration$viewstate$zoom <- zoom
  configuration$viewstate$timestamp <- Sys.time()

  # Send the configuration list to the server
  update_map(session = session, map_ID = map_ID, configuration = configuration)
}
