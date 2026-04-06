#' Update a map's language
#'
#' This function updates a map's language, generally used for the `stories` preview
#' on hover. It generates a configuration list that includes the new language
#' and sends this configuration to the server to update the map.
#'
#' @param session <`shiny::session`> The Shiny session object.
#' @param map_ID <`character`> A unique identifier for the map input.
#' @param lang <`character`> The new language for the map.
#'
#' @return No return value. The function sends an update message to the Shiny
#' server to update the map.
#'
#' @export
map_update_lang  <- function(session, map_ID, lang) {

  # Create an empty configuration list
  configuration <- list()

  # Update language
  configuration$lang <- lang

  # Send the configuration list to the server
  update_map(session = session, map_ID = map_ID, configuration = configuration)
}
