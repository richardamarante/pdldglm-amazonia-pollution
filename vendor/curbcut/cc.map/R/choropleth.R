#' Update a map with a choropleth overlay
#'
#' This function updates a map with a choropleth overlay. It generates a
#' configuration list that includes the tileset and fill colour for the choropleth
#' overlay and sends this configuration to the server to update the map.
#'
#' @param session <`shiny::session`> The Shiny session object.
#' @param map_ID <`character`> A unique identifier for the map input.
#' @param tileset <`character`> The tileset to be used for the choropleth overlay.
#' @param fill_colour <`data.frame`> A tibble with two columns: 'ID_color' and 'fill'. ID is
#' the ID of the feature, and fill are hexes of 6 digits.
#' @param outline_width <`numeric`> Outline width of the fill features. Default
#' to 1.
#' @param outline_color <`numeric`> Outline color of the fill features. Defaults
#' to `"transparent"`. It can be a color or a JSON object representing the line-color
#' mapping.
#' @param pickable <`logical`> Should there be hovered effect, indicating the layer
#' can be pickable? Defaults to TRUE.
#' @param select_id <`character`> The selected ID that should be highlighted on
#' the map. Defaults to none, NA.
#' @param fill_fun <`function`> A function to generate the fill-color configuration.
#' Defaults to \code{\link{map_choropleth_fill_fun}}. It needs to return the JSON
#' that will be fed to `fill-color` paint argument of the mapbox choropleth.
#' @param fill_fun_args <`list`> A list of arguments to pass to the fill_fun function.
#' Defaults to a list with \code{df}, \code{get_col}, and \code{fallback}.
#'
#' @return No return value. The function sends an update message to the Shiny
#' server to update the map.
#'
#' @export
map_choropleth <- function(session, map_ID, tileset, fill_colour, outline_width = 1,
                           outline_color = "transparent",
                           pickable = TRUE, select_id = NA,
                           fill_fun = map_choropleth_fill_fun,
                           fill_fun_args = list(df = fill_colour,
                                                get_col = names(fill_colour)[1],
                                                fallback = "transparent")) {

  # Create an empty configuration list
  configuration <- list()
  configuration$choropleth <- list()

  # Add the fill colour to the configuration list and transfer it to JSON
  configuration$choropleth$fill_colour <- do.call(fill_fun, fill_fun_args)

  # Update the outline width of the fill
  configuration$choropleth$outline_width <- outline_width

  # Update the outline color of the fill
  configuration$choropleth$outline_color <- outline_color

  # Add the tileset to the configuration list
  configuration$choropleth$tileset <- tileset

  # Add the pickable to the configuration list
  configuration$choropleth$pickable <- pickable

  # Add a selection if it's not NA
  if (!is.null(select_id)) {
    if (!is.na(select_id)) {
      configuration$choropleth$select_id <- select_id
    }
  }

  # Send the configuration list to the server
  update_map(session = session, map_ID = map_ID, configuration = configuration)
}

#' Update a map's choropleth overlay fill colour
#'
#' This function updates a map's choropleth overlay fill colour. It generates a
#' configuration list that includes the fill colour for the choropleth overlay
#' and sends this configuration to the server to update the map.
#'
#' @param session <`shiny::session`> The Shiny session object.
#' @param map_ID <`character`> A unique identifier for the map input.
#' @param fill_colour <`data.frame`> A tibble with two columns: 'ID' and 'fill'. ID is
#' the ID of the feature, and fill are hexes of 6 digits.
#' @param fill_fun <`function`> A function to generate the fill-color configuration.
#' Defaults to \code{\link{map_choropleth_fill_fun}}. It needs to return the JSON
#' that will be fed to `fill-color` paint argument of the mapbox choropleth.
#' @param fill_fun_args <`list`> A list of arguments to pass to the fill_fun function.
#' Defaults to a list with \code{df}, \code{get_col}, and \code{fallback}.
#'
#' @return No return value. The function sends an update message to the Shiny
#' server to update the map.
#'
#' @export
map_choropleth_update_fill_colour <-function(session, map_ID, fill_colour,
                                             fill_fun = map_choropleth_fill_fun,
                                             fill_fun_args = list(df = fill_colour,
                                                                  get_col = names(fill_colour)[1],
                                                                  fallback = "transparent")) {

  # Create an empty configuration list
  configuration <- list()
  configuration$choropleth <- list()

  # Add the fill colour to the configuration list and transfer it to JSON
  configuration$choropleth$fill_colour <- do.call(fill_fun, fill_fun_args)

  # Send the configuration list to the server
  update_map(session = session, map_ID = map_ID, configuration = configuration)
}

#' Update a map's selection
#'
#' This function updates a map's selection. It generates a configuration list
#' that includes the selection and a timestamp to ensure changes are recognized
#' each time it's updated. This configuration is sent to the server to update
#' the map.
#'
#' @param session <`shiny::session`> The Shiny session object.
#' @param map_ID <`character`> A unique identifier for the map input.
#' @param select_id <`character`> The selected ID that should be highlighted on
#' the map.
#'
#' @return No return value. The function sends an update message to the Shiny
#' server to update the map.
#'
#' @export
map_choropleth_update_selection <- function(session, map_ID, select_id) {

  # Create an empty configuration list
  configuration <- list()
  configuration$selection <- list()

  # Add the selection with a timestamp, to make sure it gets triggered at every
  # time it's changing.
  configuration$selection$select_id <- select_id
  configuration$selection$timestamp <- Sys.time()

  # Send the configuration list to the server
  update_map(session = session, map_ID = map_ID, configuration = configuration)
}

#' Remove tileset
#'
#' This function removes the tileset.
#'
#' @param session <`shiny::session`> The Shiny session object.
#' @param map_ID <`character`> A unique identifier for the map input.
#'
#' @return No return value. The function sends an update message to the Shiny
#' server to update the map.
#'
#' @export
map_choropleth_remove <- function(session, map_ID) {

  # Create an empty configuration list
  configuration <- list()
  configuration$choropleth <- list()

  # Send a 'remove' character. This will ensure the removal of the choropleth
  # map.
  configuration$choropleth$tileset <- "remove"

  # Send the configuration list to the server
  update_map(session = session, map_ID = map_ID, configuration = configuration)
}

#' Redraw the tileset
#'
#' If we are under the impression that the map did not draw correctly, force a
#' redraw of the tileset. First, we are looking at if the tilesets are currently
#' loaded on the map. Only if there are not, will we trigger a redraw.
#'
#' @param session <`shiny::session`> The Shiny session object.
#' @param map_ID <`character`> A unique identifier for the map input.
#'
#' @return No return value. The function sends an update message to the Shiny
#' server to update the map.
#'
#' @export
map_choropleth_redraw <- function(session, map_ID) {

  # Create an empty configuration list
  configuration <- list()
  configuration$choropleth <- list()

  # Send a 'redraw' character. This will ensure the redrawal of the tileset
  configuration$choropleth$redraw <- unname(proc.time()[3])*100

  # Send the configuration list to the server
  update_map(session = session, map_ID = map_ID, configuration = configuration)
}

#' Create a JSON object for fill-color in Mapbox choropleth
#'
#' This function takes a data frame and generates a JSON object that represents
#' the fill-color mapping paint property for a Mapbox choropleth map. It matches
#' the values from the specified column with their corresponding colors and includes
#' a fallback color. This function will use the match decision syntax, as is
#' documented here: https://docs.mapbox.com/mapbox-gl-js/style-spec/expressions/#match
#'
#' @param df <`data.frame`> Data frame of two columns. The first is the value that
#' match the tile (e.g. field `ID_colour`, which every normal choropleth tile should have),
#' and the second column is hexes (the fill colour).
#' @param get_col <`character`> Name of the feature value to match the fill color
#' to the tileset. Defaults to the name of the first column of `df`, which more
#' often than not, should be `ID_colour` (value shared by all the normal choropleth
#' tileset, included at import.).
#' @param fallback <`character`> Fallback color if no match is found (default is
#' "transparent")
#'
#' @return A JSON object representing the fill-color mapping
#' @export
map_choropleth_fill_fun <- function(df, get_col = names(df)[1],
                                    fallback = "transparent") {

  # # Convert each row to character
  # row_as_chr <- as.character(apply(df, 1, as.character))
  #
  # # Add the fallback color
  # row_as_chr <- c(row_as_chr, fallback)
  #
  # # Create a Mapbox fill-color object using "match" and "get"
  # mapbox_fill_clr <- c("match", "x", lapply(row_as_chr, c))
  # mapbox_fill_clr[[2]] <- list("get", get_col)
  #
  # # Convert the fill-color object to JSON
  # return(jsonlite::toJSON(mapbox_fill_clr, auto_unbox = T))

  ## FASTER
  row_as_chr <- as.vector(t(df))
  row_as_chr_pasted <- stringi::stri_paste(stringi::stri_paste('"', row_as_chr, '"', sep=""),
                                           collapse=",")
  out <- sprintf('["match",["get","%s"],%s,"%s"]', get_col, row_as_chr_pasted,
                 fallback)
  return(out)
}

