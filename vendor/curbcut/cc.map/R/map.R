#' Create a React Shiny input for a map
#'
#' This function creates a React Shiny input for a map. It generates a map
#' input widget with specified parameters and adds it to the Shiny application.
#'
#' @param map_ID <`character`> A unique identifier for the map input.
#' @param username <`character`> Mapbox username, where the tilesets live.
#' @param token <`character`> Necessary token to access mapbox.
#' @param longitude <`numeric`> The longitude value for the initial map center.
#' @param latitude <`numeric`> The latitude value for the initial map center.
#' @param zoom <`numeric`> The zoom level for the initial map display.
#' @param inst_prefix  <`numeric`> The prefix for the tileset to be used with the map.
#' This will be used only for the stories tileset.
#' @param map_style_id <`character`> Full map identifier link, e.g.
#' `"mapbox://styles/curbcut/cljkciic3002h01qveq5z1wrp"`.
#' @param lang <`character`> Default language of the app. This will inform
#' which stories preview is shown.
#' @param stories <`data.frame`> The stories dataframe. Defaults to NULL if no
#' stories is to be added to the map.
#' @param stories_min_zoom <`numeric`> What is the minimum zoom at which the stories
#' should appear?
#' @param div_height <`character`> Height of the input. The map will take 100%
#' of the space. Defaults to the entire viewport (100vh).
#' @param div_width <`character`> Width of the input. The map will take 100%
#' of the space. Defaults to 100% of the width of the viewport (100%).
#'
#' @return A React Shiny input widget for the map.
#'
#' @importFrom reactR createReactShinyInput
#' @importFrom htmltools htmlDependency tags div
#'
#' @export
map_input <- function(map_ID, username, token, longitude, latitude, zoom, inst_prefix,
                      map_style_id, stories = NULL, stories_min_zoom = 1, lang = "en", div_height = "100vh",
                      div_width = "100%") {

  if (stories_min_zoom == 0) stop(paste0("`stories_min_zoom` can't be `0`, as ",
                                         "it means NO zoom level. Use `1` instead."))

  div_style <- sprintf("height: %s; width: %s", div_height, div_width)
  div <- function(...) {
    do.call(htmltools::tags$div, list(style = div_style, ...))
  }

  # All default configurations
  configurations <- list(
    username = username,
    token = token,
    lang = lang,
    style = map_style_id,
    viewstate = list(longitude = longitude,
                     latitude = latitude,
                     zoom = zoom)
  )

  # If NULL, no stories added.
  if (!is.null(stories)) {
    configurations$stories <- list()
    configurations$stories$stories <- sprintf("%s_stories", inst_prefix)
    configurations$stories$stories_img <- sapply(stories$img_base64, list) |> jsonlite::toJSON()
    configurations$stories$min_zoom <- stories_min_zoom
  }

  reactR::createReactShinyInput(
    map_ID,
    "map",
    htmltools::htmlDependency(
      name = "map-inputss",
      version = "1.0.0",
      src = "www/cc.map/map",
      package = "cc.map",
      script = "map.js"
    ),
    "",
    configurations,
    div
  )
}

#' Update a map in a Shiny application
#'
#' This function updates a map in a Shiny application by sending an input message
#' to the specified map_ID. It includes a configuration object for updating specific
#' aspects of the map.
#'
#' @param session <`session`> The Shiny session object.
#' @param map_ID <`character`> The identifier of the map to be updated.
#' @param configuration A named list object for updating the map, with the following
#' options:
#' - `viewstate`: To update viewstate, configuration must be a named list of
#'  `lat`, `lon`, and `zoom`, as numeric.
#' - `select_id`: A character value to update the selected feature on the map.
#' Selection must be in the viewport for the feature to get updated. Update viewstate first.
#' - `fill_colour`: A tibble with two columns: 'ID' and 'fill'. ID is
#' the ID of the feature, and fill are hexes of 6 digits.
#' - `tileset`: A character value to update the tileset used for rendering the map.
#' Should be used in combination with `fill_colour`.
#'
#' @return None.
update_map <- function(session, map_ID, configuration = NULL) {
  message <- list(value = map_ID)

  if (!is.null(configuration)) {
    message$configuration <- configuration
  }

  session$sendInputMessage(map_ID, message)
}
