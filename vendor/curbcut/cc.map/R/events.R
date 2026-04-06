#' Retrieve Event Information from a Map
#'
#' This function retrieves the specified event information of a Shiny map object.
#' The type of event is specified with `event_type` and the values to be
#' retrieved are defined in `values`.
#'
#' @param map_id <`character`> The identifier for the map object in the Shiny application.
#' @param event_type <`character`>The type of event to retrieve information for.
#' @param values <`character vector`> A vector of values to be retrieved from the
#' specified event.
#' @param session <`session`> The Shiny session object. Defaults to the current reactive
#' domain.
#' @return The specified values for the event if it exists and is of the
#' specified event type, NULL otherwise.
#' @examples
#' \dontrun{
#' get_map_event(
#'   map_id = "map", event_type = "viewstate",
#'   values = c("lon", "lat", "zoom")
#' )
#' }
#' @export
get_map_event <- function(map_id, event_type, values, session = shiny::getDefaultReactiveDomain()) {
  if (!is.null(session$input[[map_id]]["event"])) {
    if (!is.na(session$input[[map_id]]["event"])) {
      if (session$input[[map_id]]["event"] == event_type) {
        return(session$input[[map_id]][values])
      }
    }
  }
}

#' Retrieve ViewState Information from a Map
#'
#' This function is a specialized version of `get_map_event()`, specifically
#' designed to retrieve "viewstate" (longitude, latitude and zoom) information
#' from a Shiny map object.
#'
#' @param map_id <`character`> The identifier for the map object in the Shiny application.
#' @param session <`session`> The Shiny session object. Defaults to the current reactive
#' domain.
#' @return The viewstate information if it exists, NULL otherwise.
#' @examples
#' \dontrun{
#' get_map_viewstate(map_id = "map")
#' }
#' @seealso \code{\link{get_map_event}}
#' @export
get_map_viewstate <- function(map_id, session = shiny::getDefaultReactiveDomain()) {
  out <- get_map_event(
    map_id = map_id, event_type = "viewstate",
    values = c("longitude", "latitude", "zoom", "boundingbox"), session = session
  )

  if (length(out) == 0) {
    return(NULL)
  }

  out[c("longitude", "latitude", "zoom")] <-
    lapply(out[c("longitude", "latitude", "zoom")], as.numeric)
  out$boundingbox$southWest <- lapply(out$boundingbox$southWest, as.numeric)
  out$boundingbox$northEast <- lapply(out$boundingbox$northEast, as.numeric)

  return(out)
}

#' Retrieve Click Information from a Map
#'
#' This function is a specialized version of `get_map_event()`, specifically
#' designed to retrieve "click" (ID and sourceLayer) information from a Shiny
#' map object.
#'
#' @param map_id <`character`> The identifier for the map object in the Shiny application.
#' @param session <`session`> The Shiny session object. Defaults to the current reactive
#' domain.
#' @return The click information if it exists, NULL otherwise.
#' @examples
#' \dontrun{
#' get_map_click(map_id = "map")
#' }
#' @seealso \code{\link{get_map_event}}
#' @export
get_map_click <- function(map_id, session = shiny::getDefaultReactiveDomain()) {
  out <- get_map_event(
    map_id = map_id, event_type = "click",
    values = c("ID", "layerName"), session = session
  )

  if (length(out) == 0) {
    return(NULL)
  }
  if (length(out$ID) == 0) {
    return(list(ID = NA))
  }

  return(out)
}
