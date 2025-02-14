#' Fetch RDF data for a specific concept from the Finto Skosmos API using an asteriID
#'
#' This function retrieves RDF data for a given concept from the Finto API.
#' The user only needs to provide the asteriID, which is appended to a fixed base URI.
#'
#' @param asteriID The unique identifier to append to the base URI.
#' @param format The MIME type of the serialization format (e.g., "application/rdf+xml" or "text/turtle"). Default is "application/json".
#' @return A tibble containing the RDF data for the concept.
#' @import tibble
#' @import purrr
#' @examples
#' concept_data <- fetch_kanto_info(asteriID = "000094320")
#' print(concept_data)
#' @export
fetch_kanto_info <- function(asteriID, format = "application/json") {

  base_uri <- "http://urn.fi/URN:NBN:fi:au:finaf:"

  if (missing(asteriID) || is.null(asteriID)) {
    stop("The 'asteriID' parameter is required.")
  }

  full_uri <- paste0(base_uri, asteriID)
  params <- list(uri = full_uri, format = format)
  response <- finto_api_request("data", params)

  if (!is.list(response) || is.null(response$graph)) {
    stop("Unexpected API response format.")
  }

  graph_data <- response$graph

  # Enhanced safe_extract function to handle empty fields
  safe_extract <- function(x, field, subfield = NULL) {
    val <- if (!is.null(subfield)) x[[field]][[subfield]] else x[[field]]
    if (is.null(val) || length(val) == 0) {
      return(NA_character_)
    }
    if (is.list(val)) {
      return(paste(unlist(val), collapse = ", "))
    }
    val
  }

  # Convert graph data into a tibble
  rdf_tibble <- tibble(
    uri = map_chr(graph_data, ~ safe_extract(.x, "uri")),
    type = map_chr(graph_data, ~ safe_extract(.x, "type")),
    prefLabel = map_chr(graph_data, ~ safe_extract(.x, "prefLabel", "value")),
    altLabel = map_chr(graph_data, ~ safe_extract(.x, "altLabel", "value")),
    hiddenLabel = map_chr(graph_data, ~ safe_extract(.x, "hiddenLabel", "value")),
    broader = map_chr(graph_data, ~ safe_extract(.x, "broader", "uri")),
    narrower = map_chr(graph_data, ~ safe_extract(.x, "narrower", "uri")),
    related = map_chr(graph_data, ~ safe_extract(.x, "related", "uri")),
    definition = map_chr(graph_data, ~ safe_extract(.x, "skos:definition", "value")),
    scopeNote = map_chr(graph_data, ~ safe_extract(.x, "skos:scopeNote", "value")),
    example = map_chr(graph_data, ~ safe_extract(.x, "skos:example", "value")),
    historyNote = map_chr(graph_data, ~ safe_extract(.x, "skos:historyNote", "value")),
    editorialNote = map_chr(graph_data, ~ safe_extract(.x, "skos:editorialNote", "value")),
    changeNote = map_chr(graph_data, ~ safe_extract(.x, "skos:changeNote", "value")),
    note = map_chr(graph_data, ~ safe_extract(.x, "skos:note", "value")),
    birthDate = map_chr(graph_data, ~ safe_extract(.x, "http://rdaregistry.info/Elements/a/P50121")),
    deathDate = map_chr(graph_data, ~ safe_extract(.x, "http://rdaregistry.info/Elements/a/P50120")),
    exactMatch = map_chr(graph_data, ~ safe_extract(.x, "exactMatch", "uri")),
    closeMatch = map_chr(graph_data, ~ safe_extract(.x, "closeMatch", "uri")),
    inScheme = map_chr(graph_data, ~ safe_extract(.x, "inScheme", "uri")),
    created = map_chr(graph_data, ~ safe_extract(.x, "dct:created", "value")),
    modified = map_chr(graph_data, ~ safe_extract(.x, "dct:modified", "value"))
  )

  return(rdf_tibble)
}
