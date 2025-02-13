#' Fetch RDF data for a specific concept from the Finto Skosmos API using an asteriID
#'
#' This function retrieves RDF data for a given concept from the Finto API.
#' The user only needs to provide the asteriID, which is appended to a fixed base URI.
#'
#' @param asteriID The unique identifier to append to the base URI.
#' @param format The MIME type of the serialization format (e.g., "application/rdf+xml" or "text/turtle"). Default is "application/json".
#' @return A tibble containing the RDF data for the concept.
#' @import tibble
#' @examples
#' concept_data <- fetch_kanto_info(asteriID = "000094320")
#' print(concept_data)
#' @export
fetch_kanto_info <- function(asteriID, format = "application/json") {
  # Define the constant base URI
  base_uri <- "http://urn.fi/URN:NBN:fi:au:finaf:"

  # Check if asteriID is provided
  if (missing(asteriID) || is.null(asteriID)) {
    stop("The 'asteriID' parameter is required.")
  }

  # Construct the full URI
  full_uri <- paste0(base_uri, asteriID)

  # Set up parameters
  params <- list(
    uri = full_uri,
    format = format
  )

  # Request concept data
  response <- finto_api_request("data", params)

  # Parse the graph data from the response
  graph_data <- response$graph

  # Convert graph data into a tibble format
  rdf_tibble <- tibble::tibble(
    uri = sapply(graph_data, function(x) x$uri),
    type = sapply(graph_data, function(x) paste(x$type, collapse = ", ")),
    prefLabel = sapply(graph_data, function(x) ifelse(!is.null(x$prefLabel$value), x$prefLabel$value, NA)),
    description = sapply(graph_data, function(x) ifelse(!is.null(x$`dc11:description`$value), x$`dc11:description`$value, NA)),
    publisher = sapply(graph_data, function(x) ifelse(!is.null(x$`dc11:publisher`), paste(sapply(x$`dc11:publisher`, function(p) p$value), collapse = ", "), NA)),
    contributor = sapply(graph_data, function(x) ifelse(!is.null(x$`dc11:contributor`), paste(sapply(x$`dc11:contributor`, function(c) c$value), collapse = ", "), NA))
  )

  return(rdf_tibble)
}
