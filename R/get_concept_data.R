#' Get RDF data for a specific concept from the Finto Skosmos API
#'
#' @param uri The URI of the concept to retrieve data for.
#' @param format The MIME type of the serialization format (e.g., "application/rdf+xml" or "text/turtle"). Default is "application/json".
#' @return A tibble containing the RDF data for the concept.
#' @import tibble
#' @examples
#' concept_data <- get_concept_data(uri = "http://www.yso.fi/onto/afo/")
#' print(concept_data)
#' @export
get_concept_data <- function(uri, format = "application/json") {
  # Check if uri is provided
  if (missing(uri) || is.null(uri)) {
    stop("The 'uri' parameter is required.")
  }

  # Set up parameters
  params <- list(
    uri = uri,
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
