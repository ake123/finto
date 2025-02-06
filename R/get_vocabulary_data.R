#' Get and structure RDF data of the whole vocabulary or a specific concept from the Finto Skosmos API
#'
#' @param vocid The vocabulary ID, e.g., "yso".
#' @param uri The URI of a specific concept to retrieve data for. If NULL, retrieves data for the entire vocabulary. Optional.
#' @param format The MIME type of the serialization format, e.g., "application/rdf+xml". Default is "application/rdf+xml".
#' @param lang The RDF language code for the requested resource, e.g., "fi" or "en". Optional.
#' @return A tibble containing structured RDF data for the specified vocabulary or concept.
#' #' @examples
#' rdf_xml_data <- get_vocabulary_data(vocid = "yso", uri = "http://www.w3.org/2004/02/skos/core#Concept", format = "application/rdf+xml")
#' cat(rdf_xml_data)
#' turtle_data <- get_vocabulary_data(vocid = "yso", format = "text/turtle")
#' cat(turtle_data)  # Print the Turtle data
#' @export
get_vocabulary_data <- function(vocid, format = "application/rdf+xml", uri = NULL, lang = NULL) {
  # Construct the base URL for the API request
  base_url <- paste0("https://api.finto.fi/rest/v1/", vocid, "/data")

  # Set up the query parameters for the API request
  params <- list(
    format = format,
    uri = uri,
    lang = lang
  )

  # Make the API request using httr::GET
  response <- httr::GET(base_url, query = params, httr::accept(format))

  # Check for successful response and handle errors
  if (httr::status_code(response) != 200) {
    stop("Error: ", httr::status_code(response), " - ", httr::content(response, "text"))
  }

  # Return the content of the response as text
  return(httr::content(response, "text"))
}
