#' Get RDF data of a specific vocabulary or concept from the Finto Skosmos API
#'
#' @param vocid The vocabulary ID, e.g., "yso".
#' @param format The MIME type of the serialization format, e.g., "text/turtle" or "application/rdf+xml". Default is "application/rdf+xml".
#' @param uri Optional URI of the specific concept to retrieve data for. If not provided, the whole vocabulary is returned.
#' @param lang Optional language code for the RDF resource, e.g., "fi" or "en".
#' @return The RDF data in the specified format.
#' @import tidyverse
#' @examples
#' result <- get_vocabulary_data(vocid = "yso", format = "text/turtle")
#' print(result)
#' @export
get_vocabulary_data <- function(vocid, format = "application/rdf+xml", uri = NULL, lang = NULL) {
  # Construct the base URL
  base_url <- paste0("https://api.finto.fi/rest/v1/", vocid, "/data")

  # Set up the query parameters
  params <- list(
    format = format,
    uri = uri,
    lang = lang
  )

  # Make the API request
  response <- GET(base_url, query = params, accept(format))

  # Check for successful response
  if (status_code(response) != 200) {
    stop("Error: ", status_code(response), " - ", content(response, "text"))
  }

  # Return the content of the response as text
  return(content(response, "text"))
}
