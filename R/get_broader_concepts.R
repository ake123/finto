#' Get broader concepts for a specific concept from the Finto Skosmos API
#'
#' @param vocid The vocabulary ID, e.g., "yso".
#' @param uri The URI of the concept whose broader concepts to retrieve.
#' @param lang The language code for the labels, e.g., "fi" or "en". Optional.
#' @return A tibble containing the broader concepts (prefLabel and uri) for the specified concept.
#' @importFrom httr GET status_code content accept
#' @importFrom jsonlite fromJSON
#' @importFrom tibble tibble
#' @examples
#' conepts <- get_broader_concepts(vocid = "yso",
#' uri = "http://www.yso.fi/onto/yso/p22922", lang = "fi")
#' print(conepts)
#' @export
get_broader_concepts <- function(vocid, uri, lang = NULL) {
  # Construct the base URL for the API request
  base_url <- paste0("https://api.finto.fi/rest/v1/", vocid, "/broader")

  # Set up the query parameters
  params <- list(
    uri = uri,
    lang = lang
  )

  # Make the API request using httr::GET
  response <- httr::GET(base_url, query = params, httr::accept("application/ld+json"))

  # Check for a successful response
  if (httr::status_code(response) != 200) {
    stop("Error: ", httr::status_code(response), " - ", httr::content(response, "text"))
  }

  # Parse the response content using jsonlite
  data <- jsonlite::fromJSON(httr::content(response, "text", encoding = "UTF-8"))

  # Structure the output as a tibble
  broader_concepts <- tibble::tibble(
    uri = data$broader$uri,
    prefLabel = data$broader$prefLabel
  )

  return(broader_concepts)
}
