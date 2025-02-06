#' Get narrower concepts for a specific concept from the Finto Skosmos API
#'
#' @param vocid The vocabulary ID, e.g., "yso".
#' @param uri The URI of the concept whose narrower concepts to retrieve.
#' @param lang The language code for the labels, e.g., "fi" or "en". Optional.
#' @return A tibble containing the narrower concepts (prefLabel and uri) for the specified concept.
#' @importFrom httr GET status_code content accept
#' @importFrom jsonlite fromJSON
#' @importFrom tibble tibble
#' @importFrom utils str
#' @examples
#' narrower_concepts <- get_narrower_concepts(vocid = "yso",
#' uri = "http://www.yso.fi/onto/yso/p690", lang = "fi")
#' print(narrower_concepts)
#' @export
get_narrower_concepts <- function(vocid, uri, lang = NULL) {
  # Input validation
  if (missing(vocid) || missing(uri)) {
    stop("Both 'vocid' and 'uri' parameters are required.")
  }

  # Construct the base URL for the API request
  base_url <- paste0("https://api.finto.fi/rest/v1/", vocid, "/narrower")

  # Set up the query parameters
  params <- list(
    uri = uri,
    lang = lang
  )

  # Make the API request using httr::GET
  response <- httr::GET(base_url, query = params, httr::accept("application/ld+json"))

  # Check for a successful response
  if (httr::status_code(response) != 200) {
    error_message <- httr::content(response, "text")
    stop("Error: ", httr::status_code(response), " - ", error_message)
  }

  # Parse the response content using jsonlite
  data <- jsonlite::fromJSON(httr::content(response, "text", encoding = "UTF-8"))

  # Print the structure of the parsed data for debugging
  print(str(data))  # Debugging line to inspect the structure

  # Check if there are no narrower concepts
  if (nrow(data$narrower) == 0) {
    return(tibble::tibble(uri = character(), prefLabel = character()))
  }

  # Structure the output as a tibble directly from the data frame
  narrower_concepts <- tibble::tibble(
    uri = data$narrower$uri,
    prefLabel = data$narrower$prefLabel
  )

  return(narrower_concepts)
}
