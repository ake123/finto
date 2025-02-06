#' Get narrower transitive concepts for a specific concept from the Finto Skosmos API
#'
#' @param vocid The vocabulary ID, e.g., "yso".
#' @param uri The URI of the concept whose narrower transitive hierarchy to retrieve.
#' @param lang The language code for the labels, e.g., "fi" or "en". Optional.
#' @return A tibble containing the narrower transitive concepts (prefLabel and uri) for the specified concept.
#' @importFrom httr GET status_code content accept
#' @importFrom jsonlite fromJSON
#' @importFrom tibble tibble
#' @examples
#' narrow <- get_narrower_transitive(vocid = "yso",
#' uri = "http://www.yso.fi/onto/yso/p690", lang = "fi")
#' print(narrow)
#' @export
get_narrower_transitive <- function(vocid, uri, lang = NULL) {
  # Construct the base URL for the API request
  base_url <- paste0("https://api.finto.fi/rest/v1/", vocid, "/narrowerTransitive")

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

  # Extract narrower transitive concepts
  if (is.null(data$narrowerTransitive) || length(data$narrowerTransitive) == 0) {
    return(tibble::tibble(uri = character(), prefLabel = character()))
  }

  # Structure the output as a tibble
  narrower_transitive <- tibble::tibble(
    uri = unlist(lapply(data$narrowerTransitive, function(x) x$uri)),
    prefLabel = unlist(lapply(data$narrowerTransitive, function(x) x$prefLabel))
  )

  return(narrower_transitive)
}
