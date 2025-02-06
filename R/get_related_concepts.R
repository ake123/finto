#' Get related concepts for a specific concept from the Finto Skosmos API
#'
#' @param vocid The vocabulary ID, e.g., "yso".
#' @param uri The URI of the concept whose related concepts to retrieve.
#' @param lang The language code for the labels, e.g., "fi" or "en". Optional.
#' @return A tibble containing the related concepts (prefLabel and uri) for the specified concept.
#' @importFrom httr GET status_code content accept
#' @importFrom jsonlite fromJSON
#' @importFrom tibble tibble
#' @examples
#' related <- get_related_concepts(vocid = "yso",
#' uri = "http://www.yso.fi/onto/yso/p24489", lang = "fi")
#' print(related)
#' @export
get_related_concepts <- function(vocid, uri, lang = NULL) {
  # Construct the base URL for the API request
  base_url <- paste0("https://api.finto.fi/rest/v1/", vocid, "/related")

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

  # Extract related concepts
  if (is.null(data$related) || length(data$related) == 0) {
    return(tibble::tibble(uri = character(), prefLabel = character()))
  }

  # Check if related is a data frame and extract values safely
  if (is.data.frame(data$related)) {
    related_concepts <- tibble::tibble(
      uri = data$related$uri,
      prefLabel = data$related$prefLabel
    )
  } else {
    # If related is not a data frame, handle it accordingly
    related_concepts <- tibble::tibble(
      uri = sapply(data$related, function(x) x$uri),
      prefLabel = sapply(data$related, function(x) x$prefLabel)
    )
  }

  return(related_concepts)
}
