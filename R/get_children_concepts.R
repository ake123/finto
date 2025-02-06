#' Get children concepts for a specific concept from the Finto Skosmos API
#'
#' @param vocid The vocabulary ID, e.g., "yso".
#' @param uri The URI of the concept whose children concepts to retrieve.
#' @param lang The language code for the labels, e.g., "fi" or "en". Optional.
#' @return A tibble containing the children concepts (prefLabel, uri, hasChildren) for the specified concept.
#' @importFrom httr GET status_code content accept
#' @importFrom jsonlite fromJSON
#' @importFrom tibble tibble
#' @examples
#' children <- get_children_concepts(vocid = "yso",
#'  uri = "http://www.yso.fi/onto/yso/p690", lang = "fi")
#' print(children)
#' @export
get_children_concepts <- function(vocid, uri, lang = NULL) {
  # Construct the base URL for the API request
  base_url <- paste0("https://api.finto.fi/rest/v1/", vocid, "/children")

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

  # Print the structure of data for debugging
  #print(str(data))

  # Check if data contains narrower concepts
  if (!is.null(data$narrower) && nrow(data$narrower) > 0) {
    # Structure the output as a tibble
    children_concepts <- tibble::tibble(
      uri = data$narrower$uri,
      prefLabel = data$narrower$prefLabel,
      hasChildren = data$narrower$hasChildren
    )
  } else {
    children_concepts <- tibble::tibble(uri = character(), prefLabel = character(), hasChildren = logical())
  }

  return(children_concepts)
}
