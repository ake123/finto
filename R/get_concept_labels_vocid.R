#' Get labels for a specific concept from the Finto Skosmos API
#'
#' @param vocid The vocabulary ID, e.g., "yso".
#' @param uri The URI of the concept whose labels to retrieve.
#' @param lang The language code for the labels, e.g., "fi" or "en". Optional.
#' @return A tibble containing the labels (prefLabel, altLabel, hiddenLabel) for the specified concept.
#' @importFrom httr GET status_code content accept
#' @importFrom jsonlite fromJSON
#' @importFrom tibble tibble
#' @examples
#' lables <- get_concept_labels_vocid(vocid = "yso",
#' uri = "http://www.yso.fi/onto/yso/p22922", lang = "fi")
#' print(lables)
#' @export
get_concept_labels_vocid <- function(vocid, uri, lang = NULL) {
  # Construct the base URL for the API request
  base_url <- paste0("https://api.finto.fi/rest/v1/", vocid, "/label")

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
  #print(data)

  # Structure the output as a tibble
  result <- tibble::tibble(
    uri = data$uri,
    prefLabel = data$prefLabel,
    altLabel = ifelse(length(data$altLabel) > 0, paste(data$altLabel, collapse = ", "), NA),
    hiddenLabel = ifelse(length(data$hiddenLabel) > 0, paste(data$hiddenLabel, collapse = ", "), NA)
  )

  return(result)
}
