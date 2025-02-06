#' Get labels for a specific concept from the Finto Skosmos API and return as a tibble
#'
#' @param uri The URI of the concept to retrieve labels for.
#' @param lang The language of labels to retrieve, e.g., "en" or "fi". Optional.
#' @return A tibble containing the URI, preferred label (`prefLabel`), alternative labels (`altLabel`), and hidden labels (`hiddenLabel`) for the concept.
#' @import tibble
#' @examples
#' labeles <- get_concept_labels(uri = "http://www.yso.fi/onto/koko/p91024",
#' lang = "fi")
#' print(labeles)
#' @export
get_concept_labels <- function(uri, lang = NULL) {
  # Check if uri is provided
  if (missing(uri) || is.null(uri)) {
    stop("The 'uri' parameter is required.")
  }

  # Set up parameters
  params <- list(
    uri = uri,
    lang = lang
  )

  # Remove any NULL parameters
  params <- Filter(Negate(is.null), params)

  # Request concept labels
  response <- finto_api_request("label", params)

  # Create a tibble from the labels
  labels_tibble <- tibble(
    uri = response$uri,
    prefLabel = response$prefLabel,
    altLabel = if (!is.null(response$altLabel)) paste(response$altLabel, collapse = ", ") else NA,
    hiddenLabel = if (!is.null(response$hiddenLabel)) paste(response$hiddenLabel, collapse = ", ") else NA
  )

  return(labels_tibble)
}
