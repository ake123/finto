#' Get top concepts of a specific vocabulary from the Finto Skosmos API
#'
#' @param vocid The vocabulary ID, e.g., "yso".
#' @param lang The language of labels to retrieve, e.g., "en" or "fi". Optional.
#' @param scheme The concept scheme to retrieve top concepts from. Optional.
#' @return A tibble containing the URI, label, topConceptOf, notation, and hasChildren for each top concept.
#' @examples
#' result <- get_top_concepts(vocid = "yso", lang = "fi")
#' print(result)
#' @export
get_top_concepts <- function(vocid, lang = NULL, scheme = NULL) {
  # Check if vocid is provided
  if (missing(vocid) || is.null(vocid)) {
    stop("The 'vocid' parameter is required.")
  }

  # Set up parameters
  params <- list(
    lang = lang,
    scheme = scheme
  )

  # Request top concepts data for the specified vocabulary
  response <- finto_api_request(paste0(vocid, "/topConcepts"), params)

  # Extract and format top concepts data
  topconcepts_data <- response$topconcepts

  # Convert top concepts data into a tibble
  top_concepts_tibble <- tibble::tibble(
    uri = sapply(topconcepts_data, function(x) x$uri),
    label = sapply(topconcepts_data, function(x) if (!is.null(x$label)) x$label else NA),
    topConceptOf = sapply(topconcepts_data, function(x) if (!is.null(x$topConceptOf)) x$topConceptOf else NA),
    notation = sapply(topconcepts_data, function(x) if (!is.null(x$notation)) x$notation else NA),
    hasChildren = sapply(topconcepts_data, function(x) if (!is.null(x$hasChildren)) x$hasChildren else NA)
  )

  return(top_concepts_tibble)
}
