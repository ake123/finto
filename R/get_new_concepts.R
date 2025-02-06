#' Get new concepts in a specific vocabulary from the Finto Skosmos API
#'
#' @param vocid The vocabulary ID, e.g., "yso".
#' @param lang The language of labels, e.g., "en" or "fi". Optional.
#' @param offset Offset for the starting index of the results. Optional.
#' @param limit Maximum number of concepts to return. Optional.
#' @return A tibble containing the new concepts with URI, label, and creation date.
#' @examples
#' result <- get_new_concepts(vocid = "yso", lang = "fi", limit = 10)
#' print(result)
#' @export
get_new_concepts <- function(vocid, lang = NULL, offset = NULL, limit = NULL) {
  # Check if vocid is provided
  if (missing(vocid) || is.null(vocid)) {
    stop("The 'vocid' parameter is required.")
  }

  # Set up parameters
  params <- list(
    lang = lang,
    offset = offset,
    limit = limit
  )

  # Request new concepts
  response <- finto_api_request(paste0(vocid, "/new"), params)

  # Convert new concepts list into a tibble
  new_concepts_tbl <- tibble(
    uri = sapply(response$changeList, function(x) x$uri),
    prefLabel = sapply(response$changeList, function(x) if (!is.null(x$prefLabel)) x$prefLabel else NA),
    date = sapply(response$changeList, function(x) x$date)
  )

  return(new_concepts_tbl)
}
