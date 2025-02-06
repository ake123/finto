# Load necessary libraries

#' Search for concepts and collections in a specific vocabulary from the Finto Skosmos API
#'
#' @param vocid The vocabulary ID, e.g., "yso".
#' @param query The search term, e.g., "cat*".
#' @param lang The language of labels to match, e.g., "en" or "fi". Optional.
#' @param type Limit search to concepts of a given type, e.g., "skos:Concept". Optional.
#' @param maxhits Maximum number of results to return. Optional.
#' @param offset Offset where to start in the result set, useful for paging. Optional.
#' @return A tibble containing the search results with fields such as uri, type, prefLabel, and altLabel.
#' @examples
#' search_results <- search_vocabulary_concepts(vocid = "yso", query = "cat", lang = "en")
#' print(search_results)
#' @export
search_vocabulary_concepts <- function(vocid, query, lang = NULL, type = NULL, maxhits = NULL, offset = NULL) {
  # Check if vocid and query are provided
  if (missing(vocid) || is.null(vocid)) {
    stop("The 'vocid' parameter is required.")
  }
  if (missing(query) || is.null(query)) {
    stop("The 'query' parameter is required.")
  }

  # Set up parameters
  params <- list(
    query = query,
    lang = lang,
    type = type,
    maxhits = maxhits,
    offset = offset
  )

  # Request search results
  response <- finto_api_request(paste0(vocid, "/search"), params)

  # Extract results data
  search_results <- response$results

  # Convert search results into a tibble
  results_tibble <- tibble::tibble(
    uri = sapply(search_results, function(x) x$uri),
    type = sapply(search_results, function(x) if (!is.null(x$type)) paste(x$type, collapse = ", ") else NA),
    prefLabel = sapply(search_results, function(x) if (!is.null(x$prefLabel)) x$prefLabel else NA),
    altLabel = sapply(search_results, function(x) if (!is.null(x$altLabel)) x$altLabel else NA),
    hiddenLabel = sapply(search_results, function(x) if (!is.null(x$hiddenLabel)) x$hiddenLabel else NA),
    lang = sapply(search_results, function(x) if (!is.null(x$lang)) x$lang else NA),
    vocab = sapply(search_results, function(x) if (!is.null(x$vocab)) x$vocab else NA),
    notation = sapply(search_results, function(x) if (!is.null(x$notation)) x$notation else NA)
  )

  return(results_tibble)
}
