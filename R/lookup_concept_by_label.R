#' Look up concepts by label in a specific vocabulary from the Finto Skosmos API
#'
#' @param vocid The vocabulary ID, e.g., "yso".
#' @param label The label to look up, e.g., "cat".
#' @param lang The search language, e.g., "en" or "fi". Optional.
#' @return A tibble containing the best matching concept(s) with fields like uri, type, prefLabel, and altLabel.
#' @examples
#' result <- lookup_concept_by_label(vocid = "yso", label = "cat", lang = "en")
#' print(result)
#' @export
lookup_concept_by_label <- function(vocid, label, lang = NULL) {
  # Check if vocid and label are provided
  if (missing(vocid) || is.null(vocid)) {
    stop("The 'vocid' parameter is required.")
  }
  if (missing(label) || is.null(label)) {
    stop("The 'label' parameter is required.")
  }

  # Set up parameters
  params <- list(
    label = label,
    lang = lang
  )

  # Request lookup results
  response <- finto_api_request(paste0(vocid, "/lookup"), params)

  # Extract result data
  lookup_results <- response$result

  # Convert lookup results into a tibble
  results_tibble <- tibble::tibble(
    uri = sapply(lookup_results, function(x) x$uri),
    type = sapply(lookup_results, function(x) if (!is.null(x$type)) paste(x$type, collapse = ", ") else NA),
    prefLabel = sapply(lookup_results, function(x) if (!is.null(x$prefLabel)) x$prefLabel else NA),
    altLabel = sapply(lookup_results, function(x) if (!is.null(x$altLabel)) x$altLabel else NA),
    hiddenLabel = sapply(lookup_results, function(x) if (!is.null(x$hiddenLabel)) x$hiddenLabel else NA),
    lang = sapply(lookup_results, function(x) if (!is.null(x$lang)) x$lang else NA),
    vocab = sapply(lookup_results, function(x) if (!is.null(x$vocab)) x$vocab else NA),
    notation = sapply(lookup_results, function(x) if (!is.null(x$notation)) x$notation else NA)
  )

  return(results_tibble)
}
