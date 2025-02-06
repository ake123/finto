#' Get label statistics by language in a specific vocabulary from the Finto Skosmos API
#'
#' @param vocid The vocabulary ID, e.g., "yso".
#' @return A tibble containing the label statistics for each language, with columns for language, literal, property, and label count.
#' @examples
#' result <- get_label_statistics(vocid = "yso")
#' print(result)
#' @export
get_label_statistics <- function(vocid) {
  # Check if vocid is provided
  if (missing(vocid) || is.null(vocid)) {
    stop("The 'vocid' parameter is required.")
  }

  # Request label statistics
  response <- finto_api_request(paste0(vocid, "/labelStatistics"))

  # Extract language-specific label statistics
  label_stats <- do.call(rbind, lapply(response$languages, function(lang_data) {
    tibble(
      language = lang_data$language,
      literal = lang_data$literal,
      property = sapply(lang_data$properties, function(x) x$property),
      labels = sapply(lang_data$properties, function(x) x$labels)
    )
  }))

  return(label_stats)
}
