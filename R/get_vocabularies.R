#' Get available vocabularies from the Finto Skosmos API
#'
#' @param lang Language of labels, e.g., "en" or "fi" (default is "fi")
#' @return A data frame with the vocabulary details: uri, id, and title
#' @examples
#' result <- get_vocabularies(lang = "fi")
#' print(result)
#' @export
get_vocabularies <- function(lang = "fi") {
  # Send request to the 'vocabularies' endpoint with the specified language
  response <- finto_api_request("vocabularies", list(lang = lang))

  # Extract relevant vocabulary data
  vocabularies <- response$vocabularies

  # Convert to a data frame with uri, id, and title fields
  vocab_data <- do.call(rbind, lapply(vocabularies, function(voc) {
    data.frame(
      uri = voc$uri,
      id = voc$id,
      title = voc$title,
      stringsAsFactors = FALSE
    )
  }))

  return(vocab_data)
}
