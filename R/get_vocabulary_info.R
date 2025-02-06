#' Get general information about a specific vocabulary from the Finto Skosmos API
#'
#' @param vocid The vocabulary ID, e.g., "yso".
#' @param lang The language of labels to retrieve, e.g., "en" or "fi". Optional.
#' @return A list containing the vocabulary details such as URI, title, languages, and concept schemes.
#' @examples
#' result <- get_vocabulary_info(vocid = "yso", lang = "fi")
#' print(result)
#' @export
get_vocabulary_info <- function(vocid, lang = NULL) {
  # Check if vocid is provided
  if (missing(vocid) || is.null(vocid)) {
    stop("The 'vocid' parameter is required.")
  }

  # Set up parameters
  params <- list(
    lang = lang
  )

  # Request vocabulary information
  response <- finto_api_request(paste0(vocid, "/"), params)

  # Extract vocabulary-level information
  vocab_uri <- response$uri
  vocab_id <- response$id
  vocab_title <- response$title
  vocab_marcSource <- response$marcSource
  vocab_defaultLanguage <- response$defaultLanguage
  vocab_languages <- paste(unlist(response$languages), collapse = ", ")  # Flatten to a single string

  # Create a tibble for concept schemes with additional vocabulary-level columns
  conceptschemes <- tibble::tibble(
    vocab_uri = vocab_uri,
    vocab_id = vocab_id,
    vocab_title = vocab_title,
    vocab_marcSource = vocab_marcSource,
    vocab_defaultLanguage = vocab_defaultLanguage,
    vocab_languages = vocab_languages,
    label = sapply(response$conceptschemes, function(x) if (!is.null(x$label)) x$label else NA),
    title = sapply(response$conceptschemes, function(x) if (!is.null(x$title)) x$title else NA),
    uri = sapply(response$conceptschemes, function(x) x$uri),
    type = sapply(response$conceptschemes, function(x) if (!is.null(x$type)) x$type else NA)
  )

  return(conceptschemes)
}
