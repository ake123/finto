#' Get concepts starting with a specific letter in the alphabetical index for a given vocabulary
#'
#' @param vocid The vocabulary ID, e.g., "yso".
#' @param letter The initial letter, or "0-9" for numeric or "!*" for special character labels.
#' @param lang The language of labels, e.g., "en" or "fi". Optional.
#' @return A tibble containing concepts with labels starting with the given letter.
#' @examples
#' result <- get_concepts_by_letter(vocid = "yso", letter = "A", lang = "fi")
#' print(result)
#' @export
get_concepts_by_letter <- function(vocid, letter, lang = NULL) {
  # Check if vocid and letter are provided
  if (missing(vocid) || is.null(vocid)) {
    stop("The 'vocid' parameter is required.")
  }
  if (missing(letter) || is.null(letter)) {
    stop("The 'letter' parameter is required.")
  }

  # Set up parameters
  params <- list(
    lang = lang
  )

  # Request concepts for the given letter
  response <- finto_api_request(paste0(vocid, "/index/", letter), params)
  print

  # Convert concepts list into a tibble
  concepts_tbl <- tibble(
    uri = sapply(response$indexConcepts, function(x) x$uri),
    localname = sapply(response$indexConcepts, function(x) x$localname),
    prefLabel = sapply(response$indexConcepts, function(x) if (!is.null(x$prefLabel)) x$prefLabel else NA),
    altLabel = sapply(response$indexConcepts, function(x) if (!is.null(x$altLabel)) x$altLabel else NA),
    lang = sapply(response$indexConcepts, function(x) if (!is.null(x$lang)) x$lang else NA)
  )

  return(concepts_tbl)
}
