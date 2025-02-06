#' Get the initial letters of the alphabetical index for labels in a specific
#' vocabulary from the Finto Skosmos API
#'
#' @param vocid The vocabulary ID, e.g., "yso".
#' @param lang The language of labels, e.g., "en" or "fi". Optional.
#' @return A tibble containing the initial letters of the alphabetical index.
#' @examples
#' result <- get_alphabetical_index_letters(vocid = "yso", lang = "fi")
#' print(result)
#' @export
get_alphabetical_index_letters <- function(vocid, lang = NULL) {
  # Check if vocid is provided
  if (missing(vocid) || is.null(vocid)) {
    stop("The 'vocid' parameter is required.")
  }

  # Set up parameters
  params <- list(
    lang = lang
  )

  # Request initial letters of the alphabetical index
  response <- finto_api_request(paste0(vocid, "/index/"), params)

  # Convert index letters to a tibble and unlist the index letters
  index_letters_tbl <- tibble(
    indexLetter = unlist(response$indexLetters)
  )

  return(index_letters_tbl)
}
