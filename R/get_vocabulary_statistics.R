#' Get vocabulary statistics as a single tibble containing concepts, subtypes, and concept groups
#'
#' @param vocid The vocabulary ID, e.g., "yso".
#' @param lang The language of labels, e.g., "en" or "fi". Optional.
#' @return A tibble containing counts of concepts, subtypes, and concept groups, with each entry in one row.
#' @importFrom dplyr bind_rows
#' @examples
#' result <- get_vocabulary_statistics(vocid = "yso", lang = "fi")
#' print(result)
#' @export
get_vocabulary_statistics <- function(vocid, lang = NULL) {
  # Check if vocid is provided
  if (missing(vocid) || is.null(vocid)) {
    stop("The 'vocid' parameter is required.")
  }

  # Set up parameters
  params <- list(
    lang = lang
  )

  # Request vocabulary statistics
  response <- finto_api_request(paste0(vocid, "/vocabularyStatistics"), params)

  # Create a tibble for the main concepts statistics
  concepts_tbl <- tibble(
    category = "Concepts",
    class = response$concpets$class,
    label = response$concpets$label,
    count = response$concpets$count,
    deprecatedCount = response$concpets$deprecatedCount
  )

  # Create a tibble for each subtype of concept
  subTypes_tbl <- tibble(
    category = "SubTypes",
    class = sapply(response$subTypes, function(x) x$type),
    label = sapply(response$subTypes, function(x) x$label),
    count = sapply(response$subTypes, function(x) x$count),
    deprecatedCount = sapply(response$subTypes, function(x) x$deprecatedCount)
  )

  # Create a tibble for the concept groups statistics
  conceptGroups_tbl <- tibble(
    category = "ConceptGroups",
    class = response$conceptGroups$class,
    label = response$conceptGroups$label,
    count = response$conceptGroups$count,
    deprecatedCount = response$conceptGroups$deprecatedCount
  )

  # Combine all tables into one
  vocabulary_statistics_tbl <- bind_rows(concepts_tbl, subTypes_tbl, conceptGroups_tbl)

  return(vocabulary_statistics_tbl)
}
