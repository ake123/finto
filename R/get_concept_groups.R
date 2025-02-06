#' Get concept groups for a given vocabulary from the Finto Skosmos API
#'
#' @param vocid The vocabulary ID, e.g., "yso".
#' @param lang The language of labels, e.g., "en" or "fi". Optional.
#' @return A tibble containing concept groups, with fields for URI, prefLabel, and hasMembers.
#' @examples
#' result <- get_concept_groups(vocid = "yso", lang = "fi")
#' print(result)
#' @export
get_concept_groups <- function(vocid, lang = NULL) {
  # Check if vocid is provided
  if (missing(vocid) || is.null(vocid)) {
    stop("The 'vocid' parameter is required.")
  }

  # Set up parameters
  params <- list(
    lang = lang
  )

  # Request concept groups
  response <- finto_api_request(paste0(vocid, "/groups"), params)

  # Convert groups list into a tibble
  groups_tbl <- tibble(
    uri = sapply(response$groups, function(x) x$uri),
    prefLabel = sapply(response$groups, function(x) if (!is.null(x$prefLabel)) x$prefLabel else NA),
    hasMembers = sapply(response$groups, function(x) x$hasMembers),
    childGroups = sapply(response$groups, function(x) if (!is.null(x$childGroups)) paste(x$childGroups, collapse = ", ") else NA)
  )

  return(groups_tbl)
}
