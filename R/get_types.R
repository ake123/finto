#' Get information about the types (classes) of objects in all vocabularies from the Finto Skosmos API
#'
#' @param lang The language of labels to retrieve, e.g., "en" or "fi". Optional.
#' @return A tibble containing the URI, label, and superclass for each type.
#' @import tibble
#' @examples
#' result <- get_types(lang = "fi")
#' print(result)
#' @export
get_types <- function(lang = NULL) {
  # Set up parameters
  params <- list(
    lang = lang
  )

  # Remove NULL parameters
  params <- Filter(Negate(is.null), params)

  # Request types data
  response <- finto_api_request("types", params)

  # Extract and format types data
  types_data <- response$types

  # Convert types data into a tibble
  types_tibble <- tibble::tibble(
    uri = sapply(types_data, function(x) x$uri),
    label = sapply(types_data, function(x) if (!is.null(x$label)) x$label else NA),
    superclass = sapply(types_data, function(x) if (!is.null(x$superclass)) x$superclass else NA)
  )

  return(types_tibble)
}
