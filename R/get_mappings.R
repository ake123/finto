#' Get mappings for a specific concept from the Finto Skosmos API
#'
#' @param vocid The vocabulary ID, e.g., "yso".
#' @param uri The URI of the concept whose mappings to retrieve.
#' @param lang The language code for the labels, e.g., "fi" or "en". Optional.
#' @return A tibble containing mappings associated with the specified concept.
#' @importFrom httr GET status_code content accept
#' @importFrom jsonlite fromJSON
#' @importFrom tibble tibble
#' @importFrom dplyr bind_rows
#' @examples
#' mappings_data <- get_mappings(vocid = "yso", uri = "http://www.yso.fi/onto/yso/p22922", lang = "fi")
#' print(mappings_data)
#' @export
get_mappings <- function(vocid, uri, lang = NULL) {
  # Construct the base URL for the API request
  base_url <- paste0("https://api.finto.fi/rest/v1/", vocid, "/mappings")

  # Set up the query parameters
  params <- list(
    uri = uri,
    lang = lang
  )

  # Make the API request using httr::GET
  response <- httr::GET(base_url, query = params, httr::accept("application/ld+json"))

  # Check for a successful response
  if (httr::status_code(response) != 200) {
    stop("Error: ", httr::status_code(response), " - ", httr::content(response, "text"))
  }

  # Parse the response content using jsonlite
  data <- jsonlite::fromJSON(httr::content(response, "text", encoding = "UTF-8"))

  # Initialize a tibble to store the results
  results <- tibble(
    uri = character(),
    prefLabel = character(),
    typeLabel = character(),
    description = character(),
    fromURI = character(),
    toURI = character(),
    vocabName = character(),
    hrefLink = character()
  )

  # Extract mappings data
  if (!is.null(data$mappings) && nrow(data$mappings) > 0) {
    for (i in seq_len(nrow(data$mappings))) {
      mapping <- data$mappings[i, ]
      results <- results %>%
        dplyr::bind_rows(
          tibble(
            uri = mapping$uri,
            prefLabel = mapping$prefLabel,
            typeLabel = paste(mapping$type, collapse = ", "),
            description = mapping$description,
            fromURI = mapping$from$memberSet[[1]]$uri,
            toURI = mapping$to$memberSet[[1]]$uri,
            vocabName = mapping$vocabName,
            hrefLink = mapping$hrefLink
          )
        )
    }
  }

  # Return the final tibble with mappings data
  return(results)
}
