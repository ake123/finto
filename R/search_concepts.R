#' Search for concepts in the Finto Skosmos API by query term with additional parameters
#'
#' @param query The term to search for, e.g., "sibelius".
#' @param lang Language of labels to match, e.g., "en" or "fi". Optional.
#' @param labellang Language of labels to return, e.g., "en" or "fi". Optional.
#' @param vocab Vocabulary or vocabularies to limit search to, e.g., "yso". Optional.
#' @param type Limit search to concepts of the given type(s), e.g., "skos:Concept". Optional.
#' @param parent Limit search to concepts which have the given concept as a parent. Optional.
#' @param group Limit search to concepts in a specific group. Optional.
#' @param maxhits Maximum number of results to return (default is 191 as per the screenshot). Optional.
#' @param offset Offset to start in the result set, useful for paging. Optional.
#' @param fields Extra fields to include in the results (space-separated list). Optional.
#' @param unique Boolean flag to return each concept only once. Optional.
#' @return A data frame containing search results with columns: `uri`, `type`, `prefLabel`, `altLabel`, `hiddenLabel`, `lang`, and `vocab`.
#' @examples
#' concepts <- search_concepts(query = "sibelius", lang = "fi", maxhits = 191)
#' print(concepts)
#' @export
search_concepts <- function(query, lang = NULL, labellang = NULL, vocab = NULL, type = NULL,
                            parent = NULL, group = NULL, maxhits = NULL, offset = NULL,
                            fields = NULL, unique = NULL) {
  # Set up parameters
  params <- list(
    query = query,
    lang = lang,
    labellang = labellang,
    vocab = vocab,
    type = type,
    parent = parent,
    group = group,
    maxhits = maxhits,
    offset = offset,
    fields = fields,
    unique = unique
  )

  # Remove NULL parameters
  params <- Filter(Negate(is.null), params)

  # Request search results
  response <- finto_api_request("search", params)

    # Extract and format search results
  results <- response$results
  if (is.null(results) || length(results) == 0) {
    warning("No results found for the specified query.")
    return(data.frame())
  }

  # Convert results into a data frame
  result_data <- do.call(rbind, lapply(results, function(result) {
    data.frame(
      uri = result$uri,
      type = paste(result$type, collapse = ", "), # Concatenate types if multiple
      prefLabel = result$prefLabel,
      altLabel = ifelse(!is.null(result$altLabel), result$altLabel, NA),
      hiddenLabel = ifelse(!is.null(result$hiddenLabel), result$hiddenLabel, NA),
      lang = result$lang,
      vocab = result$vocab,
      stringsAsFactors = FALSE
    )
  }))

  return(result_data)
}
