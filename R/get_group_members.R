#' Get members of a specific concept group from the Finto Skosmos API
#'
#' @param vocid The vocabulary ID, e.g., "yso".
#' @param uri The URI of the concept group whose members to retrieve.
#' @param lang The language code for the labels, e.g., "fi" or "en". Optional.
#' @return A tibble containing members associated with the specified concept group.
#' @importFrom httr GET status_code content accept
#' @importFrom jsonlite fromJSON
#' @importFrom dplyr bind_rows mutate select
#' @examples
#' group_members_data <- get_group_members(vocid = "yso",
#' uri = "http://www.yso.fi/onto/yso/p26580", lang = "fi")
#' print(head(group_members_data))
#' @export
get_group_members <- function(vocid, uri, lang = NULL) {
  base_url <- paste0("https://api.finto.fi/rest/v1/", vocid, "/groupMembers")

  # Prepare parameters for the query
  params <- list(
    uri = uri,
    lang = lang
  )

  # Make the GET request to the API
  response <- GET(base_url, query = params, accept("application/ld+json"))

  # Check for a successful response
  if (status_code(response) != 200) {
    stop("Error: ", status_code(response), " - ", content(response, "text"))
  }

  # Parse the response content
  data <- fromJSON(content(response, "text", encoding = "UTF-8"), simplifyDataFrame = TRUE)

  # Check if members exist and are structured correctly
  if (!is.null(data$members) && is.data.frame(data$members)) {
    members_df <- data$members %>%
      mutate(
        type = sapply(type, function(t) paste(t, collapse = ", ")) # Combine types into a single string
      ) %>%
      select(uri, prefLabel, isSuper, hasMembers, type) # Select relevant columns
  } else {
    # Return an empty tibble if no members found
    members_df <- tibble(uri = character(), prefLabel = character(), isSuper = logical(), hasMembers = logical(), type = character())
  }

  return(members_df)
}
