#' Suggest subjects for a given text using the Annif REST API
#'
#' This function sends a request to the Annif API to suggest subjects based on the provided input text.
#'
#' @param project_id The project identifier (e.g., "yso-en").
#' @param text The input text for which subjects are to be suggested.
#' @param limit An optional parameter to specify the maximum number of results to return. Default is 10.
#' @param threshold An optional parameter to specify the minimum score threshold for results. Default is 0.
#' @param language An optional parameter to specify the language of subject labels. Default is "en".
#' @return A tibble containing suggested subjects, including their labels, URIs, and scores.
#' @importFrom httr POST status_code content accept
#' @importFrom jsonlite fromJSON
#' @importFrom tibble as_tibble
#' @importFrom purrr map
#' @examples
#' suggestions <- suggest_subjects(project_id = "yso-en",
#' text = "Jean Sibelius orchestra music and composer")
#' print(suggestions)
#' @export
suggest_subjects <- function(project_id, text, limit = 10, threshold = 0, language = "en") {
  # Define the base URL for the Annif API
  base_url <- paste0("https://ai.finto.fi/v1/projects/", project_id, "/suggest")

  # Prepare the body of the POST request
  body <- list(
    text = text,
    limit = limit,
    threshold = threshold,
    language = language
  )

  # Make the API request using httr::POST
  response <- httr::POST(base_url, body = body, encode = "form", httr::accept("application/json"))

  # Check for successful response and handle errors
  if (httr::status_code(response) != 200) {
    stop("Error: ", httr::status_code(response), " - ", httr::content(response, "text"))
  }

  # Parse the JSON response
  json_data <- jsonlite::fromJSON(httr::content(response, "text", encoding = "UTF-8"))

  # Convert the results into a tibble
  suggestions_df <- as_tibble(json_data$results) %>%
    select(label, uri, score) # Select relevant columns

  return(suggestions_df)
}
