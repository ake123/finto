#' Get a list of projects from the Annif REST API
#'
#' This function retrieves a list of projects available in the Annif API.
#'
#' @return A tibble containing project details, including project ID, name,
#' language, backend ID,
#'         training status, and modification time.
#' @importFrom httr GET status_code content accept
#' @importFrom jsonlite fromJSON
#' @importFrom tibble as_tibble
#' @examples
#' projects <- get_annif_projects()
#' print(projects)
#' @export
get_annif_projects <- function() {
  # Define the base URL for the Annif API
  base_url <- "https://ai.finto.fi/v1/projects"

  # Make the API request using httr::GET
  response <- httr::GET(base_url, httr::accept("application/json"))

  # Check for successful response and handle errors
  if (httr::status_code(response) != 200) {
    stop("Error: ", httr::status_code(response), " - ", httr::content(response, "text"))
  }

  # Parse the JSON response
  json_data <- jsonlite::fromJSON(httr::content(response, "text", encoding = "UTF-8"))

  # Convert the list of projects into a tibble
  projects_df <- as_tibble(json_data$projects)

  return(projects_df)
}
