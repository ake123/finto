#' Get information about a specific project from the Annif REST API
#'
#' This function retrieves detailed information about a specified project
#' using the Annif API.
#'
#' @param project_id A string representing the project identifier
#' (e.g., "dummy-fi").
#' @return A tibble containing project details, or an error message if the
#' project is not found.
#' @importFrom httr GET status_code content accept
#' @importFrom jsonlite fromJSON
#' @importFrom tibble as_tibble
#' @examples
#' project_info <- get_annif_project_info(project_id = "yso-fi")
#' print(project_info)
#' @export
get_annif_project_info <- function(project_id) {
  # Define the base URL for the Annif API
  base_url <- paste0("https://ai.finto.fi/v1/projects/", project_id)

  # Make the API request using httr::GET
  response <- httr::GET(base_url, httr::accept("application/json"))

  # Check for successful response and handle errors
  if (httr::status_code(response) == 200) {
    # Parse the JSON response
    json_data <- jsonlite::fromJSON(httr::content(response, "text",
                                                  encoding = "UTF-8"))

    # Convert the project details into a tibble
    project_df <- as_tibble(json_data)

    return(project_df)
  } else if (httr::status_code(response) == 404) {
    # Handle the project not found case
    error_message <- httr::content(response, "text", encoding = "UTF-8")
    stop("Error: Project not found. ", jsonlite::fromJSON(error_message)$detail)
  } else {
    # Handle other errors
    stop("Error: ", httr::status_code(response), " - ",
         httr::content(response, "text"))
  }
}
