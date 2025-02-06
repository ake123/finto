#' Get basic information about the Annif REST API
#'
#' This function retrieves basic information such as the title and version of
#' the Annif API.
#'
#' @return A list containing the title and version of the API.
#' @importFrom httr GET status_code content accept
#' @importFrom jsonlite fromJSON
#' @examples
#' api_info <- get_annif_api_info()
#' print(api_info)
#' @export
get_annif_api_info <- function() {
  # Define the base URL for the Annif API
  base_url <- "https://ai.finto.fi/v1/"

  # Make the API request using httr::GET
  response <- httr::GET(base_url, httr::accept("application/json"))

  # Check for successful response and handle errors
  if (httr::status_code(response) != 200) {
    stop("Error: ", httr::status_code(response), " - ", httr::content(response,
                                                                      "text"))
  }

  # Parse the JSON response and return it as a list
  api_info <- jsonlite::fromJSON(httr::content(response, "text",
                                               encoding = "UTF-8"))

  return(api_info)
}
