#' Helper function to send requests to the Finto Skosmos API
#'
#' @param endpoint API endpoint to request.
#' @param params List of query parameters.
#' @return Parsed JSON response from the API.
#' @examples
#' \dontrun{
#' finto_api_request("vocabularies")
#' }
#' @export
finto_api_request <- function(endpoint, params = list()) {
  base_url <- "https://api.finto.fi/rest/v1"
  url <- httr::modify_url(base_url, path = paste0("rest/v1/", endpoint),
                          query = params)

  # Print the URL to check if it is correct
  message("Requesting URL: ", url)

  # Send the GET request
  response <- httr::GET(url)

  # Check if the status code is not 200, and stop with a clear message
  if (httr::status_code(response) != 200) {
    stop("API request failed with status: ", httr::status_code(response),
         "\nURL: ", url)
  }

  httr::content(response, as = "parsed", type = "application/json")
}
