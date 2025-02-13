#' Extract asteriIDs, clean data, fetch RDF data from the Finto Skosmos API, and filter results
#'
#' This function extracts asteriIDs, fetches RDF data, and ensures only valid rows
#' where `uri` matches the expected format (`http://urn.fi/URN:NBN:fi:au:finaf:<asteriID>`).
#'
#' @param data A dataframe containing 'Code_1000' and 'Code_7001' columns.
#' @return A cleaned tibble with fetched RDF data (limited to correct rows).
#' @import dplyr purrr tibble stringr tidyr
#' @importFrom dplyr mutate select coalesce rowwise filter distinct slice
#' @examples
#' results <- process_and_fetch_kanto_info(my_data)
#' print(results)
#' @export
process_and_fetch_kanto_info <- function(data) {
  # Step 1: Extract numeric asteriIDs from Code_1000 and Code_7001
  data_clean <- data %>%
    dplyr::mutate(
      asteriID_1000 = stringr::str_extract(Code_1000, "\\d{9}"),
      asteriID_7001 = stringr::str_extract(Code_7001, "\\d{9}"),
      asteriID = dplyr::coalesce(asteriID_1000, asteriID_7001)  # Prioritize non-NA asteriID
    ) %>%
    dplyr::select(-asteriID_1000, -asteriID_7001)  # Remove unnecessary columns

  # Step 2: Fetch RDF data for each valid asteriID
  results <- data_clean %>%
    dplyr::rowwise() %>%
    dplyr::mutate(
      rdf_data = if (!is.na(asteriID))
        list(tryCatch(
          fetch_kanto_info(asteriID) %>%
            dplyr::filter(uri == paste0("http://urn.fi/URN:NBN:fi:au:finaf:", asteriID)),  # Only include correct row
          error = function(e) tibble::tibble(uri = NA, type = NA, prefLabel = NA,
                                             description = NA, publisher = NA,
                                             contributor = NA)))
      else list(NULL)
    ) %>%
    tidyr::unnest(cols = c(rdf_data), keep_empty = TRUE)  # Ensure missing rows are preserved

  return(results)
}

# Example usage
#rdf_results <- process_and_fetch_kanto_info(data)
#print(rdf_results)
