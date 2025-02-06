test_that("search_concepts returns a data frame with expected structure", {
  # Run the function with a real API query
  result <- search_concepts(query = "sibelius", lang = "fi", maxhits = 5)

  # Check that the result is a data frame
  expect_s3_class(result, "data.frame")

  # Check that the data frame is not empty
  expect_gt(nrow(result), 0)

  # Check that required columns are present
  expected_columns <- c("uri", "type", "prefLabel", "altLabel", "hiddenLabel", "lang", "vocab")
  expect_named(result, expected_columns, ignore.order = TRUE)

  # Check that some key fields contain non-empty values
  expect_true(all(!is.na(result$uri)))
  expect_true(all(!is.na(result$prefLabel)))

  # Ensure 'type' column correctly concatenates multiple values
  expect_true(all(grepl("skos:Concept", result$type)))
})
