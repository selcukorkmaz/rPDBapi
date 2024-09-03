# Testing query_search
test_that("query_search returns correct PDB IDs for search term", {
  result <- query_search("ribosome")

  expect_type(result, "character")
  expect_true(length(result) > 0)
})

test_that("query_search handles invalid search type gracefully", {
  expect_error(query_search("ribosome", query_type = "invalid_type"), "Unsupported Query Type")
})
