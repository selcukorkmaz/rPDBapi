# Testing perform_search
test_that("perform_search returns correct search results", {
  search_operator <- DefaultOperator("4HHB")
  result <- perform_search(search_operator)

  expect_type(result, "character")
  expect_true(length(result) > 0)
  expect_true(any(grepl("4HHB", unlist(result))))
})
