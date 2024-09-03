# Testing find_papers
test_that("find_papers returns correct paper titles", {
  result <- find_papers("ribosome", max_results = 2)

  expect_type(result, "list")
  expect_true(length(result) > 0)
  expect_type(result[[1]], "character")
})

# Testing find_results
test_that("find_results returns correct field information", {
  result <- find_results("crispr", field = "citation")

  expect_type(result, "list")
  expect_true(length(result) > 0)
  expect_type(result[[1]], "list")
  expect_true("title" %in% names(result[[1]]))
})
