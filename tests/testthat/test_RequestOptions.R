# Testing RequestOptions
test_that("RequestOptions returns correct structure", {
  result <- RequestOptions(result_start_index = 0, num_results = 10, sort_by = "score", desc = TRUE)
  expect_true("paginate" %in% names(result))
  expect_true("sort" %in% names(result))
  expect_equal(result$paginate$start, 0)
  expect_equal(result$paginate$rows, 10)
  expect_equal(result$sort[[1]]$sort_by, "score")
  expect_equal(result$sort[[1]]$direction, "desc")
})
