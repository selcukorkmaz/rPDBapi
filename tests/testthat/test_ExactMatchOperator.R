# Testing ExactMatchOperator
test_that("ExactMatchOperator returns correct structure", {
  attribute <- "rcsb_entry_info.resolution_combined"
  value <- "2.0"
  result <- ExactMatchOperator(attribute, value)
  expect_equal(result$attribute, attribute)
  expect_equal(result$value, value)
  expect_equal(result$operator, "exact_match")
  expect_s3_class(result, "ExactMatchOperator")
})
