# Testing ComparisonOperator
test_that("ComparisonOperator returns correct structure", {
  attribute <- "rcsb_entry_info.resolution_combined"
  value <- 2.0
  comparison_type <- "EQUAL"
  result <- ComparisonOperator(attribute, value, comparison_type)
  expect_equal(result$attribute, attribute)
  expect_equal(result$value, value)
  expect_equal(result$operator, "equals")
  expect_s3_class(result, "ComparisonOperator")
})
