# Testing RangeOperator
test_that("RangeOperator returns correct structure", {
  attribute <- "rcsb_entry_info.resolution_combined"
  from_value <- 1.5
  to_value <- 2.5
  result <- RangeOperator(attribute, from_value, to_value)
  expect_equal(result$attribute, attribute)
  expect_equal(result$value$from, from_value)
  expect_equal(result$value$to, to_value)
  expect_equal(result$operator, "range")
  expect_s3_class(result, "RangeOperator")
})
