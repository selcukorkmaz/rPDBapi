# Testing DefaultOperator
test_that("DefaultOperator returns correct structure", {
  value <- "4HHB"
  result <- DefaultOperator(value)
  expect_equal(result$value, value)
  expect_s3_class(result, "DefaultOperator")
})
