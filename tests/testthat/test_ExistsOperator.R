# Testing ExistsOperator
test_that("ExistsOperator returns correct structure", {
  attribute <- "rcsb_primary_citation.doi"
  result <- ExistsOperator(attribute)
  expect_equal(result$attribute, attribute)
  expect_equal(result$operator, "exists")
  expect_s3_class(result, "ExistsOperator")
})
