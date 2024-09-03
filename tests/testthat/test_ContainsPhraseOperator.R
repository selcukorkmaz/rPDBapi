# Testing ContainsPhraseOperator
test_that("ContainsPhraseOperator returns correct structure", {
  attribute <- "rcsb_primary_citation.title"
  value <- "molecular dynamics"
  result <- ContainsPhraseOperator(attribute, value)
  expect_equal(result$attribute, attribute)
  expect_equal(result$value, value)
  expect_equal(result$operator, "contains_phrase")
  expect_s3_class(result, "ContainsPhraseOperator")
})
