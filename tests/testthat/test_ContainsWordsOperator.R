# Testing ContainsWordsOperator
test_that("ContainsWordsOperator returns correct structure", {
  attribute <- "rcsb_primary_citation.title"
  value <- "crystal structure"
  result <- ContainsWordsOperator(attribute, value)
  expect_equal(result$attribute, attribute)
  expect_equal(result$value, value)
  expect_equal(result$operator, "contains_words")
  expect_s3_class(result, "ContainsWordsOperator")
})
