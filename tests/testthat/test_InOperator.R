# Testing InOperator
test_that("InOperator returns correct structure", {
  attribute <- "rcsb_entity_source_organism.taxonomy_lineage.name"
  value <- c("Homo sapiens", "Mus musculus")
  result <- InOperator(attribute, value)
  expect_equal(result$attribute, attribute)
  expect_equal(result$value, value)
  expect_equal(result$operator, "in")
  expect_s3_class(result, "InOperator")
})
