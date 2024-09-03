# Testing QueryGroup
test_that("QueryGroup returns correct structure", {
  queries <- list(DefaultOperator("4HHB"), ExactMatchOperator("rcsb_entry_info.resolution_combined", "2.0"))
  logical_operator <- "AND"
  result <- QueryGroup(queries, logical_operator)
  expect_equal(result$type, "group")
  expect_equal(length(result$nodes), length(queries))
})
