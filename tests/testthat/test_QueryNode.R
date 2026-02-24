# Testing QueryNode
test_that("QueryNode returns correct structure for terminal node", {
  search_operator <- DefaultOperator("4HHB")
  result <- QueryNode(search_operator)
  expect_equal(result$type, "terminal")
  expect_equal(result$service, infer_search_service(search_operator))
  expect_equal(result$parameters, search_operator)
})

test_that("QueryNode returns correct structure for group node", {
  queries <- list(DefaultOperator("4HHB"), ExactMatchOperator("rcsb_entry_info.resolution_combined", "2.0"))
  query_group <- QueryGroup(queries, "AND")
  result <- QueryNode(query_group)
  expect_equal(result$type, "group")
  expect_equal(result$logical_operator, query_group$logical_operator)
  expect_equal(result$nodes, query_group$nodes)
})

test_that("QueryNode derives group logical operator from argument when missing", {
  query_group <- list(
    type = "group",
    nodes = list(QueryNode(DefaultOperator("4HHB")))
  )

  result <- QueryNode(query_group, logical_operator = "OR")

  expect_equal(result$type, "group")
  expect_equal(result$logical_operator, "or")
  expect_equal(result$nodes, query_group$nodes)
})
