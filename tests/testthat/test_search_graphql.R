# Testing search_graphql
test_that("search_graphql handles GraphQL queries", {
  graphql_json_query <- list(query = "{entries(entry_ids: [\"4LZA\", \"5RU3\"]){cell {volume, angle_beta}, exptl {method}}}")
  result <- search_graphql(graphql_json_query)
  expect_type(result, "list")
  expect_true("data" %in% names(result))
})
