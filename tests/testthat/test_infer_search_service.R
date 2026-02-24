# Testing infer_search_service

test_that("infer_search_service routes StructureOperator to structure", {
  search_operator <- StructureOperator("4HHB")
  result <- infer_search_service(search_operator)

  expect_equal(result, "structure")
})

test_that("infer_search_service keeps text operators routed to text", {
  search_operator <- ExactMatchOperator("rcsb_id", "4HHB")
  result <- infer_search_service(search_operator)

  expect_equal(result, "text")
})

test_that("infer_search_service supports raw text operator lists", {
  search_operator <- list(
    attribute = "rcsb_id",
    operator = "exact_match",
    value = "4HHB"
  )
  result <- infer_search_service(search_operator)

  expect_equal(result, "text")
})

test_that("QueryNode routes StructureOperator nodes to structure service", {
  search_operator <- StructureOperator("4HHB")
  node <- QueryNode(search_operator)

  expect_equal(node$type, "terminal")
  expect_equal(node$service, "structure")
})
