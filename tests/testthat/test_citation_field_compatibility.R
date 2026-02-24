# Testing citation field compatibility across payload variants

test_that("find_results default citation field falls back to rcsb_primary_citation", {
  local_mocked_bindings(
    query_search = function(search_term, ...) {
      c("4HHB")
    },
    get_info = function(pdb_id, ...) {
      list(rcsb_primary_citation = list(title = "Modern Title"))
    },
    .package = "rPDBapi"
  )

  result <- find_results("hemoglobin")

  expect_true("4HHB" %in% names(result))
  expect_equal(result$`4HHB`$title, "Modern Title")
})

test_that("find_results rcsb_primary_citation field falls back to legacy citation", {
  local_mocked_bindings(
    query_search = function(search_term, ...) {
      c("4HHB")
    },
    get_info = function(pdb_id, ...) {
      list(citation = list(title = "Legacy Title"))
    },
    .package = "rPDBapi"
  )

  result <- find_results("hemoglobin", field = "rcsb_primary_citation")

  expect_true("4HHB" %in% names(result))
  expect_equal(result$`4HHB`$title, "Legacy Title")
})

test_that("find_papers reads title from rcsb_primary_citation", {
  local_mocked_bindings(
    query_search = function(search_term, ...) {
      c("4HHB")
    },
    get_info = function(pdb_id, ...) {
      list(rcsb_primary_citation = list(title = "Modern Title"))
    },
    .package = "rPDBapi"
  )

  result <- find_papers("hemoglobin", max_results = 1)

  expect_true("4HHB" %in% names(result))
  expect_equal(result$`4HHB`, "Modern Title")
})

test_that("find_papers remains compatible with legacy citation field", {
  local_mocked_bindings(
    query_search = function(search_term, ...) {
      c("4HHB")
    },
    get_info = function(pdb_id, ...) {
      list(citation = list(title = "Legacy Title"))
    },
    .package = "rPDBapi"
  )

  result <- find_papers("hemoglobin", max_results = 1)

  expect_true("4HHB" %in% names(result))
  expect_equal(result$`4HHB`, "Legacy Title")
})
