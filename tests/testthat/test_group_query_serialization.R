# Testing group query serialization

test_that("perform_search serializes grouped query with logical_operator and nodes", {
  captured_request <- NULL

  local_mocked_bindings(
    POST = function(url, body, encode, ...) {
      captured_request <<- list(url = url, body = body, encode = encode)
      structure(list(), class = "response")
    },
    http_status = function(response) {
      list(category = "Success", status = 200, message = "OK")
    },
    content = function(response, as = "text", encoding = "UTF-8", ...) {
      "{\"result_set\":[{\"identifier\":\"4HHB\"}]}"
    },
    .package = "rPDBapi"
  )

  query_group <- QueryGroup(
    queries = list(
      DefaultOperator("4HHB"),
      ExactMatchOperator("rcsb_id", "4HHB")
    ),
    logical_operator = "AND"
  )

  result <- perform_search(query_group, verbosity = FALSE)

  expect_equal(as.character(result), "4HHB")
  expect_equal(captured_request$encode, "json")
  expect_equal(captured_request$body$query$type, "group")
  expect_equal(captured_request$body$query$logical_operator, "and")
  expect_equal(length(captured_request$body$query$nodes), 2)
})
