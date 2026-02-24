# Testing request body encoding behavior

test_that("query_search sends request body as structured data", {
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

  result <- query_search("ribosome", num_attempts = 1, sleep_time = 0)

  expect_equal(as.character(result), "4HHB")
  expect_equal(captured_request$encode, "json")
  expect_type(captured_request$body, "list")
  expect_equal(captured_request$body$return_type, "entry")
  expect_equal(captured_request$body$query$service, "full_text")
})

test_that("perform_search sends request body as structured data", {
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

  result <- perform_search(DefaultOperator("4HHB"), verbosity = FALSE)

  expect_equal(as.character(result), "4HHB")
  expect_equal(captured_request$encode, "json")
  expect_type(captured_request$body, "list")
  expect_equal(captured_request$body$return_type, "entry")
  expect_equal(captured_request$body$query$type, "terminal")
})
