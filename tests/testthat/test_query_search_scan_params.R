# Testing query_search scan_params behavior

test_that("query_search applies scan_params request_options overrides", {
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

  result <- query_search(
    search_term = "ribosome",
    scan_params = list(
      request_options = list(
        paginate = list(start = 5, rows = 2),
        return_all_hits = FALSE
      )
    ),
    num_attempts = 1,
    sleep_time = 0
  )

  expect_equal(as.character(result), "4HHB")
  expect_equal(captured_request$body$request_options$paginate$start, 5)
  expect_equal(captured_request$body$request_options$paginate$rows, 2)
  expect_false(captured_request$body$request_options$return_all_hits)
})

test_that("query_search uses scan_params query override when provided", {
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

  result <- query_search(
    search_term = "ignored",
    query_type = "sequence",
    scan_params = list(
      query = list(
        type = "terminal",
        service = "text",
        parameters = list(
          operator = "exact_match",
          attribute = "rcsb_id",
          value = "4HHB"
        )
      )
    ),
    num_attempts = 1,
    sleep_time = 0
  )

  expect_equal(as.character(result), "4HHB")
  expect_equal(captured_request$body$query$service, "text")
  expect_equal(captured_request$body$query$parameters$attribute, "rcsb_id")
})

test_that("query_search validates scan_params type", {
  expect_error(
    query_search("ribosome", scan_params = "invalid"),
    "Invalid scan_params"
  )
})
