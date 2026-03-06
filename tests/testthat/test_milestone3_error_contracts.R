# Milestone 3: typed error and warning contract hardening.

test_that("send_api_request emits typed network errors", {
  local_mocked_bindings(
    GET = function(url) stop("timeout"),
    .package = "rPDBapi"
  )

  expect_error(
    send_api_request("https://example.org", method = "GET", verbosity = FALSE),
    class = "rPDBapi_error_network"
  )
})

test_that("handle_api_errors emits typed http errors", {
  local_mocked_bindings(
    http_status = function(response) list(category = "Client error", status = 404, message = "Not Found"),
    .package = "rPDBapi"
  )

  expect_error(
    handle_api_errors(structure(list(), class = "response"), "https://example.org"),
    class = "rPDBapi_error_http"
  )
})

test_that("parse_response emits typed input and parsing errors", {
  expect_error(
    parse_response(structure(list(), class = "response"), format = "xml"),
    class = "rPDBapi_error_invalid_input"
  )

  local_mocked_bindings(
    content = function(response, as = "text", encoding = "UTF-8") "{bad json",
    fromJSON = function(txt) stop("parse failed"),
    .package = "rPDBapi"
  )

  expect_error(
    parse_response(structure(list(), class = "response"), format = "json"),
    class = "rPDBapi_error_malformed_response"
  )
})

test_that("search_graphql emits typed http error on unsuccessful status", {
  local_mocked_bindings(
    rpdbapi_http_request = function(url, method = "POST", body = NULL, encode = "json", content_type_value = "application/json") {
      structure(list(), class = "response")
    },
    rpdbapi_http_success = function(response) FALSE,
    rpdbapi_response_text = function(response) "bad request",
    rpdbapi_http_status_code = function(response) 400,
    rpdbapi_http_status_message = function(response) "Bad Request",
    .package = "rPDBapi"
  )

  expect_error(
    suppressWarnings(search_graphql(list(query = "broken"))),
    class = "rPDBapi_error_http"
  )
})

test_that("query_search emits typed request-failed error after retries", {
  local_mocked_bindings(
    rpdbapi_http_request = function(url, method = "POST", body = NULL, encode = "json", content_type_value = "application/json") {
      stop("network down")
    },
    .package = "rPDBapi"
  )

  expect_error(
    suppressWarnings(query_search("kinase", num_attempts = 2, sleep_time = 0)),
    class = "rPDBapi_error_request_failed"
  )
})

test_that("perform_search emits typed network and http errors", {
  local_mocked_bindings(
    rpdbapi_http_request = function(url, method = "POST", body = NULL, encode = "json", content_type_value = "application/json") {
      stop("network down")
    },
    .package = "rPDBapi"
  )
  expect_error(
    perform_search(DefaultOperator("4HHB"), verbosity = FALSE),
    class = "rPDBapi_error_network"
  )

  local_mocked_bindings(
    rpdbapi_http_request = function(url, method = "POST", body = NULL, encode = "json", content_type_value = "application/json") {
      structure(list(), class = "response")
    },
    rpdbapi_http_success = function(response) FALSE,
    rpdbapi_http_status_code = function(response) 500,
    rpdbapi_http_status_message = function(response) "Internal Server Error",
    rpdbapi_response_text = function(response) "server error",
    .package = "rPDBapi"
  )
  expect_error(
    perform_search(DefaultOperator("4HHB"), verbosity = FALSE),
    class = "rPDBapi_error_http"
  )
})

test_that("data_fetcher wraps build/fetch/format failures with typed errors", {
  local_mocked_bindings(
    generate_json_query = function(ids, data_type, properties) stop("bad props"),
    .package = "rPDBapi"
  )
  expect_error(
    data_fetcher(id = "4HHB", data_type = "ENTRY", properties = list(rcsb_id = list())),
    class = "rPDBapi_error_invalid_input"
  )

  local_mocked_bindings(
    generate_json_query = function(ids, data_type, properties) "query{}",
    fetch_data = function(json_query, data_type, ids) stop("boom"),
    .package = "rPDBapi"
  )
  expect_error(
    data_fetcher(id = "4HHB", data_type = "ENTRY", properties = list(rcsb_id = list())),
    class = "rPDBapi_error_request_failed"
  )

  local_mocked_bindings(
    generate_json_query = function(ids, data_type, properties) "query{}",
    fetch_data = function(json_query, data_type, ids) list(data = list(list("4HHB" = list(rcsb_id = "4HHB")))),
    return_data_as_dataframe = function(response, data_type, ids) stop("flatten failed"),
    .package = "rPDBapi"
  )
  expect_error(
    data_fetcher(id = "4HHB", data_type = "ENTRY", properties = list(rcsb_id = list())),
    class = "rPDBapi_error_malformed_response"
  )
})

test_that("get_info and describe_chemical emit typed errors on invalid/network paths", {
  local_mocked_bindings(
    send_api_request = function(url, method = "GET", body = NULL, encode = "json", content_type = "application/json", verbosity = TRUE) {
      stop("timeout")
    },
    .package = "rPDBapi"
  )
  expect_error(
    get_info("4HHB"),
    class = "rPDBapi_error_network"
  )

  expect_error(
    describe_chemical("TOOLONG"),
    class = "rPDBapi_error_invalid_input"
  )
})
