# Testing API contracts and typed errors for core functions

test_that("query_search returns contract-tagged identifier vectors", {
  local_mocked_bindings(
    POST = function(url, body, encode, ...) structure(list(), class = "response"),
    http_status = function(response) list(category = "Success", status = 200, message = "OK"),
    content = function(response, as = "text", encoding = "UTF-8", ...) {
      "{\"result_set\":[{\"identifier\":\"4HHB\"}]}"
    },
    .package = "rPDBapi"
  )

  result <- query_search("hemoglobin", num_attempts = 1, sleep_time = 0)
  expect_s3_class(result, "rPDBapi_query_ids")
  expect_equal(attr(result, "return_type"), "entry")
})

test_that("query_search raises typed malformed-response errors", {
  local_mocked_bindings(
    POST = function(url, body, encode, ...) structure(list(), class = "response"),
    http_status = function(response) list(category = "Success", status = 200, message = "OK"),
    content = function(response, as = "text", encoding = "UTF-8", ...) {
      "{\"result_set\":[{\"id\":\"4HHB\"}]}"
    },
    .package = "rPDBapi"
  )

  expect_error(
    query_search("hemoglobin", num_attempts = 1, sleep_time = 0),
    class = "rPDBapi_error_malformed_response"
  )
})

test_that("perform_search returns contract-tagged results and typed mapping errors", {
  local_mocked_bindings(
    POST = function(url, body, encode, ...) structure(list(), class = "response"),
    http_status = function(response) list(category = "Success", status = 200, message = "OK"),
    content = function(response, as = "text", encoding = "UTF-8", ...) {
      "{\"result_set\":[{\"identifier\":\"4HHB\"}]}"
    },
    .package = "rPDBapi"
  )

  result <- perform_search(DefaultOperator("4HHB"), verbosity = FALSE)
  expect_s3_class(result, "rPDBapi_search_ids")

  expect_error(
    perform_search(DefaultOperator("4HHB"), return_type = "NONPOLYMER_INSTANCE", verbosity = FALSE),
    class = "rPDBapi_error_unsupported_mapping"
  )
})

test_that("perform_search raises typed malformed-response errors", {
  local_mocked_bindings(
    POST = function(url, body, encode, ...) structure(list(), class = "response"),
    http_status = function(response) list(category = "Success", status = 200, message = "OK"),
    content = function(response, as = "text", encoding = "UTF-8", ...) {
      "{\"unexpected\":[]}"
    },
    .package = "rPDBapi"
  )

  expect_error(
    perform_search(DefaultOperator("4HHB"), verbosity = FALSE),
    class = "rPDBapi_error_malformed_response"
  )
})

test_that("fetch_data and data_fetcher return contract-tagged objects", {
  local_mocked_bindings(
    search_graphql = function(graphql_json_query, ...) {
      list(data = list(list(id1 = list(value = "v1"))))
    },
    .package = "rPDBapi"
  )
  fetch_result <- fetch_data("query{}", data_type = "ENTRY", ids = c("id1"))
  expect_s3_class(fetch_result, "rPDBapi_fetch_response")
  expect_equal(attr(fetch_result, "ids"), "id1")

  local_mocked_bindings(
    generate_json_query = function(ids, data_type, properties) "query{}",
    fetch_data = function(json_query, data_type, ids) {
      structure(list(data = list(list(id1 = list(rcsb_id = "id1")))), class = c("rPDBapi_fetch_response", "list"))
    },
    return_data_as_dataframe = function(response, data_type, ids) data.frame(rcsb_id = ids, stringsAsFactors = FALSE),
    .package = "rPDBapi"
  )

  df_result <- data_fetcher(
    id = "id1",
    data_type = "ENTRY",
    properties = list(rcsb_id = list()),
    return_as_dataframe = TRUE
  )
  raw_result <- data_fetcher(
    id = "id1",
    data_type = "ENTRY",
    properties = list(rcsb_id = list()),
    return_as_dataframe = FALSE
  )

  expect_s3_class(df_result, "rPDBapi_dataframe")
  expect_s3_class(raw_result, "rPDBapi_fetch_response")
})

test_that("fetch_data contract emits explicit warnings/errors for normalization issues", {
  local_mocked_bindings(
    search_graphql = function(graphql_json_query, ...) {
      list(data = list(list(list(value = "v1"))))
    },
    .package = "rPDBapi"
  )

  expect_warning(
    fetch_data("query{}", data_type = "ENTRY", ids = c("id1")),
    "normalized by position"
  )

  local_mocked_bindings(
    search_graphql = function(graphql_json_query, ...) list(),
    .package = "rPDBapi"
  )

  expect_error(
    fetch_data("query{}", data_type = "ENTRY", ids = c("id1")),
    class = "rPDBapi_error_malformed_response"
  )
})
