# Testing perform_search return_type validation and mapping

capture_search_api_return_type <- function(return_type) {
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

  result <- perform_search(DefaultOperator("4HHB"), return_type = return_type, verbosity = FALSE)

  list(result = result, api_return_type = captured_request$body$return_type)
}

test_that("perform_search maps supported return types to API return_type values", {
  expect_equal(capture_search_api_return_type("ENTRY")$api_return_type, "entry")
  expect_equal(capture_search_api_return_type("ASSEMBLY")$api_return_type, "assembly")
  expect_equal(capture_search_api_return_type("POLYMER_ENTITY")$api_return_type, "polymer_entity")
  expect_equal(capture_search_api_return_type("NONPOLYMER_ENTITY")$api_return_type, "non_polymer_entity")
  expect_equal(capture_search_api_return_type("NON_POLYMER_ENTITY")$api_return_type, "non_polymer_entity")
  expect_equal(capture_search_api_return_type("POLYMER_INSTANCE")$api_return_type, "polymer_instance")
  expect_equal(capture_search_api_return_type("MOL_DEFINITION")$api_return_type, "mol_definition")
  expect_equal(capture_search_api_return_type("CHEMICAL_COMPONENT")$api_return_type, "mol_definition")
})

test_that("perform_search rejects unsupported unmapped return types", {
  expect_error(
    perform_search(DefaultOperator("4HHB"), return_type = "NONPOLYMER_INSTANCE", verbosity = FALSE),
    "Invalid return_type"
  )
})

test_that("ReturnType mapping contains no NULL values for accepted keys", {
  null_mappings <- vapply(names(ReturnType), function(type_name) {
    is.null(ReturnType[[type_name]])
  }, logical(1))

  expect_false(any(null_mappings))
})
