# Testing fetch_data response normalization

test_that("fetch_data keeps requested IDs in requested order for named payloads", {
  local_mocked_bindings(
    search_graphql = function(graphql_json_query, ...) {
      list(
        data = list(
          list(
            id2 = list(value = "second"),
            id1 = list(value = "first"),
            extra = list(value = "extra")
          )
        )
      )
    },
    .package = "rPDBapi"
  )

  response <- suppressWarnings(fetch_data("query{}", data_type = "ENTRY", ids = c("id1", "id2"))
  )

  expect_equal(names(response$data[[1]]), c("id1", "id2"))
  expect_equal(response$data[[1]]$id1$value, "first")
  expect_equal(response$data[[1]]$id2$value, "second")
})

test_that("fetch_data assigns IDs positionally for unnamed payloads", {
  local_mocked_bindings(
    search_graphql = function(graphql_json_query, ...) {
      list(
        data = list(
          list(
            list(value = "first"),
            list(value = "second")
          )
        )
      )
    },
    .package = "rPDBapi"
  )

  response <- suppressWarnings(fetch_data("query{}", data_type = "ENTRY", ids = c("id1", "id2"))
  )

  expect_equal(names(response$data[[1]]), c("id1", "id2"))
})

test_that("fetch_data errors when requested IDs are missing in named payloads", {
  local_mocked_bindings(
    search_graphql = function(graphql_json_query, ...) {
      list(
        data = list(
          list(
            id1 = list(value = "first")
          )
        )
      )
    },
    .package = "rPDBapi"
  )

  expect_error(
    suppressWarnings(fetch_data("query{}", data_type = "ENTRY", ids = c("id1", "id2"))),
    "One or more IDs could not be retrieved"
  )
})
