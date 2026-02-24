# Testing return_data_as_dataframe edge cases

test_that("return_data_as_dataframe keeps all rows for unnamed duplicate-key lists", {
  response <- list(
    data = list(
      list(
        list(a = "A1", b = "B1"),
        list(a = "A2", b = "B2")
      )
    )
  )

  result <- return_data_as_dataframe(response, data_type = "ENTRY", ids = c("id1", "id2"))

  expect_true(is.data.frame(result))
  expect_equal(nrow(result), 2)
  expect_true(all(c("A1", "A2") %in% as.vector(unlist(result))))
})

test_that("return_data_as_dataframe does not fail with duplicated ID columns", {
  response <- list(
    data = list(
      list(
        id1 = list(ID = "id1"),
        id2 = list(ID = "id2")
      )
    )
  )

  result <- return_data_as_dataframe(response, data_type = "ENTRY", ids = c("id1", "id2"))

  expect_true(is.data.frame(result))
  expect_equal(nrow(result), 2)
})
