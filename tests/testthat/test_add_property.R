# Testing add_property

test_that("add_property merges duplicate property names and removes duplicates", {
  properties <- list(
    cell = c("length_a"),
    cell = c("length_b", "length_a"),
    exptl = list("method")
  )

  result <- add_property(properties)

  expect_true("cell" %in% names(result))
  expect_true("exptl" %in% names(result))
  expect_setequal(result$cell, c("length_a", "length_b"))
  expect_equal(result$exptl, "method")
})

test_that("add_property validates input type and names", {
  expect_error(add_property("not-a-list"), "Property must be a list")
  expect_error(add_property(list(c("length_a"))), "Each property must be a named list element")
})

test_that("add_property supports empty lists", {
  expect_equal(add_property(list()), list())
})
