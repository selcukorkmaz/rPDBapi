# Testing data_fetcher
test_that("data_fetcher returns correct structure", {
  properties <- list(
    rcsb_id = list(),
    chem_comp = list("type", "formula_weight", "name", "formula"),
    rcsb_chem_comp_info = list("initial_release_date")
  )

  ids <- c("NAG", "EBW")

  result <- data_fetcher(
    id = ids,
    data_type = "CHEMICAL_COMPONENT",
    properties = properties,
    return_as_dataframe = TRUE
  )

  expect_type(result, "list")
  expect_true("rcsb_id" %in% names(result))
  expect_equal(result$rcsb_id, ids)
})
