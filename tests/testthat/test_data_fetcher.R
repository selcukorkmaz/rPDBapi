# Testing data_fetcher
test_that("data_fetcher returns correct structure", {
  properties <- list(
    rcsb_id = list(),
    chem_comp = list("type", "formula_weight", "name", "formula"),
    rcsb_chem_comp_info = list("initial_release_date")
  )

  ids <- c("NAG", "EBW")

  local_mocked_bindings(
    generate_json_query = function(ids, data_type, properties) {
      "mock_query"
    },
    fetch_data = function(json_query, data_type, ids) {
      list(data = list(list(
        NAG = list(rcsb_id = "NAG"),
        EBW = list(rcsb_id = "EBW")
      )))
    },
    return_data_as_dataframe = function(response, data_type, ids) {
      data.frame(rcsb_id = ids, stringsAsFactors = FALSE)
    },
    .package = "rPDBapi"
  )

  result <- data_fetcher(
    id = ids,
    data_type = "CHEMICAL_COMPONENT",
    properties = properties,
    return_as_dataframe = TRUE
  )

  expect_s3_class(result, "data.frame")
  expect_true("rcsb_id" %in% names(result))
  expect_equal(result$rcsb_id, ids)
})
