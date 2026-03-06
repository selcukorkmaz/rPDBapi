# Milestone 5: schema-aware property layer (additive, strict-mode gated).

test_that("list_rcsb_fields returns schema rows and supports data_type filters", {
  all_fields <- list_rcsb_fields()
  expect_s3_class(all_fields, "data.frame")
  expect_true(all(c("data_type", "field", "subfield") %in% names(all_fields)))
  expect_true(any(all_fields$data_type == "ENTRY"))

  entry_fields <- list_rcsb_fields("ENTRY")
  expect_true(all(entry_fields$data_type == "ENTRY"))

  expect_error(
    list_rcsb_fields("UNKNOWN_TYPE"),
    class = "rPDBapi_error_invalid_input"
  )
})

test_that("search_rcsb_fields filters known field registry", {
  res <- search_rcsb_fields("resolution")
  expect_s3_class(res, "data.frame")
  expect_true(nrow(res) >= 1)
  expect_true(any(grepl("resolution", res$subfield, ignore.case = TRUE)))
})

test_that("validate_properties supports strict and non-strict modes", {
  valid_props <- list(
    rcsb_id = list(),
    rcsb_entry_info = c("resolution_combined")
  )

  expect_invisible(validate_properties(valid_props, data_type = "ENTRY", strict = TRUE))

  non_strict <- validate_properties(
    list(rcsb_entry_info = c("resolution_combined", "unknown_sub")),
    data_type = "ENTRY",
    strict = FALSE
  )
  expect_true("unknown_subfields" %in% names(non_strict))
  expect_equal(non_strict$unknown_subfields$rcsb_entry_info, "unknown_sub")

  expect_error(
    validate_properties(list(unknown_field = c("x")), data_type = "ENTRY", strict = TRUE),
    class = "rPDBapi_error_invalid_input"
  )

  expect_error(
    validate_properties(list(rcsb_entry_info = c("unknown_sub")), data_type = "ENTRY", strict = TRUE),
    class = "rPDBapi_error_invalid_input"
  )
})

test_that("generate_json_query strict validation is option-gated and non-breaking by default", {
  bad_props <- list(unknown_field = c("x"))

  # Default behavior should remain permissive.
  expect_no_error(
    generate_json_query(ids = "4HHB", data_type = "ENTRY", properties = bad_props)
  )

  old_opt <- options(rPDBapi.strict_property_validation = TRUE)
  on.exit(options(old_opt), add = TRUE)
  expect_error(
    generate_json_query(ids = "4HHB", data_type = "ENTRY", properties = bad_props),
    class = "rPDBapi_error_invalid_input"
  )
})

test_that("data_fetcher respects strict property validation option through generate_json_query", {
  old_opt <- options(rPDBapi.strict_property_validation = TRUE)
  on.exit(options(old_opt), add = TRUE)

  expect_error(
    data_fetcher(
      id = "4HHB",
      data_type = "ENTRY",
      properties = list(unknown_field = c("x")),
      return_as_dataframe = FALSE
    ),
    class = "rPDBapi_error_invalid_input"
  )
})
