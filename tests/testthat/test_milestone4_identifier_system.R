# Milestone 4: identifier system hardening (additive, non-breaking).

test_that("infer_id_type classifies common identifier formats", {
  ids <- c("4HHB", "4HHB-1", "4HHB_1", "4HHB.A", "4HHB/A", "4HHB:A", "ATP", "")
  inferred <- infer_id_type(ids)

  expect_equal(
    inferred,
    c("ENTRY", "ASSEMBLY", "ENTITY", "INSTANCE", "INSTANCE", "INSTANCE", "CHEMICAL_COMPONENT", "UNKNOWN")
  )
})

test_that("parse_rcsb_id returns structured components", {
  entry <- parse_rcsb_id("4HHB")
  expect_equal(entry$id_type, "ENTRY")
  expect_equal(entry$entry_id, "4HHB")

  assembly <- parse_rcsb_id("4HHB-2")
  expect_equal(assembly$id_type, "ASSEMBLY")
  expect_equal(assembly$entry_id, "4HHB")
  expect_equal(assembly$assembly_id, "2")

  entity <- parse_rcsb_id("4HHB_3")
  expect_equal(entity$id_type, "ENTITY")
  expect_equal(entity$entity_id, "3")

  instance <- parse_rcsb_id("4HHB.B")
  expect_equal(instance$id_type, "INSTANCE")
  expect_equal(instance$instance_id, "B")
  expect_equal(instance$separator, ".")

  chem <- parse_rcsb_id("ATP")
  expect_equal(chem$id_type, "CHEMICAL_COMPONENT")
  expect_equal(chem$entity_id, "ATP")
})

test_that("build_* ID helpers create expected identifiers", {
  expect_equal(build_entry_id(" 4HHB "), "4HHB")
  expect_equal(build_assembly_id("4HHB", 1), "4HHB-1")
  expect_equal(build_entity_id("4HHB", 2), "4HHB_2")
  expect_equal(build_instance_id("4HHB", "A"), "4HHB.A")
  expect_equal(build_instance_id("4HHB", "A", separator = ":"), "4HHB:A")
})

test_that("generate_json_query and fetch/data_fetcher normalize IDs through helper", {
  query <- generate_json_query(
    ids = c(" 4HHB ", " 1CRN "),
    data_type = "ENTRY",
    properties = list(rcsb_id = list())
  )
  expect_match(query, "4HHB")
  expect_match(query, "1CRN")
  expect_false(grepl(" 4HHB | 1CRN ", query))

  local_mocked_bindings(
    search_graphql = function(graphql_json_query, ...) {
      list(data = list(list("4HHB" = list(value = "ok"))))
    },
    .package = "rPDBapi"
  )
  out <- suppressWarnings(fetch_data("query{}", data_type = "ENTRY", ids = factor(" 4HHB ")))
  expect_equal(attr(out, "ids"), "4HHB")

  local_mocked_bindings(
    generate_json_query = function(ids, data_type, properties) {
      expect_equal(ids, c("4HHB", "1CRN"))
      "query{}"
    },
    fetch_data = function(json_query, data_type, ids) {
      expect_equal(ids, c("4HHB", "1CRN"))
      list(data = list(list("4HHB" = list(rcsb_id = "4HHB"), "1CRN" = list(rcsb_id = "1CRN"))))
    },
    return_data_as_dataframe = function(response, data_type, ids) {
      data.frame(rcsb_id = ids, stringsAsFactors = FALSE)
    },
    .package = "rPDBapi"
  )

  df <- data_fetcher(
    id = c(" 4HHB ", " 1CRN "),
    data_type = "ENTRY",
    properties = list(rcsb_id = list()),
    return_as_dataframe = TRUE
  )
  expect_equal(df$rcsb_id, c("4HHB", "1CRN"))
})
