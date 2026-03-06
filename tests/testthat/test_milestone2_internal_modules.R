# Milestone 2: internal architecture split coverage.

test_that("internal HTTP helper dispatches methods and validates unsupported methods", {
  local_mocked_bindings(
    GET = function(url) list(method = "GET", url = url),
    POST = function(url, body = NULL, encode = "json", content_type = NULL) {
      list(method = "POST", url = url, body = body, encode = encode, content_type = content_type)
    },
    content_type = function(x) paste0("ct:", x),
    .package = "rPDBapi"
  )

  get_resp <- rPDBapi:::rpdbapi_http_request("https://example.org/get", method = "GET")
  expect_equal(get_resp$method, "GET")

  post_resp <- rPDBapi:::rpdbapi_http_request(
    "https://example.org/post",
    method = "POST",
    body = list(a = 1),
    encode = "json",
    content_type_value = "application/json"
  )
  expect_equal(post_resp$method, "POST")
  expect_equal(post_resp$content_type, "ct:application/json")

  expect_error(
    rPDBapi:::rpdbapi_http_request("https://example.org", method = "PUT"),
    "Unsupported HTTP method"
  )
})

test_that("internal query type resolver and builders preserve mapping contracts", {
  pubmed <- rPDBapi:::rpdbapi_resolve_query_type(1234, "PubmedIdQuery")
  expect_equal(pubmed$query_type, "text")
  expect_equal(pubmed$query_subtype, "pmid")

  exp <- rPDBapi:::rpdbapi_resolve_query_type("x-ray diffraction", "ExpTypeQuery")
  expect_equal(exp$query_subtype, "experiment_type")
  expect_equal(exp$search_term, "X-RAY DIFFRACTION")

  expect_error(
    rPDBapi:::rpdbapi_resolve_query_type("unknown", "ExpTypeQuery"),
    "Invalid Experimental Method"
  )

  qp <- rPDBapi:::rpdbapi_build_query_params("kinase", "full_text")
  expect_equal(qp$type, "terminal")
  expect_equal(qp$service, "full_text")
  expect_equal(qp$parameters$value, "kinase")

  expect_error(
    rPDBapi:::rpdbapi_build_query_params("kinase", "unsupported"),
    "Unsupported Query Type"
  )
})

test_that("internal query subtype application and scan param merge remain stable", {
  qp <- list(type = "terminal", service = "text", parameters = list(value = "x"))
  qp2 <- rPDBapi:::rpdbapi_apply_query_subtype(qp, "taxid", "9606")
  expect_equal(qp2$parameters$operator, "exact_match")
  expect_equal(qp2$parameters$attribute, "rcsb_entity_source_organism.taxonomy_lineage.id")

  merged <- rPDBapi:::rpdbapi_build_scan_params(
    query_params = qp2,
    return_type = "entry",
    scan_params = list(request_options = list(return_all_hits = FALSE))
  )
  expect_equal(merged$return_type, "entry")
  expect_false(merged$request_options$return_all_hits)

  defaulted <- rPDBapi:::rpdbapi_build_scan_params(
    query_params = qp2,
    return_type = "entry",
    scan_params = NULL
  )
  expect_true(defaulted$request_options$return_all_hits)
})

test_that("internal perform_search helpers validate return type and extract results", {
  expect_error(
    rPDBapi:::rpdbapi_validate_perform_return_type("NONPOLYMER_INSTANCE"),
    class = "rPDBapi_error_unsupported_mapping"
  )

  valid <- rPDBapi:::rpdbapi_build_search_request(
    cast_query_object = list(type = "terminal", service = "full_text", parameters = list(value = "kinase")),
    return_type = "ENTRY",
    request_options = NULL
  )
  expect_equal(valid$return_type, "entry")
  expect_true(valid$request_options$return_all_hits)

  json <- list(result_set = data.frame(identifier = c("4HHB", "1CRN"), score = c(1, 0.5)))
  ids <- rPDBapi:::rpdbapi_extract_search_results(json, return_with_scores = FALSE, return_raw_json_dict = FALSE)
  expect_s3_class(ids, "rPDBapi_search_ids")
  expect_equal(as.character(ids), c("4HHB", "1CRN"))

  scores <- rPDBapi:::rpdbapi_extract_search_results(json, return_with_scores = TRUE, return_raw_json_dict = FALSE)
  expect_s3_class(scores, "rPDBapi_search_scores")

  raw <- rPDBapi:::rpdbapi_extract_search_results(json, return_with_scores = FALSE, return_raw_json_dict = TRUE)
  expect_s3_class(raw, "rPDBapi_search_raw_response")
})

test_that("internal fetch helpers validate and normalize payloads", {
  expect_error(
    rPDBapi:::rpdbapi_validate_fetch_query(NULL),
    class = "rPDBapi_error_malformed_response"
  )

  payload <- list(
    id2 = list(value = "second"),
    id1 = list(value = "first"),
    extra = list(value = "extra")
  )
  normalized <- suppressWarnings(rPDBapi:::rpdbapi_normalize_fetch_data(payload, c("id1", "id2")))
  expect_equal(names(normalized), c("id1", "id2"))
  expect_equal(normalized$id1$value, "first")

  unnamed_payload <- list(list(value = "first"), list(value = "second"))
  normalized_unnamed <- suppressWarnings(rPDBapi:::rpdbapi_normalize_fetch_data(unnamed_payload, c("id1", "id2")))
  expect_equal(names(normalized_unnamed), c("id1", "id2"))

  expect_error(
    suppressWarnings(rPDBapi:::rpdbapi_normalize_fetch_data(list(id1 = list(value = "only")), c("id1", "id2"))),
    "One or more IDs could not be retrieved"
  )
})
