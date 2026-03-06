# Milestone 1: Additional compatibility contracts for exported functions.

test_that("SequenceOperator preserves expected fields and target mapping", {
  op <- SequenceOperator(
    sequence = "MVLSPADKTNVKAAW",
    sequence_type = "PROTEIN",
    evalue_cutoff = 10,
    identity_cutoff = 0.9
  )

  expect_s3_class(op, "SequenceOperator")
  expect_equal(op$target, "pdb_protein_sequence")
  expect_equal(op$evalue_cutoff, 10)
  expect_equal(op$identity_cutoff, 0.9)
  expect_equal(op$value, "MVLSPADKTNVKAAW")
})

test_that("SequenceOperator can infer sequence type via autoresolve", {
  op <- SequenceOperator(sequence = "ATGCGTACGTAGC")
  expect_equal(op$target, "pdb_dna_sequence")
})

test_that("autoresolve_sequence_type returns DNA/RNA/PROTEIN and rejects ambiguous input", {
  expect_equal(autoresolve_sequence_type("ATGCGTACGTAGC"), "DNA")
  expect_equal(autoresolve_sequence_type("AUGCGUACGUAGC"), "RNA")
  expect_equal(autoresolve_sequence_type("MVLSPADKTNVKAAW"), "PROTEIN")
  expect_error(autoresolve_sequence_type("ATU"), "ambiguous")
})

test_that("SeqMotifOperator returns expected mapping contract", {
  op <- SeqMotifOperator(
    pattern = "A[TU]G",
    sequence_type = "DNA",
    pattern_type = "REGEX"
  )

  expect_s3_class(op, "SeqMotifOperator")
  expect_equal(op$value, "A[TU]G")
  expect_equal(op$pattern_type, "regex")
  expect_equal(op$target, "pdb_dna_sequence")
})

test_that("ChemicalOperator detects descriptor type and preserves match type", {
  smiles <- ChemicalOperator("C1=CC=CC=C1")
  inchi <- ChemicalOperator("InChI=1S/CH4/h1H4", matching_criterion = "graph-relaxed")

  expect_s3_class(smiles, "ChemicalOperator")
  expect_equal(smiles$descriptor_type, "SMILES")
  expect_equal(smiles$match_type, "graph-strict")

  expect_s3_class(inchi, "ChemicalOperator")
  expect_equal(inchi$descriptor_type, "InChI")
  expect_equal(inchi$match_type, "graph-relaxed")
})

test_that("get_pdb_api_url concatenates base url, endpoint, and id", {
  expect_equal(
    get_pdb_api_url("core/entry/", "4HHB", base_url = "https://example.org/rest/v1/"),
    "https://example.org/rest/v1/core/entry/4HHB"
  )
})

test_that("parse_response parses JSON and text and rejects unsupported formats", {
  local_mocked_bindings(
    content = function(response, as = "text", encoding = "UTF-8") "{\"ok\":true}",
    fromJSON = function(txt) list(ok = TRUE),
    .package = "rPDBapi"
  )
  parsed_json <- parse_response(structure(list(), class = "response"), format = "json")
  expect_equal(parsed_json$ok, TRUE)

  local_mocked_bindings(
    content = function(response, as = "text", encoding = "UTF-8") "plain text",
    .package = "rPDBapi"
  )
  parsed_text <- parse_response(structure(list(), class = "response"), format = "text")
  expect_equal(parsed_text, "plain text")

  expect_error(
    parse_response(structure(list(), class = "response"), format = "xml"),
    "Unsupported format"
  )
})

test_that("send_api_request dispatches GET and POST and validates method", {
  local_mocked_bindings(
    GET = function(url) list(method = "GET", url = url),
    POST = function(url, body = NULL, encode = "json", content_type = NULL) {
      list(method = "POST", url = url, body = body, encode = encode, content_type = content_type)
    },
    content_type = function(x) paste0("ct:", x),
    .package = "rPDBapi"
  )

  get_resp <- send_api_request("https://example.org/get", method = "GET", verbosity = FALSE)
  expect_equal(get_resp$method, "GET")
  expect_equal(get_resp$url, "https://example.org/get")

  post_resp <- send_api_request(
    "https://example.org/post",
    method = "POST",
    body = list(a = 1),
    encode = "json",
    content_type = "application/json",
    verbosity = FALSE
  )
  expect_equal(post_resp$method, "POST")
  expect_equal(post_resp$encode, "json")
  expect_equal(post_resp$content_type, "ct:application/json")

  expect_error(
    send_api_request("https://example.org", method = "PUT", verbosity = FALSE),
    "Network Error: Failed to send request\\. Error: Unsupported HTTP method"
  )
})

test_that("send_api_request wraps network errors consistently", {
  local_mocked_bindings(
    GET = function(url) stop("boom"),
    .package = "rPDBapi"
  )

  expect_error(
    send_api_request("https://example.org/fail", method = "GET", verbosity = FALSE),
    "Network Error: Failed to send request"
  )
})

test_that("handle_api_errors passes successes and stops on failed HTTP status", {
  local_mocked_bindings(
    http_status = function(response) list(category = "Success", status = 200, message = "OK"),
    .package = "rPDBapi"
  )
  expect_null(handle_api_errors(structure(list(), class = "response"), "https://ok"))

  local_mocked_bindings(
    http_status = function(response) list(category = "Client error", status = 404, message = "Not Found"),
    .package = "rPDBapi"
  )
  expect_error(
    handle_api_errors(structure(list(), class = "response"), "https://missing"),
    "HTTP Error: Request to https://missing failed"
  )
})

test_that("search_graphql returns parsed content on success and errors on failure", {
  local_mocked_bindings(
    POST = function(url, body, encode = "json", ...) structure(list(), class = "response"),
    http_status = function(response) list(category = "Success", status = 200, message = "OK"),
    content = function(response, as = "parsed", encoding = "UTF-8") list(data = list(id = "4HHB")),
    .package = "rPDBapi"
  )
  parsed <- search_graphql(list(query = "{ entries(entry_ids: [\"4HHB\"]) { rcsb_id } }"))
  expect_equal(parsed$data$id, "4HHB")

  local_mocked_bindings(
    POST = function(url, body, encode = "json", ...) structure(list(), class = "response"),
    http_status = function(response) list(category = "Client error", status = 400, message = "Bad Request"),
    content = function(response, as = "text", encoding = "UTF-8") "bad request",
    .package = "rPDBapi"
  )
  expect_error(
    suppressWarnings(search_graphql(list(query = "broken"))),
    "Request failed"
  )
})

test_that("describe_chemical validates input length and returns parsed output", {
  expect_error(
    describe_chemical("TOOLONG"),
    "maximum allowed length of 3 characters"
  )

  local_mocked_bindings(
    send_api_request = function(url, method = "GET", body = NULL, encode = "json", content_type = "application/json", verbosity = TRUE) {
      list(url = url, method = method)
    },
    handle_api_errors = function(response, url = "") invisible(NULL),
    parse_response = function(response, format = "json") list(chem_comp = list(id = "ATP")),
    .package = "rPDBapi"
  )

  result <- describe_chemical("ATP", url_root = "https://example.org/chemcomp/")
  expect_equal(result$chem_comp$id, "ATP")
})

test_that("describe_chemical wraps network and API errors", {
  local_mocked_bindings(
    send_api_request = function(url, method = "GET", body = NULL, encode = "json", content_type = "application/json", verbosity = TRUE) {
      stop("timeout")
    },
    .package = "rPDBapi"
  )
  expect_error(describe_chemical("ATP"), "Network Error")

  local_mocked_bindings(
    send_api_request = function(url, method = "GET", body = NULL, encode = "json", content_type = "application/json", verbosity = TRUE) list(url = url),
    handle_api_errors = function(response, url = "") stop("status 500"),
    .package = "rPDBapi"
  )
  expect_error(describe_chemical("ATP"), "API Error")
})
