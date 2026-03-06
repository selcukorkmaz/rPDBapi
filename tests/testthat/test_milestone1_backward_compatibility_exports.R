# Milestone 1: Backward compatibility freeze for exported surface.

namespace_exports <- function() {
  lines <- readLines(testthat::test_path("..", "..", "NAMESPACE"))
  sub("^export\\((.*)\\)$", "\\1", grep("^export\\(", lines, value = TRUE))
}

test_that("NAMESPACE keeps all baseline exports (additive exports allowed)", {
  expected_exports <- c(
    "ChemicalOperator",
    "ComparisonOperator",
    "ContainsPhraseOperator",
    "ContainsWordsOperator",
    "DefaultOperator",
    "ExactMatchOperator",
    "ExistsOperator",
    "InOperator",
    "QueryGroup",
    "QueryNode",
    "RangeOperator",
    "RequestOptions",
    "ScoredResult",
    "SeqMotifOperator",
    "SequenceOperator",
    "StructureOperator",
    "add_property",
    "autoresolve_sequence_type",
    "data_fetcher",
    "describe_chemical",
    "find_papers",
    "find_results",
    "generate_json_query",
    "get_fasta_from_rcsb_entry",
    "get_info",
    "get_pdb_api_url",
    "get_pdb_file",
    "handle_api_errors",
    "infer_search_service",
    "parse_response",
    "perform_search",
    "query_search",
    "return_data_as_dataframe",
    "search_graphql",
    "send_api_request"
  )

  current_exports <- namespace_exports()
  expect_true(all(expected_exports %in% current_exports))
})

test_that("all exported symbols resolve to callable functions", {
  for (sym in namespace_exports()) {
    obj <- get(sym, envir = asNamespace("rPDBapi"), inherits = FALSE)
    expect_true(is.function(obj), info = paste("Non-function export:", sym))
  }
})

test_that("core exported function signatures remain stable", {
  expected_formals <- list(
    query_search = c("search_term", "query_type", "return_type", "scan_params", "num_attempts", "sleep_time"),
    perform_search = c("search_operator", "return_type", "request_options", "return_with_scores", "return_raw_json_dict", "verbosity"),
    data_fetcher = c("id", "data_type", "properties", "return_as_dataframe", "verbosity"),
    fetch_data = c("json_query", "data_type", "ids"),
    generate_json_query = c("ids", "data_type", "properties"),
    get_info = c("pdb_id"),
    get_pdb_file = c("pdb_id", "filetype", "rm.insert", "rm.alt", "compression", "save", "path", "verbosity", "download_base_url"),
    get_fasta_from_rcsb_entry = c("rcsb_id", "chain_id", "verbosity", "fasta_base_url"),
    describe_chemical = c("chem_id", "url_root"),
    send_api_request = c("url", "method", "body", "encode", "content_type", "verbosity"),
    parse_response = c("response", "format"),
    handle_api_errors = c("response", "url")
  )

  for (fname in names(expected_formals)) {
    fn <- get(fname, envir = asNamespace("rPDBapi"), inherits = FALSE)
    expect_identical(names(formals(fn)), expected_formals[[fname]], info = fname)
  }
})
