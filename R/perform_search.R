#' Perform a Search in the RCSB PDB
#'
#' This function facilitates searching the RCSB Protein Data Bank (PDB) using a specified search operator.
#' It allows various configurations like return type, additional request options, and verbosity control.
#'
#' @param search_operator An object specifying the search criteria.
#' @param return_type A string specifying the type of data to return, defaulting to 'ENTRY'.
#' @param request_options Additional options for the search request, default is NULL.
#' @param return_with_scores A boolean indicating whether to return search results with scores, default is FALSE.
#' @param return_raw_json_dict A boolean indicating whether to return raw JSON response, default is FALSE.
#' @param verbosity A boolean flag indicating whether to display verbose messages during execution, default is TRUE.
#' @return The search results, which can vary based on the return type and options specified.
#' @importFrom httr POST
#' @importFrom jsonlite toJSON
#' @examples
#' search_operator = InOperator(value=c("Mus musculus", "Homo sapiens"),
#' attribute="rcsb_entity_source_organism.taxonomy_lineage.name")
#' return_type = "NON_POLYMER_ENTITY"
#' results = perform_search(search_operator, return_type)
#' results
#' @export

perform_search <- function(search_operator, return_type = "ENTRY", request_options = NULL, return_with_scores = FALSE, return_raw_json_dict = FALSE, verbosity = TRUE) {
  results <- tryCatch(
    {
      perform_search_with_graph(
        query_object = search_operator,
        return_type = return_type,
        request_options = request_options,
        return_with_scores = return_with_scores,
        return_raw_json_dict = return_raw_json_dict,
        verbosity = verbosity
      )
    },
    error = function(e) {
      warning("Failed to fetch data from the database: ", e$message)
      return(NULL)
    }
  )
  return(results)
}

perform_search_with_graph <- function(query_object, return_type = "ENTRY", request_options = NULL, return_with_scores = FALSE, return_raw_json_dict = FALSE, verbosity = TRUE) {
  if ((is.null(query_object$operator) || query_object$operator %in% SEARCH_OPERATORS) && is.null(query_object$type)) {
    cast_query_object <- QueryNode(query_object)
  } else {
    cast_query_object <- query_object
  }

  request_options_dict <- if (!is.null(request_options)) {
    request_options
  } else {
    list(return_all_hits = TRUE)
  }

  rcsb_query_dict <- list(
    query = cast_query_object,
    request_options = request_options_dict,
    return_type = ReturnType[[return_type]]
  )

  if (verbosity) {
    message("Querying RCSB Search with the following parameters:\n", toJSON(rcsb_query_dict, auto_unbox = TRUE, pretty = TRUE))
  }

  response <- tryCatch(
    {
      POST(
        url = SEARCH_URL_ENDPOINT,
        body = toJSON(rcsb_query_dict, auto_unbox = TRUE, pretty = TRUE),
        encode = "json",
        content_type("application/json")
      )
    },
    error = function(e) {
      stop("HTTP request failed: ", e$message)
    }
  )

  if (http_status(response)$category != "Success") {
    stop("Request failed with: ", content(response, "text", encoding = "UTF-8"))
  }

  response_json <- fromJSON(content(response, "text", encoding = "UTF-8"))
  if (return_raw_json_dict) {
    return(response_json)
  }

  results <- if (return_with_scores) {
    response_json$result_set
  } else {
    response_json$result_set$identifier
  }

  return(results)
}
