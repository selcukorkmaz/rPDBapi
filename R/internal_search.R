# Internal helpers for perform_search().

rpdbapi_validate_perform_return_type <- function(return_type) {
  supported_return_types <- names(ReturnType)
  if (!return_type %in% supported_return_types) {
    rpdbapi_abort(
      paste0(
        "Invalid return_type '", return_type, "'. Supported types are: '",
        paste(supported_return_types, collapse = "', '"), "'."
      ),
      class = "rPDBapi_error_unsupported_mapping",
      function_name = "perform_search_with_graph",
      return_type = return_type
    )
  }
}

rpdbapi_cast_query_object <- function(query_object) {
  if ((is.null(query_object$operator) || query_object$operator %in% SEARCH_OPERATORS) && is.null(query_object$type)) {
    return(QueryNode(query_object))
  }
  query_object
}

rpdbapi_build_search_request <- function(cast_query_object, return_type, request_options = NULL) {
  request_options_dict <- if (!is.null(request_options)) request_options else list(return_all_hits = TRUE)

  rcsb_query_dict <- list(
    query = cast_query_object,
    request_options = request_options_dict,
    return_type = ReturnType[[return_type]]
  )

  if (is.null(rcsb_query_dict$return_type)) {
    rpdbapi_abort(
      paste0("Internal mapping error: no API return_type mapping found for '", return_type, "'."),
      class = "rPDBapi_error_unsupported_mapping",
      function_name = "perform_search_with_graph",
      return_type = return_type
    )
  }

  rcsb_query_dict
}

rpdbapi_parse_search_response_json <- function(response) {
  content_text <- tryCatch(
    {
      rpdbapi_response_text(response)
    },
    error = function(e) {
      rpdbapi_rethrow(
        e,
        message_prefix = "Failed to retrieve content from RCSB response: ",
        class = "rPDBapi_error_malformed_response",
        function_name = "perform_search_with_graph"
      )
    }
  )

  response_json <- tryCatch(
    {
      fromJSON(content_text)
    },
    error = function(e) {
      rpdbapi_abort(
        paste0("Failed to parse RCSB response as JSON: ", conditionMessage(e), "\nResponse content: ", content_text),
        class = "rPDBapi_error_malformed_response",
        function_name = "perform_search_with_graph",
        parent_message = conditionMessage(e)
      )
    }
  )

  if (!is.list(response_json) || is.null(response_json$result_set)) {
    rpdbapi_abort(
      "Malformed search response: missing 'result_set'.",
      class = "rPDBapi_error_malformed_response",
      function_name = "perform_search_with_graph"
    )
  }

  response_json
}

rpdbapi_extract_search_results <- function(response_json, return_with_scores = FALSE, return_raw_json_dict = FALSE) {
  if (return_raw_json_dict) {
    return(rpdbapi_add_class(response_json, "rPDBapi_search_raw_response"))
  }

  results <- tryCatch(
    {
      if (return_with_scores) response_json$result_set else response_json$result_set$identifier
    },
    error = function(e) {
      rpdbapi_rethrow(
        e,
        message_prefix = "Failed to extract results from RCSB response JSON: ",
        class = "rPDBapi_error_malformed_response",
        function_name = "perform_search_with_graph"
      )
    }
  )

  if (is.null(results)) {
    rpdbapi_abort(
      "No results were found for the given query. Please check your search criteria.",
      class = "rPDBapi_error_malformed_response",
      function_name = "perform_search_with_graph"
    )
  }

  if (return_with_scores) {
    return(rpdbapi_add_class(results, "rPDBapi_search_scores"))
  }

  results <- as.character(results)
  rpdbapi_add_class(results, "rPDBapi_search_ids")
}
