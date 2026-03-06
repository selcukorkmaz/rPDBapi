# Internal helpers for fetch_data() normalization/validation.

rpdbapi_fetch_abort <- function(message, details = NULL) {
  full_message <- paste0("ERROR: ", message)
  if (!is.null(details)) {
    full_message <- paste0(full_message, " Details: ", details)
  }

  rpdbapi_abort(
    full_message,
    class = "rPDBapi_error_malformed_response",
    function_name = "fetch_data"
  )
}

rpdbapi_validate_fetch_query <- function(json_query) {
  if (is.null(json_query) || length(json_query) == 0) {
    rpdbapi_fetch_abort("JSON query is empty or invalid. Please ensure the query is correctly formatted.")
  }
}

rpdbapi_validate_fetch_payload <- function(response) {
  if (is.null(response)) {
    rpdbapi_fetch_abort("Received NULL response from the server. Please check the server status or query validity.")
  }

  if ("errors" %in% names(response)) {
    message("Errors encountered in GraphQL query:")
    lapply(response$errors, function(error) message(error$message))
    rpdbapi_fetch_abort("The query returned errors. Please review the error messages above.")
  }

  returned_data <- response$data[[1]]
  if (is.null(returned_data) || !is.list(returned_data)) {
    rpdbapi_fetch_abort("Malformed response payload: expected a list under response$data[[1]].")
  }

  returned_data
}

rpdbapi_normalize_fetch_data <- function(returned_data, ids) {
  returned_names <- names(returned_data)
  if (is.null(returned_names) || any(returned_names == "")) {
    warning("Unnamed response entries were normalized by position against requested IDs.")

    if (length(returned_data) < length(ids)) {
      warning("Mismatch in the number of returned data entries and the provided IDs.")
      missing_ids <- ids[seq.int(length(returned_data) + 1, length(ids))]
      if (length(missing_ids) > 0) {
        warning("The following IDs were not found: ", paste(missing_ids, collapse = ", "))
      }
      rpdbapi_fetch_abort("One or more IDs could not be retrieved. Please check the IDs and try again.")
    }

    if (length(returned_data) > length(ids)) {
      warning("More entries were returned than requested IDs; truncating extras to requested IDs.")
      returned_data <- returned_data[seq_along(ids)]
    }

    names(returned_data) <- ids[seq_along(returned_data)]
    return(returned_data)
  }

  extra_ids <- setdiff(returned_names, ids)
  if (length(extra_ids) > 0) {
    warning("Ignoring unrequested IDs returned by API: ", paste(extra_ids, collapse = ", "))
  }

  missing_ids <- setdiff(ids, returned_names)
  if (length(missing_ids) > 0) {
    warning("Mismatch in the number of returned data entries and the provided IDs.")
    warning("The following IDs were not found: ", paste(missing_ids, collapse = ", "))
    rpdbapi_fetch_abort("One or more IDs could not be retrieved. Please check the IDs and try again.")
  }

  returned_data <- returned_data[ids]
  names(returned_data) <- ids
  returned_data
}
