#' Fetch Data from RCSB PDB Using a JSON Query
#'
#' This function sends a GraphQL JSON query to the RCSB Protein Data Bank (PDB) to fetch data corresponding to a specified set of IDs.
#' It is designed to handle the complexities of interacting with the PDB's GraphQL API, ensuring that errors in the query process
#' are handled gracefully and that users are informed of any discrepancies in the expected and returned data.
#'
#' @param json_query A JSON string representing the query to be sent to the PDB. This query must be well-formed and adhere to the GraphQL
#' query structure required by the PDB's API. It typically includes details such as the fields to be retrieved and the conditions for
#' data selection.
#' @param data_type A string indicating the type of data to be fetched. While this parameter is not directly used in the function,
#' it can provide context for the data being retrieved, such as "ENTRY", "ASSEMBLY", "POLYMER_ENTITY", etc.
#' @param ids A character vector of identifiers for which data is being requested. These IDs should correspond to valid entries in
#' the PDB and should match the data structure expected by the PDB's API.
#' @return A list with class \code{"rPDBapi_fetch_response"} containing the fetched
#' payload under \code{$data}. The list entries are validated and normalized to the
#' requested \code{ids}. Malformed responses raise typed errors.
#'
#' @details
#' The function performs several checks and operations:
#'
#'   * It validates the `json_query` to ensure that it is neither `NULL` nor empty.
#'   * It attempts to send the query to the PDB's GraphQL endpoint using a helper function (assumed to be `search_graphql`).
#'   * It checks the server's response to determine if the query was successful or if any errors were encountered.
#'   * If the data returned does not match the expected IDs, the function issues warnings and stops execution, providing details on the missing IDs.
#'   * The function ensures that the returned data is correctly named according to the provided IDs.
#'
#' The function is particularly useful for developers and researchers who need to programmatically access and manipulate large datasets
#' from the PDB. It abstracts away some of the complexity of directly interacting with the PDB's API, providing a more user-friendly
#' interface for data retrieval.
#'
#' @importFrom jsonlite fromJSON
#' @importFrom httr GET
#'

fetch_data <- function(json_query, data_type, ids) {

  # Helper function for better error logging
  log_error <- function(message, details = NULL) {
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

  # Validate the JSON query
  if (is.null(json_query) || length(json_query) == 0) {
    log_error("JSON query is empty or invalid. Please ensure the query is correctly formatted.")
  }

  # Execute the GraphQL query and handle possible network or query errors
  response <- tryCatch(
    {
      search_graphql(graphql_json_query = list(query = json_query))
    },
    error = function(e) {
      log_error("GraphQL query execution failed.", e$message)
    }
  )

  # Handle NULL response (server down or no response)
  if (is.null(response)) {
    log_error("Received NULL response from the server. Please check the server status or query validity.")
  }

  # Check for errors returned within the GraphQL response
  if ("errors" %in% names(response)) {
    message("Errors encountered in GraphQL query:")
    lapply(response$errors, function(error) message(error$message))
    log_error("The query returned errors. Please review the error messages above.")
  }

  # Normalize and validate returned data IDs.
  returned_data <- response$data[[1]]
  if (is.null(returned_data) || !is.list(returned_data)) {
    log_error("Malformed response payload: expected a list under response$data[[1]].")
  }

  returned_names <- names(returned_data)
  if (is.null(returned_names) || any(returned_names == "")) {
    warning("Unnamed response entries were normalized by position against requested IDs.")

    if (length(returned_data) < length(ids)) {
      warning("Mismatch in the number of returned data entries and the provided IDs.")
      missing_ids <- ids[seq.int(length(returned_data) + 1, length(ids))]
      if (length(missing_ids) > 0) {
        warning("The following IDs were not found: ", paste(missing_ids, collapse = ", "))
      }
      log_error("One or more IDs could not be retrieved. Please check the IDs and try again.")
    }

    if (length(returned_data) > length(ids)) {
      warning("More entries were returned than requested IDs; truncating extras to requested IDs.")
      returned_data <- returned_data[seq_along(ids)]
    }

    names(returned_data) <- ids[seq_along(returned_data)]
  } else {
    extra_ids <- setdiff(returned_names, ids)
    if (length(extra_ids) > 0) {
      warning("Ignoring unrequested IDs returned by API: ", paste(extra_ids, collapse = ", "))
    }

    missing_ids <- setdiff(ids, returned_names)
    if (length(missing_ids) > 0) {
      warning("Mismatch in the number of returned data entries and the provided IDs.")
      warning("The following IDs were not found: ", paste(missing_ids, collapse = ", "))
      log_error("One or more IDs could not be retrieved. Please check the IDs and try again.")
    }

    # Keep only requested IDs and preserve input order.
    returned_data <- returned_data[ids]
    names(returned_data) <- ids
  }

  response$data[[1]] <- returned_data
  response <- rpdbapi_add_class(response, "rPDBapi_fetch_response")
  attr(response, "ids") <- ids
  attr(response, "data_type") <- data_type

  return(response)
}

