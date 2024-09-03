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
#' @return A list containing the data fetched from the PDB, with the names of the list elements set to the corresponding IDs.
#' If any issues occur during the fetching process (e.g., if some IDs are not found), the function will return `NULL` and provide
#' informative error messages to help diagnose the problem.
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
  # Check if the JSON query is empty or NULL
  if (is.null(json_query) || length(json_query) == 0) {
    stop("JSON query is empty or has not been created. Please ensure your query is properly constructed.")
  }

  # Attempt to execute the GraphQL query
  response <- tryCatch(
    {
      search_graphql(graphql_json_query = list(query = json_query))
    },
    error = function(e) {
      stop("Failed to execute GraphQL query. Error: ", e$message)
    }
  )

  # Check if the response is NULL
  if (is.null(response)) {
    stop("Received a NULL response from the server. The query may have failed or the server may be down.")
  }

  # Check for errors in the response
  if ("errors" %in% names(response)) {
    message("Errors encountered in fetch_data():")
    lapply(response$errors, function(error) message(error$message))
    stop("The GraphQL query returned errors. Please review the error messages above.")
  }

  # Check if the data returned matches the expected IDs
  if (length(response$data[[1]]) != length(ids)) {
    warning("Mismatch between the number of returned data entries and the provided IDs.")
    missing_ids <- setdiff(ids, names(response$data[[1]]))
    if (length(missing_ids) > 0) {
      warning("The following IDs were not found in the PDB: ", paste(missing_ids, collapse = ", "))
    }
    stop("One or more IDs could not be retrieved from the PDB. Please check the provided IDs and try again.")
  } else {
    names(response$data[[1]]) <- ids
  }

  return(response)
}

