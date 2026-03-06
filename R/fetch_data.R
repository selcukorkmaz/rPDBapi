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
  ids <- rpdbapi_prepare_ids(ids, data_type = data_type)
  rpdbapi_validate_fetch_query(json_query)

  # Execute the GraphQL query and handle possible network or query errors
  response <- tryCatch(
    {
      search_graphql(graphql_json_query = list(query = json_query))
    },
    error = function(e) {
      rpdbapi_fetch_abort("GraphQL query execution failed.", e$message)
    }
  )

  returned_data <- rpdbapi_validate_fetch_payload(response)
  returned_data <- rpdbapi_normalize_fetch_data(returned_data = returned_data, ids = ids)

  response$data[[1]] <- returned_data
  response <- rpdbapi_add_class(response, "rPDBapi_fetch_response")
  attr(response, "ids") <- ids
  attr(response, "data_type") <- data_type

  return(response)
}

