#' Fetch Data from RCSB PDB Using a JSON Query
#'
#' This function sends a JSON query to the RCSB Protein Data Bank (PDB) and fetches the corresponding data.
#' It checks for errors in the response and warns if there are discrepancies in the number of IDs found.
#'
#' @param json_query A JSON string representing the query to be sent to the PDB.
#' @param data_type A string indicating the type of data to be fetched (not directly used in the function but may be relevant for context).
#' @param ids A vector of identifiers to fetch data for.
#' @return A list containing the data fetched from the PDB, with the names of the list elements set to the corresponding IDs.
#'   If an error is encountered in the data fetching process, the function returns `NULL`.
#' @importFrom jsonlite fromJSON

fetch_data <- function(json_query, data_type, ids) {
  if (length(json_query) == 0) {
    stop("JSON query has not been created.")
  }

  response <- tryCatch(
    {
      search_graphql(graphql_json_query = list(query = json_query))
    },
    error = function(e) {
      warning("Failed to execute GraphQL query: ", e$message)
      return(NULL)
    }
  )

  if (is.null(response)) {
    return(NULL)
  }

  if ("errors" %in% names(response)) {
    message("ERROR encountered in fetch_data().")
    lapply(response$errors, function(error) message(error$message))
    return(NULL)
  }

  if (length(response$data[[1]]) != length(ids)) {
    stop("The number of returned IDs does not match the input IDs.")
  } else {
    names(response$data[[1]]) <- ids
  }

  return(response)
}
