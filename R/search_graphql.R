#' Perform a GraphQL Query to RCSB PDB
#'
#' The `search_graphql` function sends a GraphQL query to the RCSB Protein Data Bank (PDB) using the provided JSON query format.
#' This function handles the HTTP request, sends the query, and processes the response, including error handling to ensure that the query executes successfully.
#'
#' @param graphql_json_query A list containing the GraphQL query formatted as JSON. This list should include the `query` key with a value that represents the GraphQL query string.
#'   The query string can specify various elements to retrieve, such as entry IDs, experimental methods, cell dimensions, etc.
#' @param graphql_url A string representing the base URL perform GraphQL query. By default, this is set to the global constant \code{GRAPHQL_URL}, but users can specify a different URL if needed.
#'
#' @return A parsed list containing the content of the response from the RCSB PDB, formatted as an R object. If the request fails, the function stops with an error message.
#' @importFrom httr POST content http_status
#' @importFrom jsonlite fromJSON
#' @export

search_graphql <- function(graphql_json_query, graphql_url = GRAPHQL_URL) {
  response <- POST(url = graphql_url, body = graphql_json_query, encode = "json")

  if (http_status(response)$category != "Success") {
    warning(paste("It appears the request failed with:", content(response, "text", encoding = "UTF-8")))
    stop("Request failed.")
  }

  return(content(response, "parsed"))
}


