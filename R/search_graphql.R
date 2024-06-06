#' Perform a GraphQL Query to RCSB PDB
#'
#' Executes a GraphQL query against the RCSB Protein Data Bank (PDB).
#' It sends the query in JSON format and handles the HTTP response, including error checking.
#'
#' @param graphql_json_query A list containing the GraphQL query in JSON format.
#' @return The parsed content of the response from the PDB.
#' @importFrom httr POST content
#' @importFrom jsonlite fromJSON
#' @examples
#' graphql_json_query <- list(query = "{entries(entry_ids:
#'                           [\"4LZA\", \"5RU3\"]){cell {volume, angle_beta},
#'                           exptl {method}}}")
#' result <- search_graphql(graphql_json_query)
#' result
#' @export


search_graphql <- function(graphql_json_query) {
  response <- POST(url = RSCB_GRAPHQL_URL, body = graphql_json_query, encode = "json")

  if (http_status(response)$category != "Success") {
    warning(paste("It appears the request failed with:", content(response, "text", encoding = "UTF-8")))
    stop("Request failed.")
  }

  return(content(response, "parsed"))
}


