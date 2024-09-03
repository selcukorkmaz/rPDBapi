#' Create a Grouped Query Object for RCSB PDB Searches
#'
#' The `QueryGroup` function constructs a grouped query object that allows users to perform complex searches in the RCSB Protein Data Bank (PDB).
#' This function is particularly useful when multiple query objects need to be combined using logical operators like 'AND' or 'OR'.
#' The resulting grouped query can be used in advanced search operations to filter or combine results based on multiple criteria.
#'
#' @param queries A list of query objects to be grouped together. Each query object can be either a simple query or another grouped query.
#' @param logical_operator A string specifying the logical operator to combine the queries. Common values are 'AND' and 'OR', but other logical operators may also be supported.
#' @return A list representing the grouped query object, which can be passed to search functions for execution.
#' @export

QueryGroup <- function(queries, logical_operator) {

      nodes <- lapply(queries, function(query) {
       if ("QueryGroup" %in% class(query)) {
          query
        } else {
          QueryNode(query, logical_operator)
        }
      })

      list(
        type = "group",
        logical_operator =  LogicalOperator[[logical_operator]],
        nodes = nodes
      )

}


#' Define Request Options for RCSB PDB Search Queries
#'
#' The `RequestOptions` function sets various options for search requests to the RCSB PDB, such as pagination and sorting preferences.
#' These options help control the volume of search results returned and the order in which they are presented.
#'
#' @param result_start_index An integer specifying the starting index for result pagination. If `NULL`, pagination is not applied.
#' @param num_results An integer specifying the number of results to return. If `NULL`, the default number of results is returned.
#' @param sort_by A string indicating the attribute to sort the results by. The default value is 'score', which ranks results based on relevance.
#' @param desc A boolean indicating whether the sorting should be in descending order. Default is `TRUE`.
#' @return A list of request options that can be included in a search query to control the results.
#' @examples
#' \donttest{
#' options <- RequestOptions(result_start_index = 0, num_results = 100, sort_by = "score", desc = TRUE)
#' }
#' @export
RequestOptions <- function(result_start_index = NULL, num_results = NULL, sort_by = "score", desc = TRUE) {

      result_dict <- list()
      if (!is.null(result_start_index) && !is.null(num_results)) {
        result_dict[["paginate"]] <- list(start = result_start_index, rows = num_results)
      }
      if (!is.null(sort_by) && !is.null(desc)) {
        result_dict[["sort"]] <- list(list(sort_by = sort_by, direction = ifelse(desc, "desc", "asc")))
      }
      result_dict


}

#' Create a Scored Result Object for PDB Searches
#'
#' The `ScoredResult` function constructs a scored result object, typically used in search results to associate an entity ID with a numerical score.
#' This is useful in ranking search results or displaying relevance scores alongside the results.
#'
#' @param entity_id A string representing the entity ID. This could be a PDB ID or any identifier relevant to the search.
#' @param score A numeric value representing the score associated with the entity. The score often indicates the relevance or quality of the match.
#' @return A list representing the scored result, which can be included in the search results or used for further processing.
#' @examples
#' \donttest{
#' result <- ScoredResult(entity_id = "1XYZ", score = 95.6)
#' }
#' @export
ScoredResult <- function(entity_id, score) {
  list(
    entity_id = entity_id,
    score = score
  )
}


#' Infer the Appropriate Search Service for RCSB PDB Queries
#'
#' The `infer_search_service` function determines the appropriate search service for a given search operator.
#' This function is essential for ensuring that queries are directed to the correct search service, such as basic search, text search, sequence search, etc.
#'
#' @param search_operator A query operator object that specifies the type of search being performed.
#' @return A string representing the inferred search service, which is necessary for constructing a valid query.
#' @export
infer_search_service <- function(search_operator) {

  if ("DefaultOperator" %in% class(search_operator)) {
    return(SearchService[["BASIC_SEARCH"]])
  }else{
    if (!is.null(search_operator$operator) && (search_operator$operator %in% TextSearchOperator || is.list(search_operator))) {
    return(SearchService[["TEXT"]])
  } else if ("SequenceOperator" %in% class(search_operator)) {
    return(SearchService[["SEQUENCE"]])
  } else if ("StructureOperator" %in% class(search_operator)) {
    return(SearchService[["STRUCTURE"]])
  } else if ("SeqMotifOperator" %in% class(search_operator)) {
    return(SearchService[["SEQMOTIF"]])
  } else if ("ChemicalOperator" %in% class(search_operator)) {
    return(SearchService[["CHEMICAL"]])
  } else {
    stop("Cannot infer Search Service for the provided search operator.")
  }

  }
}

#' Create a Query Node for RCSB PDB Searches
#'
#' The `QueryNode` function constructs a query node, which can be either a terminal node (for a simple query) or a grouped node (for complex queries).
#' This function is crucial for structuring queries that will be sent to the RCSB PDB search system.
#'
#' @param search_operator A search operator or group object. This defines the criteria for the search.
#' @param logical_operator A string specifying the logical operator to combine multiple queries. Default is `NULL`. This is used only if the search_operator is a group.
#' @return A list representing the query node, ready to be included in a larger query structure.
#' @examples
#' \donttest{
#' node <- QueryNode(search_operator = DefaultOperator("some_value"))
#' }
#' @export

QueryNode <- function(search_operator, logical_operator = NULL) {
  if(!(is.null(search_operator$type)) && search_operator$type == "group"){

    list(
      type = "group",
      # logical_operator = LogicalOperator[[logical_operator]],  # Assuming this function is defined
      nodes = search_operator            # Assuming search_operator has a to_dict method
    )

  }else{

    list(
      type = "terminal",
      service = infer_search_service(search_operator),  # Assuming this function is defined
      parameters = search_operator            # Assuming search_operator has a to_dict method
    )

  }
}



