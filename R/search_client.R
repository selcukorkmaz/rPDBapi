#' Create a Grouped Query Object
#'
#' Constructs a grouped query object for performing complex searches in RCSB PDB.
#' It groups multiple query objects using a specified logical operator.
#'
#' @param queries A list of query objects to be grouped together.
#' @param logical_operator A string specifying the logical operator (e.g., 'AND', 'OR') to combine the queries.
#' @return A list representing the grouped query object.
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

#' Define Options for Search Requests
#'
#' Sets various options for RCSB PDB search requests, such as pagination and sorting.
#'
#' @param result_start_index An integer specifying the starting index for result pagination, default is NULL.
#' @param num_results An integer specifying the number of results to return, default is NULL.
#' @param sort_by A string indicating the attribute to sort by, default is 'score'.
#' @param desc A boolean indicating whether sorting should be in descending order, default is TRUE.
#' @return A list of request options.
#' @export
#'
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

#' Create a Scored Result Object
#'
#' Constructs a scored result object typically used in search results to associate an entity ID with its score.
#'
#' @param entity_id A string representing the entity ID.
#' @param score A numeric value representing the score associated with the entity.
#' @return A list representing the scored result.
#' @export
ScoredResult <- function(entity_id, score) {
  list(
    entity_id = entity_id,
    score = score
  )
}


#' Infer the Appropriate Search Service
#'
#' Determines the appropriate search service for a given search operator in RCSB PDB queries.
#'
#' @param search_operator A query operator object.
#' @return The inferred search service.
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

#' Create a Query Node Object
#'
#' Constructs a query node, which can be a terminal node or a grouped node, for complex RCSB PDB searches. This function is used to structure queries for the search system.
#'
#' @param search_operator A search operator or group object.
#' @param logical_operator A string specifying the logical operator, default is NULL. Used only if the search_operator is a group.
#' @return A list representing the query node.
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



