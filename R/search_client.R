#' Create a Grouped Query Object
#'
#' Constructs a grouped query object for performing complex searches in RCSB PDB.
#' It groups multiple query objects using a specified logical operator.
#'
#' @param queries A list of query objects to be grouped together.
#' @param logical_operator A string specifying the logical operator (e.g., 'AND', 'OR') to combine the queries.
#' @return A list representing the grouped query object.
#' @examples
#' human_operator <- ExactMatchOperator(value="Homo sapiens", attribute="rcsb_entity_source_organism.taxonomy_lineage.name")
#' mus_operator <- ExactMatchOperator(value="Mus musculus", attribute="rcsb_entity_source_organism.taxonomy_lineage.name")
#' group_query <- QueryGroup(list(human_operator, mus_operator), "OR")
#' group_query
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
#' @examples
#' options <- RequestOptions(num_results = 10, sort_by = "resolution")
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
#' @examples
#' scored_result <- ScoredResult("1XYZ", 9.8)
#' @export
ScoredResult <- function(entity_id, score) {
  list(
    entity_id = entity_id,
    score = score
  )
}


#' Exception for Inability to Infer Search Service
#'
#' Throws an exception when the search service cannot be inferred from the provided search operator.
#'
#' @param message A string containing the error message.
#' @export
CannotInferSearchServiceException <- function(message) {
  stop(paste("CannotInferSearchServiceException:", message))
}



#' Infer the Appropriate Search Service
#'
#' Determines the appropriate search service for a given search operator in RCSB PDB queries.
#'
#' @param search_operator A query operator object.
#' @return The inferred search service.
#' @examples
#' search_op <- ExactMatchOperator(value="Homo sapiens", attribute="rcsb_entity_source_organism.taxonomy_lineage.name")
#' service <- infer_search_service(search_op)
#' service
#' @export
infer_search_service <- function(search_operator) {

  if ("DefaultOperator" %in% class(search_operator)) {
    return(SearchService[["BASIC_SEARCH"]])
  }else{
    if (search_operator$operator %in% TextSearchOperator || class(search_operator) == "list") {
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
    stop("CannotInferSearchServiceException: Cannot infer Search Service for the provided search operator.")
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
#' @examples
#' search_op <- ExactMatchOperator(value="Homo sapiens", attribute="rcsb_entity_source_organism.taxonomy_lineage.name")
#' query_node <- QueryNode(search_op)
#' query_node
#'
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


# Example Usage
# SearchOperator associated with structures with under 4 Angstroms of resolution
under_4A_resolution_operator = ComparisonOperator(
  value=4,
  attribute="rcsb_entry_info.resolution_combined",
  comparison_type="GREATER")

results = perform_search_with_graph(
  query_object=under_4A_resolution_operator,
  return_type="ENTRY")
head(results)


# SearchOperator associated with entities containing 'Mus musculus' lineage
is_mus_operator = ExactMatchOperator(
  value="Mus musculus",
  attribute="rcsb_entity_source_organism.taxonomy_lineage.name")

results = perform_search_with_graph(
  query_object=is_mus_operator,
  return_type="ENTRY")
head(results)


# SearchOperator associated with entities containing 'Homo sapiens' lineage
is_human_operator = ExactMatchOperator(
  value="Homo sapiens",
  attribute="rcsb_entity_source_organism.taxonomy_lineage.name")


results = perform_search_with_graph(
  query_object=is_human_operator,
  return_type="ENTRY")
head(results)

# QueryGroup associated with being either human or `Mus musculus`
is_human_or_mus_group = QueryGroup(
  queries = list(is_mus_operator, is_human_operator),
  logical_operator = "OR"
)

results = perform_search_with_graph(
  query_object=is_human_or_mus_group,
  return_type="ENTRY")
head(results)

# QueryGroup associated with being ((Human OR Mus) AND (Under 4 Angstroms))
is_under_4A_and_human_and_mus_group = QueryGroup(
  queries = list(is_human_or_mus_group, under_4A_resolution_operator),
  logical_operator = "AND"
)

results = perform_search_with_graph(
  query_object=is_under_4A_and_human_and_mus_group,
  return_type="ENTRY")
head(results)

