library(httr)
library(jsonlite)

SEARCH_URL_ENDPOINT <- "https://search.rcsb.org/rcsbsearch/v2/query"

LogicalOperator <- c("AND" = "and", "OR" = "or")

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


ReturnType <- c(
  ENTRY = "entry",
  ASSEMBLY = "assembly",
  POLYMER_ENTITY = "polymer_entity",
  NON_POLYMER_ENTITY = "non_polymer_entity",
  POLYMER_INSTANCE = "polymer_instance"
)


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

ScoredResult <- function(entity_id, score) {
  list(
    entity_id = entity_id,
    score = score
  )
}

perform_search <- function(search_operator, return_type = "ENTRY", request_options = NULL, return_with_scores = FALSE, return_raw_json_dict = FALSE, verbosity = TRUE) {
  # Assuming perform_search_with_graph is already defined in R
  results <- perform_search_with_graph(
    query_object = search_operator,
    return_type = return_type,
    request_options = request_options,
    return_with_scores = return_with_scores,
    return_raw_json_dict = return_raw_json_dict,
    verbosity = verbosity
  )

  # Process and return results based on specified return type and options
  return(results)
}

# Combine TEXTSEARCH_OPERATORS with other operators
SEARCH_OPERATORS <- c(TextSearchOperator, "sequence", "StructureOperator", "SeqMotifOperator")

perform_search_with_graph <- function(query_object, return_type = "ENTRY", request_options = NULL, return_with_scores = FALSE, return_raw_json_dict = FALSE, verbosity = TRUE) {
  # Check if query_object is a SearchOperator or QueryGroup
  if((is.null(query_object$operator) || query_object$operator %in% SEARCH_OPERATORS) &&  is.null(query_object$type)){
    cast_query_object = QueryNode(query_object)
  }else{
    cast_query_object = query_object
  }

  # Prepare request options
  request_options_dict <- if (!is.null(request_options)) {
    request_options
  } else {
    list(return_all_hits = TRUE)
  }

  # Construct the query dictionary
  rcsb_query_dict <- list(
    query = cast_query_object,
    request_options = request_options_dict,
    return_type = ReturnType[[return_type]]
  )

  if (verbosity) {
    message("Querying RCSB Search with the following parameters:\n", toJSON(rcsb_query_dict, auto_unbox = TRUE, pretty = TRUE))
  }

  # Perform the HTTP POST request
  response <- POST(
    url = SEARCH_URL_ENDPOINT,
    body = toJSON(rcsb_query_dict, auto_unbox = TRUE, pretty = TRUE),
    encode = "json"
  )

  # Check response status
  if (http_status(response)$category != "Success") {
    stop("Request failed with:", content(response, "text", encoding = "UTF-8"))
  }

  # Process the response
  response_json <- fromJSON(content(response, "text", encoding = "UTF-8"))
  if (return_raw_json_dict) {
    return(response_json)
  }

  results <-
    if (return_with_scores) {
      response_json$result_set
    } else {
      response_json$result_set$identifier
    }


  return(results)
}

SearchService <- c(
  BASIC_SEARCH = "full_text",
  TEXT = "text",
  SEQUENCE = "sequence",
  SEQMOTIF = "seqmotif",
  STRUCTURE = "structure",
  CHEMICAL = "chemical"
)

CannotInferSearchServiceException <- function(message) {
  stop(paste("CannotInferSearchServiceException:", message))
}

infer_search_service <- function(search_operator) {
  # Assuming text_operators$TEXTSEARCH_OPERATORS, SequenceOperator, StructureOperator, SeqMotifOperator, and ChemicalOperator are defined elsewhere

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

