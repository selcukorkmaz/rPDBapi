library(httr)
library(jsonlite)

SEARCH_URL_ENDPOINT <- "https://search.rcsb.org/rcsbsearch/v2/query"

LogicalOperator <- c("AND" = "and", "OR" = "or")

QueryGroup <- function(queries, logical_operator) {
      nodes <- lapply(queries, function(query) {
        if (inherits(query, "QueryGroup")) {
          query
        } else {
          QueryNode(query)  # Assuming QueryNode is defined elsewhere
        }
      })

      list(
        type = "group",
        logical_operator = LogicalOperator[[logical_operator]],
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
SEARCH_OPERATORS <- c(TextSearchOperator, "SequenceOperator", "StructureOperator", "SeqMotifOperator")

perform_search_with_graph <- function(query_object, return_type = "ENTRY", request_options = NULL, return_with_scores = FALSE, return_raw_json_dict = FALSE, verbosity = TRUE) {
  # Check if query_object is a SearchOperator or QueryGroup
  if(is.null(query_object$operator) || query_object$operator %in% SEARCH_OPERATORS){
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

  if (is.null(search_operator$operator)) {
    return(SearchService[["BASIC_SEARCH"]])
  }else{
    if (search_operator$operator %in% TextSearchOperator) {
    return(SearchService[["TEXT"]])
  } else if (search_operator$operator == "SequenceOperator") {
    return(SearchService[["SEQUENCE"]])
  } else if (search_operator$operator == "StructureOperator") {
    return(SearchService[["STRUCTURE"]])
  } else if (search_operator$operator == "SeqMotifOperator") {
    return(SearchService[["SEQMOTIF"]])
  } else if (search_operator$operator == "ChemicalOperator") {
    return(SearchService[["CHEMICAL"]])
  } else {
    stop("CannotInferSearchServiceException: Cannot infer Search Service for the provided search operator.")
  }

  }
}

QueryNode <- function(search_operator) {
  list(
    type = "terminal",
    service = infer_search_service(search_operator),  # Assuming this function is defined
    parameters = search_operator            # Assuming search_operator has a to_dict method
  )
}


# Example Usage
# Define a search operator (assuming it's already implemented)
search_operator <- DefaultOperator(value = "ribosome")

# Perform search and get results
pdb_ids <- perform_search(search_operator, return_type = "ENTRY",return_with_scores = T, return_raw_json_dict = T)
head(pdb_ids)
