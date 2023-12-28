library(httr)
library(jsonlite)

# Constants
SEARCH_URL_ENDPOINT <- "https://search.rcsb.org/rcsbsearch/v2/query"

# Logical Operators
LogicalOperator <- c("AND" = "and", "OR" = "or")

# Return Types
ReturnType <- c("ENTRY" = "entry", "ASSEMBLY" = "assembly", "POLYMER_ENTITY" = "polymer_entity", "NON_POLYMER_ENTITY" = "non_polymer_entity", "POLYMER_INSTANCE" = "polymer_instance")

# Query Group Class
QueryGroup <- function(queries, logical_operator) {
  list(
    queries = queries,
    logical_operator = logical_operator,
    to_dict = function() {
      list(
        type = "group",
        logical_operator = LogicalOperator[[logical_operator]],
        nodes = lapply(queries, function(query) {
          if (inherits(query, "QueryGroup")) {
            query$to_dict()
          } else {
            QueryNode(query)$to_dict()
          }
        })
      )
    }
  )
}

# Request Options Class
RequestOptions <- function(result_start_index = NULL, num_results = NULL, sort_by = "score", desc = TRUE) {
  list(
    result_start_index = result_start_index,
    num_results = num_results,
    sort_by = sort_by,
    desc = desc,
    to_dict = function() {
      result_dict <- list()
      if (!is.null(result_start_index) && !is.null(num_results)) {
        result_dict[["paginate"]] <- list(start = result_start_index, rows = num_results)
      }
      if (!is.null(sort_by) && !is.null(desc)) {
        result_dict[["sort"]] <- list(list(sort_by = sort_by, direction = ifelse(desc, "desc", "asc")))
      }
      result_dict
    }
  )
}

# Scored Result Class
ScoredResult <- function(entity_id, score) {
  list(entity_id = entity_id, score = score)
}

# Query Node Class
QueryNode <- function(search_operator) {
  list(
    search_operator = search_operator,
    to_dict = function() {
      list(
        type = "terminal",
        service = infer_search_service(search_operator),
        parameters = search_operator$to_dict()
      )
    }
  )
}

# Infer Search Service Function
infer_search_service <- function(search_operator) {
  # Implementation depends on the structure of search operators
  # This is a placeholder, needs specific implementation based on Python code
  if (inherits(search_operator, "TextSearchOperator")) {
    return("text")
  } else if (inherits(search_operator, "SequenceOperator")) {
    return("sequence")
  } else if (inherits(search_operator, "StructureOperator")) {
    return("structure")
  } else if (inherits(search_operator, "SeqMotifOperator")) {
    return("seqmotif")
  } else if (inherits(search_operator, "ChemicalOperator")) {
    return("chemical")
  } else {
    stop("Cannot infer Search Service for the provided search operator")
  }
}

# Perform Search With Graph Function
perform_search_with_graph <- function(query_object, return_type = "ENTRY", request_options = NULL, return_with_scores = FALSE, return_raw_json_dict = FALSE, verbosity = TRUE) {
  if (inherits(query_object, c("TextSearchOperator", "SequenceOperator", "StructureOperator", "SeqMotifOperator"))) {
    query_object <- QueryNode(query_object)
  }

  if (!is.null(request_options)) {
    request_options_dict <- request_options$to_dict()
  } else {
    request_options_dict <- list(return_all_hits = TRUE)
  }

  rcsb_query_dict <- list(
    query = query_object$to_dict(),
    request_options = request_options_dict,
    return_type = ReturnType[[return_type]]
  )

  if (verbosity) {
    cat("Querying RCSB Search using the following parameters:\n", toJSON(rcsb_query_dict), "\n")
  }

  response <- POST(url = SEARCH_URL_ENDPOINT, body = toJSON(rcsb_query_dict), encode = "json")

  if (http_status(response)$category != "success") {
    warning("Request failed with:", content(response, "text"))
    stop("HTTP request error")
  }

  if (return_raw_json_dict) {
    return(fromJSON(content(response, "text", encoding = "UTF-8")))
  }

  results <- lapply(fromJSON(content(response, "text", encoding = "UTF-8"))$result_set, function(hit) {
    if (return_with_scores) {
      ScoredResult(hit$identifier, hit$score)
    } else {
      hit$identifier
    }
  })

  return(results)
}

# Perform Search Function
perform_search <- function(search_operator, return_type = "ENTRY", request_options = NULL, return_with_scores = FALSE, return_raw_json_dict = FALSE, verbosity = TRUE) {
  perform_search_with_graph(search_operator, return_type, request_options, return_with_scores, return_raw_json_dict, verbosity)
}

# Search Service (similar to Enum in Python)
SearchService <- c("BASIC_SEARCH" = "full_text", "TEXT" = "text", "SEQUENCE" = "sequence", "SEQMOTIF" = "seqmotif", "STRUCTURE" = "structure", "CHEMICAL" = "chemical")

# Cannot Infer Search Service Exception
# In R, we generally use simple error messages rather than custom exception classes
cannot_infer_search_service_exception <- function(message) {
  stop(message)
}

# Infer Search Service Function
infer_search_service <- function(search_operator) {
  # Assuming the existence of specific classes for each operator type
  if (inherits(search_operator, "DefaultOperator")) {
    return(SearchService[["BASIC_SEARCH"]])
  } else if (inherits(search_operator, "TextSearchOperators")) {
    return(SearchService[["TEXT"]])
  } else if (inherits(search_operator, "SequenceOperator")) {
    return(SearchService[["SEQUENCE"]])
  } else if (inherits(search_operator, "StructureOperator")) {
    return(SearchService[["STRUCTURE"]])
  } else if (inherits(search_operator, "SeqMotifOperator")) {
    return(SearchService[["SEQMOTIF"]])
  } else if (inherits(search_operator, "ChemicalOperator")) {
    return(SearchService[["CHEMICAL"]])
  } else {
    cannot_infer_search_service_exception(
      sprintf("Cannot infer Search Service for %s", class(search_operator))
    )
  }
}

# Query Node Class
QueryNode <- function(search_operator) {
  list(
    search_operator = search_operator,
    to_dict = function() {
      list(
        type = "terminal",
        service = infer_search_service(search_operator),
        parameters = search_operator$to_dict()
      )
    }
  )
}

