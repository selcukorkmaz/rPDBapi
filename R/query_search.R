#' Search Query Function
#'
#' This function performs a search query against the RCSB Protein Data Bank
#' using their REST API. It allows for various types of searches based on the provided parameters.
#'
#' @param search_term A string specifying the term to search in the database.
#' @param query_type A string specifying the type of query to perform.
#'                   Supported values include "full_text", "PubmedIdQuery",
#'                   "TreeEntityQuery", "ExpTypeQuery", "AdvancedAuthorQuery",
#'                   "OrganismQuery", "pfam", and "uniprot". Default is "full_text".
#' @param return_type A string specifying the type of search result to return.
#'                    Possible values are "entry" (default) and "polymer_entity".
#' @param scan_params An optional list containing explicit nested search terms.
#' @param num_attempts An integer specifying the number of attempts to try the query in case of failure.
#' @param sleep_time A numeric value specifying the time in seconds to wait between attempts.
#'
#' @return Depending on the return_type, it either returns a list of PDB IDs (if "entry")
#'         or the full response from the API.
#' @export

query_search <- function(search_term, query_type = "full_text", return_type = "entry", scan_params = NULL, num_attempts = 1, sleep_time = 0.5) {
  query_subtype <- NULL
  if (query_type == "PubmedIdQuery") {
    query_type <- "text"
    query_subtype <- "pmid"
  } else if (query_type == "TreeEntityQuery") {
    query_type <- "text"
    query_subtype <- "taxid"
  } else if (query_type == "ExpTypeQuery") {
    query_type <- "text"
    query_subtype <- "experiment_type"
    search_term <- toupper(search_term)
  } else if (query_type == "AdvancedAuthorQuery") {
    query_type <- "text"
    query_subtype <- "author"
  } else if (query_type == "OrganismQuery") {
    query_type <- "text"
    query_subtype <- "organism"
  } else if (query_type == "pfam") {
    query_type <- "text"
    query_subtype <- "pfam"
  } else if (query_type == "uniprot") {
    query_type <- "text"
    query_subtype <- "uniprot"
  }

  query_params <- list(type = "terminal", service = query_type, parameters = list())
  if (is.null(scan_params)) {
    if (!is.null(query_subtype)) {
      query_params$parameters <- list(attribute = query_subtype, operator = "exact_match", value = search_term)
    } else {
      query_params$parameters <- list(value = search_term)
    }
    scan_params <- list(query = query_params, return_type = return_type, request_options = list(results_verbosity = "verbose"))

    if (return_type == "entry") {
      scan_params$request_options$return_all_hits <- TRUE
    }
  }

  url <- "https://search.rcsb.org/rcsbsearch/v2/query?json="

      query_text <- toJSON(scan_params, auto_unbox = TRUE, pretty = TRUE)

      for (attempt in 1:num_attempts) {
        response <- POST(url, body = query_text, encode = "json")
        if (!is.null(response) && http_status(response)$category == "Success") {
          response_val <- fromJSON(content(response, "text", encoding = "UTF-8"))

          if (return_type == "entry") {
            idlist <- walk_nested_dict(response_val, "identifier", maxdepth = 25, outputs = list())
            return(idlist)
          } else {
            return(response_val)
          }
        }
        Sys.sleep(sleep_time)
      }

      stop("Retrieval failed, returning NULL")
    }
