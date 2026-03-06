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
#' @param scan_params Additional parameters for the scan, provided as a list.
#'                    This is `NULL` by default and typically only used for advanced queries.
#' @param num_attempts An integer specifying the number of attempts to try the query in case of failure.
#' @param sleep_time A numeric value specifying the time in seconds to wait between attempts.
#'
#' @return
#' If \code{return_type = "entry"}, returns a character vector of identifiers with class
#' \code{"rPDBapi_query_ids"}. Otherwise returns the parsed API payload with class
#' \code{"rPDBapi_query_response"}.
#'
#' @importFrom httr POST content content_type
#' @importFrom jsonlite toJSON fromJSON
#'
#' @examples
#' # Get a list of PDBs for a specific search term
#' \donttest{
#' # Search Functions by Specific Terms
#' pdbs <- query_search("ribosome")
#' head(pdbs)
#'
#' # Search by PubMed ID Number
#' pdbs_by_pubmedid <- query_search(search_term = 27499440, query_type = "PubmedIdQuery")
#' head(pdbs_by_pubmedid)
#'
#' # Search by source organism using NCBI TaxId
#' pdbs_by_ncbi_taxid <- query_search(search_term = "6239", query_type = "TreeEntityQuery")
#' head(pdbs_by_ncbi_taxid)
#'
#' # Search by Experimental Method
#' pdbs = query_search(search_term = 'SOLID-STATE NMR', query_type='ExpTypeQuery')
#' head(pdbs)
#'
#' pdbs = query_search(search_term = '4HHB', query_type="structure")
#' head(pdbs)
#'
#' ## Advanced Searches
#'
#' # Search by Author
#' pdbs = query_search(search_term = 'Rzechorzek, N.J.', query_type='AdvancedAuthorQuery')
#' head(pdbs)
#'
#' # Search by Organism
#' pdbs = query_search(search_term = "Escherichia coli", query_type="OrganismQuery")
#' head(pdbs)
#'
#' # Search by Uniprot ID (Escherichia coli beta-lactamase)
#' pdbs = query_search(search_term = "P0A877", query_type="uniprot")
#' head(pdbs)
#'
#' # Search by PFAM number (protein kinase domain)
#' pdbs = query_search(search_term = "PF00069", query_type="pfam")
#' head(pdbs)
#' }
#'
#' @export

query_search <- function(search_term, query_type = "full_text", return_type = "entry", scan_params = NULL, num_attempts = 1, sleep_time = 0.5) {
  resolved <- rpdbapi_resolve_query_type(search_term = search_term, query_type = query_type)
  search_term <- resolved$search_term
  query_type <- resolved$query_type
  query_subtype <- resolved$query_subtype

  rpdbapi_validate_query_search_return_type(return_type)

  query_params <- rpdbapi_build_query_params(search_term = search_term, query_type = query_type)
  query_params <- rpdbapi_apply_query_subtype(
    query_params = query_params,
    query_subtype = query_subtype,
    search_term = search_term
  )
  scan_params <- rpdbapi_build_scan_params(
    query_params = query_params,
    return_type = return_type,
    scan_params = scan_params
  )

  url <- "https://search.rcsb.org/rcsbsearch/v2/query?json="

  for (attempt in 1:num_attempts) {
    response <- tryCatch(
      {
        rpdbapi_http_request(
          url = url,
          method = "POST",
          body = scan_params,
          encode = "json",
          content_type_value = "application/json"
        )
      },
      error = function(e) {
        warning("HTTP request failed on attempt ", attempt, ": ", e$message)
        NULL
      }
    )

    if (!is.null(response)) {
      if (rpdbapi_http_success(response)) {
        response_val <- tryCatch(
          {
            fromJSON(rpdbapi_response_text(response))
          },
          error = function(e) {
            rpdbapi_abort(
              paste0(
                "Parsing Error: The server response could not be parsed. ",
                "Please check the validity of the search term '", search_term, "' and try again."
              ),
              class = "rPDBapi_error_malformed_response",
              function_name = "query_search"
            )
          }
        )

        if (return_type == "entry") {
          idlist <- walk_nested_dict(response_val, "identifier", maxdepth = 25, outputs = list())
          if (is.null(idlist) || length(idlist) == 0 || is.null(idlist[[1]])) {
            rpdbapi_abort(
              "Malformed search response: missing 'identifier' values.",
              class = "rPDBapi_error_malformed_response",
              function_name = "query_search"
            )
          }

          ids <- as.character(idlist[[1]])
          ids <- ids[!is.na(ids) & nzchar(ids)]
          if (length(ids) == 0) {
            rpdbapi_abort(
              "Malformed search response: empty identifier set.",
              class = "rPDBapi_error_malformed_response",
              function_name = "query_search"
            )
          }

          ids <- rpdbapi_add_class(ids, "rPDBapi_query_ids")
          attr(ids, "return_type") <- "entry"
          return(ids)
        } else {
          response_val <- rpdbapi_add_class(response_val, "rPDBapi_query_response")
          attr(response_val, "return_type") <- return_type
          return(response_val)
        }
      } else {
        warning("Request failed with status: ", rpdbapi_http_status_message(response), ". Response content: ", rpdbapi_response_text(response))
      }
    }
    Sys.sleep(sleep_time)
  }

  rpdbapi_abort(
    paste0("All retrieval attempts failed after ", num_attempts, " tries. Please check the search term, query type, and your network connection."),
    class = "rPDBapi_error_request_failed",
    function_name = "query_search",
    num_attempts = num_attempts,
    query_type = query_type
  )
}
