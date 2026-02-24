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
  # Define query subtype as NULL initially
  query_subtype <- NULL

  # Adjust query_type and query_subtype based on input
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
    if (!search_term %in% c("X-RAY DIFFRACTION", "ELECTRON MICROSCOPY",
                            "SOLID-STATE NMR", "SOLUTION NMR", "NEUTRON DIFFRACTION",
                            "ELECTRON CRYSTALLOGRAPHY", "POWDER DIFFRACTION",
                            "FIBER DIFFRACTION", "SOLUTION SCATTERING", "EPR",
                            "FLUORESCENCE TRANSFER", "INFRARED SPECTROSCOPY",
                            "THEORETICAL MODEL")) {
      stop("Invalid Experimental Method: The provided experimental method is not recognized. Please ensure it matches one of the accepted values (e.g., 'X-RAY DIFFRACTION').")
    }
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

  # Check for valid return_type
  if (!return_type %in% c("entry", "polymer_entity")) {
    stop("Invalid Return Type: The return_type '", return_type, "' is not valid. Please use 'entry' or 'polymer_entity'.")
  }

  # Initialize query parameters
  query_params <- list()
  query_params$type <- "terminal"
  query_params$service <- query_type

  # Handle different query types
  if (query_type %in% c("full_text", "text")) {
    query_params$parameters <- list(value = search_term)
  } else if (query_type == "sequence") {
    query_params$parameters <- list(target = "pdb_protein_sequence", value = search_term)
  } else if (query_type == "structure") {
    query_params$parameters <- list(operator = "relaxed_shape_match", value = list(entry_id = search_term, assembly_id = "1"))
  }else{
    stop("Unsupported Query Type: The query_type '", query_type, "' is not supported. Please use one of the following: 'full_text', 'PubmedIdQuery', 'TreeEntityQuery', 'ExpTypeQuery', 'AdvancedAuthorQuery', 'OrganismQuery', 'pfam', 'uniprot', 'sequence', 'structure'.")
  }


  # Apply query_subtype conditions if applicable
  if (!is.null(query_subtype)) {
    if (query_subtype == "pmid") {
      query_params$parameters <- list(operator = "in", negation = FALSE, value = list(search_term), attribute = "rcsb_pubmed_container_identifiers.pubmed_id")
    } else if (query_subtype == "taxid") {
      query_params$parameters <- list(operator = "exact_match", negation = FALSE, value = as.character(search_term), attribute = "rcsb_entity_source_organism.taxonomy_lineage.id")
    } else if (query_subtype == "experiment_type") {
      query_params$parameters <- list(operator = "exact_match", negation = FALSE, value = as.character(search_term), attribute = "exptl.method")
    } else if (query_subtype == "author") {
      query_params$parameters <- list(operator = "exact_match", negation = FALSE, value = as.character(search_term), attribute = "rcsb_primary_citation.rcsb_authors")
    } else if (query_subtype == "organism") {
      query_params$parameters <- list(operator = "contains_words", negation = FALSE, value = as.character(search_term), attribute = "rcsb_entity_source_organism.taxonomy_lineage.name")
    } else if (query_subtype == "pfam") {
      query_params$parameters <- list(operator = "exact_match", negation = FALSE, value = as.character(search_term), attribute = "rcsb_polymer_entity_annotation.annotation_id")
    } else if (query_subtype == "uniprot") {
      query_params$parameters <- list(operator = "exact_match", negation = FALSE, value = as.character(search_term), attribute = "rcsb_polymer_entity_container_identifiers.reference_sequence_identifiers.database_accession")
    }
  }

  # Define default query parameters and allow user-provided overrides.
  default_scan_params <- list(
    query = query_params,
    return_type = return_type,
    request_options = list(results_verbosity = "verbose")
  )
  if (is.null(scan_params)) {
    scan_params <- default_scan_params
  } else {
    if (!is.list(scan_params)) {
      rpdbapi_abort(
        "Invalid scan_params: expected a list.",
        class = "rPDBapi_error_invalid_input",
        function_name = "query_search"
      )
    }
    scan_params <- utils::modifyList(default_scan_params, scan_params, keep.null = TRUE)
  }

  # Keep response parsing and request return_type in sync with the function argument.
  scan_params$return_type <- return_type

  # Additional handling for 'entry' return type
  if (is.null(scan_params$request_options)) {
    scan_params$request_options <- list()
  }
  if (return_type == "entry" && is.null(scan_params$request_options$return_all_hits)) {
    scan_params$request_options$return_all_hits <- TRUE
  }

  url <- "https://search.rcsb.org/rcsbsearch/v2/query?json="

  for (attempt in 1:num_attempts) {
    response <- tryCatch(
      {
        POST(url, body = scan_params, encode = "json", content_type("application/json"))
      },
      error = function(e) {
        warning("HTTP request failed on attempt ", attempt, ": ", e$message)
        NULL
      }
    )

    if (!is.null(response)) {
      if (http_status(response)$category == "Success") {
        response_val <- tryCatch(
          {
            fromJSON(content(response, "text", encoding = "UTF-8"))
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
        warning("Request failed with status: ", http_status(response)$message, ". Response content: ", content(response, "text", encoding = "UTF-8"))
      }
    }
    Sys.sleep(sleep_time)
  }

  stop("All retrieval attempts failed after ", num_attempts, " tries. Please check the search term, query type, and your network connection.")
}
