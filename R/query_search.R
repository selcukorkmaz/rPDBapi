#' Perform a Search Query Against the RCSB Protein Data Bank
#'
#' This function allows users to search the RCSB Protein Data Bank (PDB) using various query types.
#' This function interacts with the RCSB PDB's REST API to retrieve data based on the provided search term and query type.
#' It supports different query modes, including full-text search, PubMed ID, organism query, and more.
#'
#' @param search_term A string specifying the term to search in the PDB database. This could be a keyword, PubMed ID,
#'   taxonomy ID, experimental method, author name, or other searchable terms depending on the `query_type`.
#' @param query_type A string specifying the type of query to perform. Supported values include:
#'   \describe{
#'     \item{\code{"full_text"}}{Performs a full-text search across all entries. Default query type.}
#'     \item{\code{"PubmedIdQuery"}}{Searches for entries associated with a specific PubMed ID.}
#'     \item{\code{"TreeEntityQuery"}}{Searches for entries based on NCBI Taxonomy ID.}
#'     \item{\code{"ExpTypeQuery"}}{Searches for entries based on the experimental method used, such as "X-RAY DIFFRACTION".}
#'     \item{\code{"AdvancedAuthorQuery"}}{Searches for entries based on author names.}
#'     \item{\code{"OrganismQuery"}}{Searches for entries based on organism names.}
#'     \item{\code{"pfam"}}{Searches for entries based on Pfam IDs.}
#'     \item{\code{"uniprot"}}{Searches for entries based on UniProt IDs.}
#'   }
#'   Default is \code{"full_text"}.
#' @param return_type A string specifying the type of search result to return.
#'   Possible values are:
#'   \describe{
#'     \item{\code{"entry"}}{Returns a list of PDB entry identifiers that match the search criteria. Default.}
#'     \item{\code{"polymer_entity"}}{Returns detailed information about polymer entities that match the search criteria.}
#'   }
#' @param scan_params Additional parameters for the scan, provided as a list. This is usually \code{NULL} by default and
#'   typically only used for advanced queries where specific request options need to be defined.
#' @param num_attempts An integer specifying the number of attempts to try the query in case of failure due to network issues
#'   or temporary server unavailability. Default is 1 attempt.
#' @param sleep_time A numeric value specifying the time in seconds to wait between attempts if the query fails and multiple
#'   attempts are specified. Default is 0.5 seconds.
#'
#' @return Depending on the \code{return_type} specified, the function either returns:
#'   \describe{
#'     \item{PDB IDs:}{A list of PDB entry identifiers if \code{return_type} is \code{"entry"}.}
#'     \item{Full Response:}{The complete response object from the API if \code{return_type} is \code{"polymer_entity"}.}
#'   }
#'   If the query fails, the function returns \code{NULL}.
#'
#' @importFrom httr POST content content_type http_status
#' @importFrom jsonlite toJSON fromJSON
#'
#' @examples
#' \donttest{
#' # Example 1: Get a list of PDBs for a specific search term using full-text search
#' pdbs <- query_search("ribosome")
#' head(pdbs)
#'
#' # Example 2: Search by PubMed ID Number
#' pdbs_by_pubmedid <- query_search(search_term = "27499440", query_type = "PubmedIdQuery")
#' head(pdbs_by_pubmedid)
#'
#' # Example 3: Search by source organism using NCBI Taxonomy ID
#' pdbs_by_ncbi_taxid <- query_search(search_term = "6239", query_type = "TreeEntityQuery")
#' head(pdbs_by_ncbi_taxid)
#'
#' # Example 4: Search for entries related to a specific experimental method
#' pdbs_by_experiment <- query_search(search_term = "X-RAY DIFFRACTION", query_type = "ExpTypeQuery")
#' head(pdbs_by_experiment)
#' }
#'
#' @details
#' The `query_search` function is a powerful tool for querying the RCSB PDB using a variety of search criteria.
#' Depending on the specified \code{query_type}, the function will adjust the search parameters and endpoint used.
#' For instance, a \code{"full_text"} query will search across all text fields in the PDB entries, whereas a
#' \code{"TreeEntityQuery"} will specifically search based on taxonomy IDs.
#'
#' The function includes robust error handling to manage network issues and invalid input scenarios.
#' For example, if an unsupported \code{query_type} is provided, the function will stop execution with an informative error message.
#' Similarly, if the search term does not match any entries or if the server fails to return a response,
#' the function will attempt the request a specified number of times before returning \code{NULL}.
#'
#' Users can customize the behavior of the search by adjusting the \code{scan_params},
#' \code{num_attempts}, and \code{sleep_time} parameters to fine-tune the query execution.
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
  } else if (query_type != "full_text") {
    stop("Unsupported Query Type: The query_type '", query_type, "' is not supported. Please use one of the following: 'full_text', 'PubmedIdQuery', 'TreeEntityQuery', 'ExpTypeQuery', 'AdvancedAuthorQuery', 'OrganismQuery', 'pfam', 'uniprot'.")
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

  # Define the base query parameters
  scan_params <- list(query = query_params, return_type = return_type, request_options = list(results_verbosity = "verbose"))

  # Additional handling for 'entry' return type
  if (return_type == "entry") {
    scan_params$request_options$return_all_hits <- TRUE
  }

  url <- "https://search.rcsb.org/rcsbsearch/v2/query?json="

  query_text <- toJSON(scan_params, auto_unbox = TRUE, pretty = TRUE)

  for (attempt in 1:num_attempts) {
    response <- tryCatch(
      {
        POST(url, body = query_text, encode = "json", content_type("application/json"))
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
            stop("Parsing Error: The server response could not be parsed. Please check the validity of the search term '", search_term, "' and try again.")
          }
        )

        if (return_type == "entry") {
          idlist <- walk_nested_dict(response_val, "identifier", maxdepth = 25, outputs = list())
          return(idlist[[1]])
        } else {
          return(response_val)
        }
      } else {
        warning("Request failed with status: ", http_status(response)$message, ". Response content: ", content(response, "text", encoding = "UTF-8"))
      }
    }
    Sys.sleep(sleep_time)
  }

  warning("All retrieval attempts failed after ", num_attempts, " tries. Please check the search term, query type, and your network connection.")
  return(NULL)
}
