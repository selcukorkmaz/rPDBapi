#' Perform a Search in the RCSB PDB
#'
#' This function allows users to perform highly customizable searches in the RCSB Protein Data Bank (PDB) by specifying detailed search criteria. It interfaces directly with the RCSB PDB's RESTful API, enabling complex queries to retrieve specific data, such as PDB entries, assemblies, polymer entities, non-polymer entities, and more.
#'
#' @param search_operator An object that specifies the search criteria. This object can be constructed using various operator functions:
#' \describe{
#'   \item{\code{DefaultOperator}}{A basic search operator for general search operations.}
#'   \item{\code{ExactMatchOperator}}{For exact match searches, matching an exact attribute value.}
#'   \item{\code{InOperator}}{For searching where the attribute value must be within a specified set of values.}
#'   \item{\code{ContainsWordsOperator}}{For searching attributes that contain certain words.}
#'   \item{\code{ContainsPhraseOperator}}{For searching attributes that contain a specific phrase.}
#'   \item{\code{ComparisonOperator}}{For comparison-based searches, such as finding values greater than, less than, or equal to a specified value.}
#'   \item{\code{RangeOperator}}{For searching within a range of values for a given attribute.}
#'   \item{\code{ExistsOperator}}{To check the existence of a specific attribute in the database.}
#'   \item{\code{StructureOperator}}{For structure-based searches, using PDB entry IDs, assembly IDs, and search modes.}
#'   \item{\code{SequenceOperator}}{For sequence-based searches, using sequences, sequence types, and cutoffs for e-value and identity.}
#'   \item{\code{SeqMotifOperator}}{For searching sequence motifs, using pattern types like SIMPLE, PROSITE, or REGEX.}
#'   \item{\code{ChemicalOperator}}{For chemical structure searches, using SMILES or InChI descriptors and various matching criteria.}
#' }
#' These operators allow you to build complex search queries tailored to your specific needs.
#'
#' @param return_type A string specifying the type of data to return. The available options for \code{return_type} include:
#' \describe{
#'   \item{\code{"ENTRY"}}{Returns a list of PDB IDs corresponding to the entries that match the search criteria. This is the default option and provides entry-level information.}
#'   \item{\code{"ASSEMBLY"}}{Returns a list of PDB IDs appended with assembly IDs (formatted as \code{"PDB_ID-ASSEMBLY_ID"}). Useful for accessing specific biological assemblies.}
#'   \item{\code{"POLYMER_ENTITY"}}{Returns a list of PDB IDs appended with entity IDs for polymeric molecular entities. Useful for examining specific polymer chains.}
#'   \item{\code{"NON_POLYMER_ENTITY"}}{Returns a list of PDB IDs appended with entity IDs for non-polymeric entities, such as ligands or small molecules. Useful for detailed chemical analysis.}
#'   \item{\code{"POLYMER_INSTANCE"}}{Returns a list of PDB IDs appended with asym IDs, representing specific instances of polymeric entities (e.g., protein chains).}
#'   \item{\code{"CHEMICAL_COMPONENT"}}{Returns a list of chemical component identifiers, useful for detailed chemical analysis.}
#' }
#'
#' @param request_options A list of additional options to further customize the search request. These options can include:
#' \describe{
#'   \item{\code{facets}}{Faceted queries allow aggregation of search results into categories (buckets) based on the requested field values. Useful for statistical analysis and data aggregation.}
#'   \item{\code{sort_by}}{Defines the sorting criteria for the search results (e.g., by resolution, release date).}
#'   \item{\code{pagination}}{Controls how many results to return per page and which page of results to return. Useful for handling large datasets.}
#'   \item{\code{return_all_hits}}{If set to \code{TRUE}, the search returns all matching results; otherwise, a limited set is returned.}
#' }
#'
#' @param return_with_scores Logical; if \code{TRUE}, the search results will include relevance scores. Useful when prioritizing results based on their relevance to the search criteria. Default is \code{FALSE}.
#'
#' @param return_raw_json_dict Logical; if \code{TRUE}, the function returns the raw JSON response from the PDB API. This option is valuable for advanced users who wish to process the raw data themselves or need access to additional details. Default is \code{FALSE}.
#'
#' @param verbosity Logical; if \code{TRUE}, detailed messages will be displayed during execution, providing insights into the query being sent and the response received. Verbose mode is useful for debugging or when you need insights into the function's operation. Default is \code{TRUE}.
#'
#' @return The function returns search results based on the specified \code{return_type}:
#' \describe{
#'   \item{\code{ENTRY}}{A vector of PDB IDs that match the search criteria.}
#'   \item{\code{ASSEMBLY}}{A list of PDB IDs with appended assembly IDs, formatted as \code{"PDB_ID-ASSEMBLY_ID"}.}
#'   \item{\code{POLYMER_ENTITY}}{A list of PDB IDs with appended entity IDs for polymeric chains.}
#'   \item{\code{NON_POLYMER_ENTITY}}{A list of PDB IDs with appended entity IDs for non-polymeric components.}
#'   \item{\code{POLYMER_INSTANCE}}{A list of PDB IDs with appended asym IDs for specific polymer instances.}
#'   \item{\code{CHEMICAL_COMPONENT}}{A list of chemical component identifiers.}
#' }
#'
#' @importFrom httr POST
#' @importFrom jsonlite toJSON
#' @examples
#' \donttest{
#' # Example 1: Search for Polymer Entities from Mus musculus and Homo sapiens
#' search_operator <- InOperator(
#'   attribute = "rcsb_entity_source_organism.taxonomy_lineage.name",
#'   value = c("Mus musculus", "Homo sapiens")
#' )
#' results <- perform_search(
#'   search_operator = search_operator,
#'   return_type = "POLYMER_ENTITY"
#' )
#' results
#'
#' # Example 2: Search for Entries Released After a Specific Date
#' operator_date <- ComparisonOperator(
#'   attribute = "rcsb_accession_info.initial_release_date",
#'   value = "2019-08-20",
#'   comparison_type = "GREATER"
#' )
#' request_options <- list(
#'   facets = list(
#'     list(
#'       name = "Methods",
#'       aggregation_type = "terms",
#'       attribute = "exptl.method"
#'     )
#'   )
#' )
#' results <- perform_search(
#'   search_operator = operator_date,
#'   return_type = "ENTRY",
#'   request_options = request_options
#' )
#' results
#'
#' # Example 3: Search for Symmetric Dimers with DNA-Binding Domain
#' operator_symbol <- ExactMatchOperator(
#'   attribute = "rcsb_struct_symmetry.symbol",
#'   value = "C2"
#' )
#' operator_kind <- ExactMatchOperator(
#'   attribute = "rcsb_struct_symmetry.kind",
#'   value = "Global Symmetry"
#' )
#' operator_full_text <- DefaultOperator(
#'   value = "\"heat-shock transcription factor\""
#' )
#' operator_dna_count <- ComparisonOperator(
#'   attribute = "rcsb_entry_info.polymer_entity_count_DNA",
#'   value = 1,
#'   comparison_type = "GREATER_OR_EQUAL"
#' )
#' query_group <- list(
#'   type = "group",
#'   logical_operator = "and",
#'   nodes = list(
#'     list(
#'       type = "terminal",
#'       service = "text",
#'       parameters = operator_symbol
#'     ),
#'     list(
#'       type = "terminal",
#'       service = "text",
#'       parameters = operator_kind
#'     ),
#'     list(
#'       type = "terminal",
#'       service = "full_text",
#'       parameters = operator_full_text
#'     ),
#'     list(
#'       type = "terminal",
#'       service = "text",
#'       parameters = operator_dna_count
#'     )
#'   )
#' )
#' results <- perform_search(
#'   search_operator = query_group,
#'   return_type = "ASSEMBLY"
#' )
#' results
#' }
#' @export

perform_search <- function(search_operator, return_type = "ENTRY", request_options = NULL, return_with_scores = FALSE, return_raw_json_dict = FALSE, verbosity = TRUE) {
  results <- tryCatch(
    {
      perform_search_with_graph(
        query_object = search_operator,
        return_type = return_type,
        request_options = request_options,
        return_with_scores = return_with_scores,
        return_raw_json_dict = return_raw_json_dict,
        verbosity = verbosity
      )
    },
    error = function(e) {
      stop("Failed to fetch data from the RCSB database: ", e$message)
    }
  )
  return(results)
}

perform_search_with_graph <- function(query_object, return_type = "ENTRY", request_options = NULL, return_with_scores = FALSE, return_raw_json_dict = FALSE, verbosity = TRUE) {
  # Validate return_type
  if (!return_type %in% c("ENTRY", "ASSEMBLY", "POLYMER_ENTITY", "NONPOLYMER_ENTITY", "POLYMER_INSTANCE", "NONPOLYMER_INSTANCE", "MOL_DEFINITION")) {
    stop("Invalid return_type '", return_type, "'. Supported types are: 'ENTRY', 'ASSEMBLY', 'POLYMER_ENTITY', 'NONPOLYMER_ENTITY', 'POLYMER_INSTANCE', 'NONPOLYMER_INSTANCE', 'MOL_DEFINITION'.")
  }

  # Cast query object if necessary
  cast_query_object <- tryCatch(
    {
      if ((is.null(query_object$operator) || query_object$operator %in% SEARCH_OPERATORS) && is.null(query_object$type)) {
        QueryNode(query_object)
      } else {
        query_object
      }
    },
    error = function(e) {
      stop("Failed to process the query object: ", e$message)
    }
  )

  request_options_dict <- if (!is.null(request_options)) {
    request_options
  } else {
    list(return_all_hits = TRUE)
  }

  rcsb_query_dict <- list(
    query = cast_query_object,
    request_options = request_options_dict,
    return_type = ReturnType[[return_type]]
  )

  if (verbosity) {
    message("Querying RCSB Search with the following parameters:\n", toJSON(rcsb_query_dict, auto_unbox = TRUE, pretty = TRUE))
  }

  response <- tryCatch(
    {
      POST(
        url = SEARCH_URL_ENDPOINT,
        body = toJSON(rcsb_query_dict, auto_unbox = TRUE, pretty = TRUE),
        encode = "json",
        content_type("application/json")
      )
    },
    error = function(e) {
      stop("HTTP request to RCSB failed: ", e$message)
    }
  )

  if (http_status(response)$category != "Success") {
    stop("RCSB search request failed with status ", http_status(response)$status, ": ", http_status(response)$message, "\nResponse content: ", content(response, "text", encoding = "UTF-8"))
  }

  content <- tryCatch(
    {
      content(response, "text", encoding = "UTF-8")
    },
    error = function(e) {
      stop("Failed to retrieve content from RCSB response: ", e$message)
    }
  )

  response_json <- tryCatch(
    {
      fromJSON(content)
    },
    error = function(e) {
      stop("Failed to parse RCSB response as JSON: ", e$message, "\nResponse content: ", content)
    }
  )

  if (return_raw_json_dict) {
    return(response_json)
  }

  results <- tryCatch(
    {
      if (return_with_scores) {
        response_json$result_set
      } else {
        response_json$result_set$identifier
      }
    },
    error = function(e) {
      stop("Failed to extract results from RCSB response JSON: ", e$message)
    }
  )

  if (is.null(results)) {
    stop("No results were found for the given query. Please check your search criteria.")
  }

  return(results)
}

