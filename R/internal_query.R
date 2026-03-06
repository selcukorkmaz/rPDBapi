# Internal query helpers for query_search().

rpdbapi_supported_experimental_methods <- function() {
  c(
    "X-RAY DIFFRACTION", "ELECTRON MICROSCOPY",
    "SOLID-STATE NMR", "SOLUTION NMR", "NEUTRON DIFFRACTION",
    "ELECTRON CRYSTALLOGRAPHY", "POWDER DIFFRACTION",
    "FIBER DIFFRACTION", "SOLUTION SCATTERING", "EPR",
    "FLUORESCENCE TRANSFER", "INFRARED SPECTROSCOPY",
    "THEORETICAL MODEL"
  )
}

rpdbapi_resolve_query_type <- function(search_term, query_type) {
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
    if (!search_term %in% rpdbapi_supported_experimental_methods()) {
      rpdbapi_abort(
        "Invalid Experimental Method: The provided experimental method is not recognized. Please ensure it matches one of the accepted values (e.g., 'X-RAY DIFFRACTION').",
        class = "rPDBapi_error_invalid_input",
        function_name = "query_search",
        query_type = "ExpTypeQuery",
        search_term = search_term
      )
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

  list(
    search_term = search_term,
    query_type = query_type,
    query_subtype = query_subtype
  )
}

rpdbapi_validate_query_search_return_type <- function(return_type) {
  if (!return_type %in% c("entry", "polymer_entity")) {
    rpdbapi_abort(
      paste0("Invalid Return Type: The return_type '", return_type, "' is not valid. Please use 'entry' or 'polymer_entity'."),
      class = "rPDBapi_error_invalid_input",
      function_name = "query_search",
      return_type = return_type
    )
  }
}

rpdbapi_build_query_params <- function(search_term, query_type) {
  query_params <- list(
    type = "terminal",
    service = query_type
  )

  if (query_type %in% c("full_text", "text")) {
    query_params$parameters <- list(value = search_term)
  } else if (query_type == "sequence") {
    query_params$parameters <- list(target = "pdb_protein_sequence", value = search_term)
  } else if (query_type == "structure") {
    query_params$parameters <- list(
      operator = "relaxed_shape_match",
      value = list(entry_id = search_term, assembly_id = "1")
    )
  } else {
    rpdbapi_abort(
      paste0("Unsupported Query Type: The query_type '", query_type, "' is not supported. Please use one of the following: 'full_text', 'PubmedIdQuery', 'TreeEntityQuery', 'ExpTypeQuery', 'AdvancedAuthorQuery', 'OrganismQuery', 'pfam', 'uniprot', 'sequence', 'structure'."),
      class = "rPDBapi_error_invalid_input",
      function_name = "query_search",
      query_type = query_type
    )
  }

  query_params
}

rpdbapi_apply_query_subtype <- function(query_params, query_subtype, search_term) {
  if (is.null(query_subtype)) {
    return(query_params)
  }

  if (query_subtype == "pmid") {
    query_params$parameters <- list(
      operator = "in",
      negation = FALSE,
      value = list(search_term),
      attribute = "rcsb_pubmed_container_identifiers.pubmed_id"
    )
  } else if (query_subtype == "taxid") {
    query_params$parameters <- list(
      operator = "exact_match",
      negation = FALSE,
      value = as.character(search_term),
      attribute = "rcsb_entity_source_organism.taxonomy_lineage.id"
    )
  } else if (query_subtype == "experiment_type") {
    query_params$parameters <- list(
      operator = "exact_match",
      negation = FALSE,
      value = as.character(search_term),
      attribute = "exptl.method"
    )
  } else if (query_subtype == "author") {
    query_params$parameters <- list(
      operator = "exact_match",
      negation = FALSE,
      value = as.character(search_term),
      attribute = "rcsb_primary_citation.rcsb_authors"
    )
  } else if (query_subtype == "organism") {
    query_params$parameters <- list(
      operator = "contains_words",
      negation = FALSE,
      value = as.character(search_term),
      attribute = "rcsb_entity_source_organism.taxonomy_lineage.name"
    )
  } else if (query_subtype == "pfam") {
    query_params$parameters <- list(
      operator = "exact_match",
      negation = FALSE,
      value = as.character(search_term),
      attribute = "rcsb_polymer_entity_annotation.annotation_id"
    )
  } else if (query_subtype == "uniprot") {
    query_params$parameters <- list(
      operator = "exact_match",
      negation = FALSE,
      value = as.character(search_term),
      attribute = "rcsb_polymer_entity_container_identifiers.reference_sequence_identifiers.database_accession"
    )
  }

  query_params
}

rpdbapi_build_scan_params <- function(query_params, return_type, scan_params) {
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

  scan_params$return_type <- return_type
  if (is.null(scan_params$request_options)) {
    scan_params$request_options <- list()
  }
  if (return_type == "entry" && is.null(scan_params$request_options$return_all_hits)) {
    scan_params$request_options$return_all_hits <- TRUE
  }

  scan_params
}
