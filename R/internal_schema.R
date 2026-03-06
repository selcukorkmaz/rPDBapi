# Internal schema helpers for property discovery and validation.

rpdbapi_schema_registry <- function() {
  list(
    ENTRY = list(
      rcsb_id = character(0),
      struct = c("title"),
      struct_keywords = c("pdbx_keywords"),
      exptl = c("method"),
      cell = c("length_a", "length_b", "length_c", "volume", "angle_beta"),
      citation = c("title"),
      rcsb_primary_citation = c("title"),
      rcsb_entry_info = c("molecular_weight", "resolution_combined", "polymer_entity_count_DNA"),
      rcsb_accession_info = c("initial_release_date", "deposit_date")
    ),
    ASSEMBLY = list(
      rcsb_id = character(0),
      pdbx_struct_assembly = c("details", "method_details", "oligomeric_count"),
      rcsb_struct_symmetry = c("kind", "symbol")
    ),
    POLYMER_ENTITY = list(
      rcsb_id = character(0),
      rcsb_entity_source_organism = c("ncbi_taxonomy_id", "ncbi_scientific_name"),
      rcsb_cluster_membership = c("cluster_id", "identity")
    ),
    BRANCHED_ENTITY = list(
      rcsb_id = character(0)
    ),
    NONPOLYMER_ENTITY = list(
      rcsb_id = character(0),
      rcsb_nonpolymer_entity = c("details", "formula_weight", "pdbx_description"),
      rcsb_nonpolymer_entity_container_identifiers = c("chem_ref_def_id")
    ),
    POLYMER_ENTITY_INSTANCE = list(
      rcsb_id = character(0)
    ),
    BRANCHED_ENTITY_INSTANCE = list(
      rcsb_id = character(0)
    ),
    NONPOLYMER_ENTITY_INSTANCE = list(
      rcsb_id = character(0)
    ),
    CHEMICAL_COMPONENT = list(
      rcsb_id = character(0),
      chem_comp = c("id", "type", "formula_weight", "name", "formula"),
      rcsb_chem_comp_info = c("initial_release_date"),
      rcsb_chem_comp_descriptor = c("smiles")
    )
  )
}

rpdbapi_supported_data_types <- function() {
  names(rpdbapi_schema_registry())
}

rpdbapi_validate_data_type <- function(data_type, function_name = "validate_properties") {
  if (!data_type %in% rpdbapi_supported_data_types()) {
    rpdbapi_abort(
      paste0("Invalid 'data_type': ", data_type, "."),
      class = "rPDBapi_error_invalid_input",
      function_name = function_name,
      data_type = data_type
    )
  }
}

rpdbapi_normalize_property_values <- function(values) {
  if (is.null(values) || length(values) == 0) {
    return(character(0))
  }
  as.character(unlist(values, recursive = TRUE, use.names = FALSE))
}

rpdbapi_property_validation_details <- function(properties, data_type) {
  rpdbapi_validate_data_type(data_type, function_name = "validate_properties")

  if (!is.list(properties)) {
    rpdbapi_abort(
      "Property must be a list.",
      class = "rPDBapi_error_invalid_input",
      function_name = "validate_properties",
      data_type = data_type
    )
  }

  schema <- rpdbapi_schema_registry()[[data_type]]
  prop_names <- names(properties)

  if (is.null(prop_names) || anyNA(prop_names) || any(trimws(prop_names) == "")) {
    rpdbapi_abort(
      "Each property must be a named list element.",
      class = "rPDBapi_error_invalid_input",
      function_name = "validate_properties",
      data_type = data_type
    )
  }

  unknown_fields <- setdiff(prop_names, names(schema))
  unknown_subfields <- list()

  for (fname in prop_names) {
    if (fname %in% unknown_fields) {
      next
    }

    allowed_sub <- schema[[fname]]
    requested_sub <- rpdbapi_normalize_property_values(properties[[fname]])
    if (length(allowed_sub) == 0 || length(requested_sub) == 0) {
      next
    }

    missing_sub <- setdiff(requested_sub, allowed_sub)
    if (length(missing_sub) > 0) {
      unknown_subfields[[fname]] <- missing_sub
    }
  }

  list(
    unknown_fields = unknown_fields,
    unknown_subfields = unknown_subfields
  )
}

rpdbapi_maybe_validate_properties <- function(properties, data_type) {
  strict <- isTRUE(getOption("rPDBapi.strict_property_validation", FALSE))
  if (!strict) {
    return(invisible(TRUE))
  }

  validate_properties(properties = properties, data_type = data_type, strict = TRUE)
}
