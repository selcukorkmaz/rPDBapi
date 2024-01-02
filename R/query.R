library(httr)
library(jsonlite)

query <- function(search_term, query_type = "full_text", return_type = "entry", scan_params = NULL) {
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

  query_params <- list(type = "terminal", service = query_type)
  if (is.null(scan_params)) {
    if (query_type %in% c("full_text", "text")) {
      query_params$parameters <- list(value = search_term)
    } else if (query_type == "sequence") {
      query_params$parameters <- list(target = "pdb_protein_sequence", value = search_term)
    } else if (query_type == "structure") {
      query_params$parameters <- list(operator = "relaxed_shape_match", value = list(entry_id = search_term, assembly_id = "1"))
    }

    if (!is.null(query_subtype)) {
      query_params$parameters <- list(operator = "exact_match", negation = FALSE, value = search_term, attribute = get_attribute_for_subtype(query_subtype))
    }

    scan_params <- list(query = query_params, return_type = return_type, request_options = list(results_verbosity = "verbose"))
    if (return_type == "entry") {
      scan_params$request_options$return_all_hits <- TRUE
    }
  }

  list(
    search_term = search_term,
    query_type = query_type,
    return_type = return_type,
    scan_params = scan_params
    # url = "https://search.rcsb.org/rcsbsearch/v2/query?json="
  )
}

get_attribute_for_subtype <- function(subtype) {
  attributes <- list(
    pmid = "rcsb_pubmed_container_identifiers.pubmed_id",
    taxid = "rcsb_entity_source_organism.taxonomy_lineage.id",
    experiment_type = "exptl.method",
    author = "rcsb_primary_citation.rcsb_authors",
    organism = "rcsb_entity_source_organism.taxonomy_lineage.name",
    pfam = "rcsb_polymer_entity_annotation.annotation_id",
    uniprot = "rcsb_polymer_entity_container_identifiers.reference_sequence_identifiers.database_accession"
  )
  attributes[[subtype]]
}

# Example usage
# query <- query(search_term = "actin network")
# query
