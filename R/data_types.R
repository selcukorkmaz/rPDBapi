FASTA_BASE_URL <- "https://www.rcsb.org/fasta/entry/"
PDB_DOWNLOAD_BASE_URL <- "https://files.rcsb.org/download/"

DataType <- c(
  ENTRY = "entries",
  POLYMER_ENTITY = "polymer_entities",
  BRANCHED_ENTITY = "branched_entities",
  NONPOLYMER_ENTITY = "nonpolymer_entities",
  POLYMER_ENTITY_INSTANCE = "polymer_entity_instances",
  BRANCHED_ENTITY_INSTANCE = "branched_entity_instances",
  NONPOLYMER_ENTITY_INSTANCE = "nonpolymer_entity_instances",
  ASSEMBLY = "assemblies",
  CHEMICAL_COMPONENT = "chem_comps"
)


add_property <- function(property) {
  if (!is.list(property)) {
    stop("Property must be a list.")
  }

  for (key in names(property)) {
    if (!is.character(property[[key]])) {
      stop("Property values must be character vectors.")
    }
    if (!key %in% names(property)) {
      property[[key]] <- character()
    }
    property[[key]] <- unique(c(property[[key]], property[[key]]))
  }

  return(property)
}


generate_json_query <- function(ids, data_type, properties) {
  if (length(properties) == 0) {
    stop("ERROR: no properties given to generate JSON query.")
  }

  q_str <- switch(data_type,
                  ENTRY = "entry_ids",
                  POLYMER_ENTITY = "entity_ids",
                  BRANCHED_ENTITY = "entity_ids",
                  NONPOLYMER_ENTITY = "entity_ids",
                  POLYMER_ENTITY_INSTANCE = "instance_ids",
                  BRANCHED_ENTITY_INSTANCE = "instance_ids",
                  NONPOLYMER_ENTITY_INSTANCE = "instance_ids",
                  ASSEMBLY = "assembly_ids",
                  CHEMICAL_COMPONENT = "comp_ids")

  data_str <- sprintf("%s(%s: [\"%s\"])", DataType[[data_type]], q_str, paste(ids, collapse = "\", \""))

  props_str <- sapply(properties, function(x) {
    if (length(x) == 0) {
      return("")
    } else {
      return(paste0("{", paste(x, collapse = ", "), "}"))
    }
  }, USE.NAMES = FALSE)

  props_str <- paste(names(properties), props_str, collapse = ", ")

  json_query <- paste0(list(query = paste0("{", data_str, "{", props_str, "}}")))
  return(json_query)
}


fetch_data <- function(json_query, data_type, ids) {
  if (length(json_query) == 0) {
    stop("JSON query has not been created.")
  }

  response <- search_graphql(list(query = json_query))

  if ("errors" %in% names(response)) {
    message("ERROR encountered in fetch_data().")
    lapply(response$errors, function(error) message(error$message))
    return(NULL)
  }

  if (length(response$data[[1]]) != length(ids)) {
    message("WARNING: one or more IDs not found in the PDB.")
  }

  names(response$data[[1]]) <- ids

  return(response)
}


return_data_as_dataframe <- function(response, data_type, ids) {
  if (is.null(response) || length(response) == 0) {
    return(NULL)
  }

  data <- response$data[[1]]

  flattened_data <- map(data, ~flatten(.x))
  df <- map_df(flattened_data, ~as.data.frame(t(.x)), .id = "ID") %>% select_if(~ !any(is.na(.)))

  return(df)
}


validate_id <- function(id, data_type) {
  if (grepl("entity", data_type) && !grepl("instance", data_type)) {
    if (!grepl("_", id)) {
      warning(paste(id, "not valid for", data_type, "."))
    }
  } else if (grepl("instance", data_type)) {
    if (!grepl("\\.", id)) {
      warning(paste(id, "not valid for", data_type, "."))
    }
  } else if (data_type == "ASSEMBLY") {
    if (!grepl("-", id)) {
      warning(paste(id, "not valid for", data_type, "."))
    }
  }
}


DataFetcher <- function(id = NULL, data_type = "ENTRY", properties = NULL, return_as_dataframe = TRUE) {


  # # Adjust ID if necessary
  # if (!is.list(id)) {
  #   ids <- list(id)
  # }
  # lapply(id, validate_id, data_type)


  properties <- add_property(properties)
  # print(properties)
  json_query <- generate_json_query(id, data_type, properties)
  # print(json_query)
  response <- fetch_data(json_query, data_type, id)
  # print(response)

  if(return_as_dataframe){

    response <- return_data_as_dataframe(response, data_type, id)

  }

  return(response)

}

# Example Usage

### Fetch entries using PDB IDs


data_fetcher <- DataFetcher(id = c("4HHB", "12CA", "3PQR"), data_type = "ENTRY",
                            properties = list(exptl =  c("method", "details"), cell = c("length_a", "length_b", "length_c")),
                            return_as_dataframe = T)



data_fetcher

### Fetch Assemblies

data_fetcher <- DataFetcher(id = c("4HHB-1", "12CA-1", "3PQR-1"), data_type = "ASSEMBLY",
                            properties = list(rcsb_assembly_info =  c("entry_id", "assembly_id", "polymer_entity_instance_count")),
                            return_as_dataframe = T)



data_fetcher

### Fetch Polymer Entities

data_fetcher <- DataFetcher(id = c("2CPK_1","3WHM_1","2D5Z_1"), data_type = "POLYMER_ENTITY",
                            properties = list(rcsb_entity_source_organism = c("ncbi_taxonomy_id", "ncbi_scientific_name"),
                                              rcsb_cluster_membership = c("cluster_id", "identity")),
                            return_as_dataframe = T)



data_fetcher

### Fetch Polymer Entity Instance

data_fetcher <- DataFetcher(id = c("4HHB.A", "12CA.A", "3PQR.A"), data_type = "POLYMER_ENTITY_INSTANCE",
                            properties = list(rcsb_polymer_instance_annotation = c("annotation_id", "name", "type")),
                            return_as_dataframe = T)



data_fetcher


### Fetch Branched Entity

data_fetcher <- DataFetcher(id = c("5FMB_2", "6L63_3"), data_type = "BRANCHED_ENTITY",
                            properties = list(pdbx_entity_branch = "type", pdbx_entity_branch_descriptor = c("type", "descriptor")),
                            return_as_dataframe = T)



data_fetcher


### Fetch Chemical Components

data_fetcher <- DataFetcher(id = c("NAG","EBW"), data_type = "CHEMICAL_COMPONENT",
                            properties = list(chem_comp = c("type", "formula_weight","name","formula"),
                                              rcsb_chem_comp_info = c("initial_release_date")),
                            return_as_dataframe = T)



data_fetcher
