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

# Example Usage
properties <- add_property(list(cell = c("volume", "angle_beta"), exptl = "method"))
print(properties)


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

  json_query <- toJSON(list(query = paste0("{", data_str, "{", props_str, "}}")), auto_unbox = T, pretty = T)
  return(json_query)
}

# Example Usage
ids <- c("4LZA", "5RU3")
data_type <- "ENTRY"
properties <- list(cell = c("volume", "angle_beta"), exptl = "method")
json_query <- generate_json_query(ids, data_type, properties)
print(json_query)


fetch_data <- function(json_query, data_type, ids) {
  if (length(json_query) == 0) {
    stop("JSON query has not been created.")
  }

  url <- "https://data.rcsb.org/graphql"
  response <- httr::POST(url, body = json_query, encode = "json") #|> httr::content("parsed")

  if ("errors" %in% names(response)) {
    message("ERROR encountered in fetch_data().")
    lapply(response$errors, function(error) message(error$message))
    return(NULL)
  }

  if (length(response$data[[data_type]]) != length(ids)) {
    message("WARNING: one or more IDs not found in the PDB.")
  }

  return(response$data[[data_type]])
}

# Example Usage
data_type <- "ENTRY"
ids <- c("4LZA", "5RU3")
response <- fetch_data(json_query, data_type, ids)
print(response)


return_data_as_dataframe <- function(response, data_type, ids) {
  if (is.null(response) || length(response) == 0) {
    return(NULL)
  }

  data <- response[[data_type]]

  # Flattening the data dictionary
  data_flat <- lapply(data, function(entry) {
    curr_dict <- list()
    for (key in names(entry)) {
      v <- ifelse(is.list(entry[[key]]), entry[[key]][[1]], entry[[key]])
      if (is.character(v)) {
        curr_dict[[key]] <- v
      } else if (is.list(v)) {
        for (subprop in names(v)) {
          curr_dict[[paste(key, subprop, sep = ".")]] <- v[[subprop]]
        }
      }
    }
    curr_dict
  })

  names(data_flat) <- ids
  data_frame <- do.call(rbind, lapply(data_flat, as.data.frame, stringsAsFactors = FALSE))
  return(data_frame)
}

# Example Usage
response <- list(...) # Assume response is already fetched
data_type <- "ENTRY"
ids <- c("4LZA", "5RU3")
df <- return_data_as_dataframe(response, data_type, ids)
print(df)

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


DataFetcher <- function(id, data_type, properties = NULL, json_query = NULL, response = NULL) {
  # properties <- list()
  # json_query <- list()
  # response <- list()

  # Helper function to validate ID

  # Adjust ID if necessary
  if (!is.list(id)) {
    id <- list(id)
  }
  lapply(id, validate_id, data_type)


  list(
    id = id,
    data_type = data_type,
    properties = properties,
    json_query = json_query,
    response = response
    # validate_id = validate_id,
    # add_property = add_property,
    # generate_json_query = generate_json_query
  )
}

# Example Usage
data_fetcher <- DataFetcher(id = c("4HHB", "12CA", "3PQR"), data_type = "ENTRY",
                            properties = list(exptl =  c("method", "details"), cell = c("length_a", "length_b", "length_c")))
data_fetcher$generate_json_query()
print(data_fetcher$json_query)


