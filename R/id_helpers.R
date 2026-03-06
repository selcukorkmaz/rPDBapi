#' Infer RCSB Identifier Type
#'
#' Infers the likely identifier category for one or more RCSB-related IDs.
#' This helper is additive and does not affect existing workflows unless called explicitly.
#'
#' @param id A character vector of identifiers.
#' @return A character vector with inferred types: \code{"ENTRY"},
#'   \code{"ASSEMBLY"}, \code{"ENTITY"}, \code{"INSTANCE"},
#'   \code{"CHEMICAL_COMPONENT"}, or \code{"UNKNOWN"}.
#' @export
infer_id_type <- function(id) {
  ids <- as.character(id)

  inferred <- vapply(ids, function(x) {
    x <- trimws(x)
    if (is.na(x) || !nzchar(x)) {
      return("UNKNOWN")
    }

    if (grepl("^[A-Za-z0-9]{4}$", x)) {
      return("ENTRY")
    }
    if (grepl("^[A-Za-z0-9]{4}-[0-9]+$", x)) {
      return("ASSEMBLY")
    }
    if (grepl("^[A-Za-z0-9]{4}_[0-9]+$", x)) {
      return("ENTITY")
    }
    if (grepl("^[A-Za-z0-9]{4}([.:/])[A-Za-z0-9]+$", x)) {
      return("INSTANCE")
    }
    if (grepl("^[A-Za-z0-9]{1,3}$", x)) {
      return("CHEMICAL_COMPONENT")
    }

    "UNKNOWN"
  }, character(1))

  unname(inferred)
}

#' Parse an RCSB Identifier
#'
#' Parses an identifier into a structured record when possible.
#'
#' @param id A scalar identifier.
#' @return A named list with parsed fields and inferred type.
#' @export
parse_rcsb_id <- function(id) {
  x <- as.character(id)[1]
  x <- trimws(x)
  id_type <- infer_id_type(x)[1]

  parsed <- list(
    raw_id = x,
    normalized_id = x,
    id_type = id_type,
    entry_id = NULL,
    assembly_id = NULL,
    entity_id = NULL,
    instance_id = NULL,
    separator = NULL
  )

  if (id_type == "ENTRY") {
    parsed$entry_id <- x
    return(parsed)
  }

  if (id_type == "ASSEMBLY") {
    parts <- strsplit(x, "-", fixed = TRUE)[[1]]
    parsed$entry_id <- parts[1]
    parsed$assembly_id <- parts[2]
    parsed$separator <- "-"
    return(parsed)
  }

  if (id_type == "ENTITY") {
    parts <- strsplit(x, "_", fixed = TRUE)[[1]]
    parsed$entry_id <- parts[1]
    parsed$entity_id <- parts[2]
    parsed$separator <- "_"
    return(parsed)
  }

  if (id_type == "INSTANCE") {
    sep <- regmatches(x, regexpr("[.:/]", x))
    parts <- strsplit(x, "[.:/]")[[1]]
    parsed$entry_id <- parts[1]
    parsed$instance_id <- parts[2]
    parsed$separator <- sep
    return(parsed)
  }

  if (id_type == "CHEMICAL_COMPONENT") {
    parsed$entity_id <- x
    return(parsed)
  }

  parsed
}

#' Build an Entry Identifier
#'
#' @param entry_id Entry identifier.
#' @return Character scalar entry ID.
#' @export
build_entry_id <- function(entry_id) {
  trimws(as.character(entry_id)[1])
}

#' Build an Assembly Identifier
#'
#' @param entry_id Entry identifier.
#' @param assembly_id Assembly index or identifier.
#' @return Character scalar assembly ID formatted as \code{ENTRY-ASSEMBLY}.
#' @export
build_assembly_id <- function(entry_id, assembly_id) {
  paste0(build_entry_id(entry_id), "-", trimws(as.character(assembly_id)[1]))
}

#' Build an Entity Identifier
#'
#' @param entry_id Entry identifier.
#' @param entity_id Entity index or identifier.
#' @return Character scalar entity ID formatted as \code{ENTRY_ENTITY}.
#' @export
build_entity_id <- function(entry_id, entity_id) {
  paste0(build_entry_id(entry_id), "_", trimws(as.character(entity_id)[1]))
}

#' Build an Instance Identifier
#'
#' @param entry_id Entry identifier.
#' @param instance_id Instance/chain identifier.
#' @param separator Separator between entry and instance. Defaults to \code{"."}.
#' @return Character scalar instance ID.
#' @export
build_instance_id <- function(entry_id, instance_id, separator = ".") {
  paste0(build_entry_id(entry_id), separator, trimws(as.character(instance_id)[1]))
}

rpdbapi_prepare_ids <- function(ids, data_type = NULL) {
  prepared <- as.character(ids)
  prepared <- trimws(prepared)
  prepared
}
