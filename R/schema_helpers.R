#' List Known RCSB Fields by Data Type
#'
#' Returns the package's built-in field registry used for schema-aware property
#' validation. This registry is intentionally conservative and additive.
#'
#' @param data_type Optional data type filter (e.g., \code{"ENTRY"}). If NULL,
#'   all supported data types are returned.
#' @return A data frame with columns \code{data_type}, \code{field}, and
#'   \code{subfield}.
#' @export
list_rcsb_fields <- function(data_type = NULL) {
  schema <- rpdbapi_schema_registry()

  if (!is.null(data_type)) {
    rpdbapi_validate_data_type(data_type, function_name = "list_rcsb_fields")
    schema <- schema[data_type]
  }

  rows <- lapply(names(schema), function(dt) {
    fields <- schema[[dt]]
    do.call(
      rbind,
      lapply(names(fields), function(field_name) {
        subfields <- fields[[field_name]]
        if (length(subfields) == 0) {
          data.frame(
            data_type = dt,
            field = field_name,
            subfield = NA_character_,
            stringsAsFactors = FALSE
          )
        } else {
          data.frame(
            data_type = dt,
            field = field_name,
            subfield = subfields,
            stringsAsFactors = FALSE
          )
        }
      })
    )
  })

  do.call(rbind, rows)
}

#' Search Known RCSB Fields by Pattern
#'
#' Searches the built-in field registry by field or subfield name.
#'
#' @param pattern Pattern to search for.
#' @param data_type Optional data type filter.
#' @param ignore_case Logical, passed to \code{grepl()}.
#' @return A filtered data frame from \code{\link{list_rcsb_fields}}.
#' @export
search_rcsb_fields <- function(pattern, data_type = NULL, ignore_case = TRUE) {
  fields <- list_rcsb_fields(data_type = data_type)
  keep <- grepl(pattern, fields$field, ignore.case = ignore_case) |
    grepl(pattern, fields$subfield, ignore.case = ignore_case)
  fields[keep, , drop = FALSE]
}

#' Validate Property Specification Against Known Field Registry
#'
#' Validates a property list for a given data type.
#'
#' @param properties Property list as used in \code{data_fetcher()} and
#'   \code{generate_json_query()}.
#' @param data_type RCSB data type.
#' @param strict Logical. If TRUE, unknown fields/subfields raise errors.
#'   If FALSE, validation details are returned without error.
#' @return Invisibly TRUE when valid in strict mode. In non-strict mode,
#'   a list with unknown fields/subfields.
#' @export
validate_properties <- function(properties, data_type, strict = TRUE) {
  details <- rpdbapi_property_validation_details(properties = properties, data_type = data_type)

  if (isTRUE(strict)) {
    if (length(details$unknown_fields) > 0) {
      rpdbapi_abort(
        paste0(
          "Unknown properties for data_type '", data_type, "': ",
          paste(details$unknown_fields, collapse = ", ")
        ),
        class = "rPDBapi_error_invalid_input",
        function_name = "validate_properties",
        data_type = data_type
      )
    }

    if (length(details$unknown_subfields) > 0) {
      bad <- vapply(
        names(details$unknown_subfields),
        function(fname) {
          paste0(fname, " [", paste(details$unknown_subfields[[fname]], collapse = ", "), "]")
        },
        character(1)
      )
      rpdbapi_abort(
        paste0(
          "Unknown subproperties for data_type '", data_type, "': ",
          paste(bad, collapse = "; ")
        ),
        class = "rPDBapi_error_invalid_input",
        function_name = "validate_properties",
        data_type = data_type
      )
    }

    return(invisible(TRUE))
  }

  details
}
