#' Validate the Format of an PDB ID Based on Data Type
#'
#' This function checks if the given PDB ID is valid for the specified data type.
#' It issues warnings if the ID format does not match the expected format for the data type.
#'
#' @param id A string representing the ID to be validated.
#' @param data_type A string specifying the data type against which the ID is validated
#'   (e.g., 'entity', 'instance', 'ASSEMBLY').
#' @details The function checks for specific patterns in the ID based on the data type:
#'   - For data types containing 'entity' but not 'instance', IDs should contain an underscore ('_').
#'   - For data types containing 'instance', IDs should contain a dot ('.').
#'   - For 'ASSEMBLY' data type, IDs should contain a dash ('-').
#' @export
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
