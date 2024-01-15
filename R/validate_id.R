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
