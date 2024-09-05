#' Generate a PDB API URL
#'
#' This function constructs a full PDB API URL by concatenating the base URL, an API endpoint, and an identifier.
#'
#' @param endpoint A character string representing the specific API endpoint to be accessed (e.g., "/pdb/entry/").
#' @param id A character string representing the identifier for the resource (e.g., a PDB ID or other relevant ID).
#' @param base_url A string representing the base URL to generate PDB API url. By default, this is set to the global constant \code{BASE_URL}, but users can specify a different URL if needed.
#'
#'
#' @return A character string containing the full URL for accessing the PDB API resource.
#'
#' @export
get_pdb_api_url <- function(endpoint, id, base_url = BASE_URL) {
  return(paste0(base_url, endpoint, id))
}
