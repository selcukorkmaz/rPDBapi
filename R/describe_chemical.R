#' Describe Chemical Compound from RCSB PDB
#'
#' This function retrieves the description of a chemical compound from the RCSB PDB based on its ID.
#'
#' @param chem_id A string representing the 3-character chemical ID.
#' @return A dictionary containing the chemical description.
#' @importFrom httr GET http_status
#' @importFrom jsonlite fromJSON
#' @examples
#' chem_desc <- describe_chemical('NAG')
#' print(chem_desc$rcsb_chem_comp_descriptor$smiles)
#' @export

describe_chemical <- function(chem_id) {
  if (nchar(chem_id) > 3) {
    stop("Ligand id with more than 3 characters provided")
  }

  url_root <- 'https://data.rcsb.org/rest/v1/core/chemcomp/'
  url <- paste0(url_root, chem_id)
  response <- GET(url)

  if (http_status(response)$category != "Success") {
    stop("Retrieval failed: ", http_status(response)$reason)
  }

  out <- fromJSON(content(response, "text", encoding = "UTF-8"))
  return(out)
}

