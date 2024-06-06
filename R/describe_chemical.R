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
#' chem_desc$rcsb_chem_comp_descriptor$smiles
#' @export

describe_chemical <- function(chem_id) {
  if (nchar(chem_id) > 3) {
    stop("Ligand id with more than 3 characters provided")
  }

  url_root <- 'https://data.rcsb.org/rest/v1/core/chemcomp/'
  url <- paste0(url_root, chem_id)

  response <- tryCatch(
    {
      GET(url)
    },
    error = function(e) {
      warning("Failed to retrieve data: ", e$message)
      return(NULL)
    }
  )

  if (is.null(response)) {
    return(NULL)
  }

  if (http_status(response)$category != "Success") {
    stop("Retrieval failed: ", http_status(response)$reason)
  }

  out <- tryCatch(
    {
      fromJSON(content(response, "text", encoding = "UTF-8"))
    },
    error = function(e) {
      warning("Failed to parse JSON response: ", e$message)
      return(NULL)
    }
  )

  return(out)
}


