#' Describe Chemical Compound from RCSB PDB
#'
#' Retrieves detailed information about a chemical compound from the RCSB Protein Data Bank (PDB) based on its chemical ID.
#'
#' @param chem_id A string representing the 3-character chemical ID. This ID is typically an alphanumeric string used to uniquely identify ligands, cofactors, or other small molecules within macromolecular structures. The string must not exceed 3 characters.
#' @param url_root A string representing the URL for retrieving information about chemical compounds. By default, this is set to the global constant \code{URL_ROOT}, but users can specify a different URL if needed.
#' @return A list containing detailed information about the chemical compound. This list includes various fields such as:
#' \describe{
#'   \item{rcsb_chem_comp_descriptor}{A sublist containing chemical descriptors like SMILES, InChI strings, molecular weight, and other chemical properties.}
#'   \item{rcsb_chem_comp_info}{Information regarding the compound’s classification, formula, and additional relevant data.}
#'   \item{other}{Other fields may also be included, depending on the specific compound and the data available from the RCSB PDB.}
#' }
#' @importFrom httr GET http_status content
#' @importFrom jsonlite fromJSON
#' @examples
#' \dontrun{
#' # Retrieve chemical information for N-Acetyl-D-Glucosamine (NAG)
#' chem_desc <- describe_chemical('NAG')
#' chem_desc
#'
#' # Access the SMILES string of the compound
#' smiles_string <- chem_desc$rcsb_chem_comp_descriptor$smiles
#' smiles_string
#'
#' }
#' @export


describe_chemical <- function(chem_id, url_root = URL_ROOT) {
  # Validate the input chemical ID
  if (nchar(chem_id) > 3) {
    stop("Input Error: The provided ligand ID '", chem_id, "' exceeds the maximum allowed length of 3 characters. Please provide a valid 3-character ligand ID.")
  }

  # Construct the URL for the API request
  url <- paste0(url_root, chem_id)

  # Send API request using the core function
  response <- tryCatch(
    {
      send_api_request(url = url)
    },
    error = function(e) {
      stop("Network Error: Failed to retrieve data for ligand ID '", chem_id, "'. Error: ", e$message)
    }
  )

  # Handle any potential HTTP errors using the core function
  tryCatch(
    {
      handle_api_errors(response, url)
    },
    error = function(e) {
      stop("API Error: Failed to retrieve data for ligand ID '", chem_id, "'. Error: ", e$message)
    }
  )

  # Parse the response using the core function
  chemical_data <- tryCatch(
    {
      parse_response(response, format = "json")
    },
    error = function(e) {
      stop("Parsing Error: Failed to parse the JSON response for ligand ID '", chem_id, "'. Error: ", e$message)
    }
  )

  # Return the parsed output
  return(chemical_data)
}

utils::globalVariables("url_root")

