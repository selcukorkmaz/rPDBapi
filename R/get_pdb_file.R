#' Download PDB Files from RCSB Database
#'
#' This function downloads PDB files from the RCSB database, supporting different file types and optional compression.
#' It warns to consider compression for CIF files for faster download.
#'
#' @param pdb_id A 4-character string specifying a PDB entry of interest.
#' @param filetype The file type: 'pdb', 'cif', 'xml', or 'structfact'. Default is 'pdb'.pdb is the older file format and cif is the newer replacement. xlm is also can be obtained and parsed. structfact retrieves structure factors (only available for certain PDB entries).
#' @param path The path where the file should be saved. If NULL, the file is saved in a temporary directory.
#' @param compression Logical indicating whether to request the data as a compressed version. Default is FALSE.
#' @return The uncompressed string representing the full PDB file or NULL if the request fails.
#' @importFrom httr GET
#' @examples
#' # Example usage:
#' # pdb_file <- get_pdb_file(pdb_id = "4HHB", filetype = "cif", compression = TRUE)
#' @export
get_pdb_file <- function(pdb_id, filetype = 'cif', compression = FALSE, save = FALSE,  path = NULL) {

  PDB_DOWNLOAD_BASE_URL <- "https://files.rcsb.org/download/"


  if (filetype == 'cif' && !compression) {
    warning("Consider using `get_pdb_file` with compression=TRUE for CIF files (it makes the file download faster!)")
  }

  pdb_url <- paste0(PDB_DOWNLOAD_BASE_URL, pdb_id)

  if (filetype == 'structfact') {
    pdb_url <- paste0(pdb_url, "-sf.cif")
  } else {
    pdb_url <- paste0(pdb_url, ".", filetype)
  }

  if (compression) {
    pdb_url <- paste0(pdb_url, ".gz")
  }

  message(paste("Sending GET request to", pdb_url, "to fetch", pdb_id, "'s", filetype, "file as a string."))

  response <- GET(pdb_url)

  if (is.null(response) || http_status(response)$category != "Success") {
    warning("Retrieval failed, returning NULL")
  }

  if(is.null(path)){

    path= paste0(tempdir(), "/", pdb_id, ".", filetype,".gz")

  }else{

    path= paste0(path, "/", pdb_id, ".", filetype,".gz")

  }

  if(save){
    download.file(response$url, path)
    message("The file saved to ", path)
  }

  if (compression) {

    result <- rawToChar(memDecompress(response$content, type = "gzip"))
    invisible(result)

  }else{

    result <- content(response, "text")
    invisible(result)


  }


}
