#' Download and Process PDB Files from the RCSB Database
#'
#' The `get_pdb_file` function is a versatile tool designed to download Protein Data Bank (PDB) files from the RCSB database.
#' It supports various file formats such as 'pdb', 'cif', 'xml', and 'structfact', with options for file compression and handling
#' alternate locations (ALT) and insertion codes (INSERT) in PDB files. This function also provides the flexibility to save
#' the downloaded files to a specified directory or to a temporary directory for immediate use.
#'
#' @param pdb_id A 4-character string specifying the PDB entry of interest (e.g., "1XYZ"). This identifier uniquely represents
#'   a macromolecular structure within the PDB database.
#' @param filetype A string specifying the format of the file to be downloaded. The default is 'cif'. Supported file types include:
#'   \describe{
#'     \item{'pdb'}{The older PDB file format, which provides atomic coordinates and metadata.}
#'     \item{'cif'}{The Crystallographic Information File (CIF) format, which is a newer standard replacing PDB files.}
#'     \item{'xml'}{An XML format file, providing structured data that can be easily parsed for various applications.}
#'     \item{'structfact'}{Structure factor files in CIF format, available for certain PDB entries, containing experimental
#'     data used to determine the structure.}
#'   }
#' @param rm.insert Logical flag indicating whether to ignore PDB insertion codes. Default is FALSE. If TRUE, records with insertion
#'   codes will be removed from the final data.
#' @param rm.alt Logical flag indicating whether to ignore alternate location indicators (ALT) in PDB files. Default is TRUE. If
#'   TRUE, only the first alternate location is kept, and others are removed.
#' @param compression Logical flag indicating whether to download the file in a compressed format (e.g., .gz). Default is TRUE,
#'   which is recommended for faster downloads, especially for CIF files.
#' @param save Logical flag indicating whether to save the downloaded file to a specified directory. Default is FALSE, which means
#'   the file is processed and optionally saved, but not retained after processing unless specified.
#' @param path A string specifying the directory where the downloaded file should be saved. If NULL, the file is saved in a
#'   temporary directory. If `save` is TRUE, this path is required.
#' @param verbosity A boolean flag indicating whether to print status messages during the function execution.
#' @param download_base_url A string representing the base URL for the PDB file retrieval. By default, this is set to the global constant \code{DOWNLOAD_BASE_URL}, but users can specify a different URL if needed.
#'
#' @return A list of class \code{"pdb"} containing the following components:
#'   \describe{
#'     \item{\code{atom}}{A data frame containing atomic coordinate data (ATOM and HETATM records). Each row corresponds to an
#'     atom, and each column to a specific record type (e.g., element, residue, chain).}
#'     \item{\code{xyz}}{A numeric matrix of class \code{"xyz"} containing the atomic coordinates from the ATOM and HETATM records.}
#'     \item{\code{calpha}}{A logical vector indicating whether each atom is a C-alpha atom (TRUE) or not (FALSE).}
#'     \item{\code{call}}{The matched call, storing the function call for reference.}
#'     \item{\code{path}}{The file path where the file was saved, if `save` was TRUE.}
#'   }
#'   The function handles errors and warnings for various edge cases, such as unsupported file types, failed downloads, or issues
#'   with reading the file.
#' @importFrom httr GET http_status
#' @importFrom utils download.file
#' @importFrom stats na.omit
#' @importFrom xml2 read_xml as_list
#' @importFrom bio3d read.pdb read.cif as.xyz trim.xyz atom2xyz atom.select.pdb
#' @examples
#' \donttest{
#'   # Download a CIF file and process it without saving
#'   pdb_file <- get_pdb_file(pdb_id = "4HHB", filetype = "cif")
#'
#'   # Download a PDB file, save it, and remove alternate location records
#'   pdb_file <- get_pdb_file(pdb_id = "4HHB", filetype = "pdb", save = TRUE, path = tempdir())
#'
#' }
#' @details
#' The `get_pdb_file` function is an essential tool for structural biologists and bioinformaticians who need to download and
#' process PDB files for further analysis. By providing options to handle alternate locations and insertion codes, this function
#' ensures that the data is clean and ready for downstream applications. Additionally, the ability to save files locally or
#' work with them in a temporary directory provides flexibility for various workflows. Error handling and informative messages
#' are included to guide the user in case of issues with file retrieval or processing.
#' @export

get_pdb_file <- function(pdb_id, filetype = 'cif', rm.insert = FALSE, rm.alt = TRUE, compression = TRUE,
                         save = FALSE, path = NULL, verbosity = TRUE, download_base_url = DOWNLOAD_BASE_URL) {

  cl <- match.call()

  # Validate the input filetype
  if (!filetype %in% c('cif', 'pdb', 'xml', 'structfact')) {
    stop("Unsupported filetype: '", filetype, "'. Please use one of the following: 'cif', 'pdb', 'xml', 'structfact'.")
  }

  if (filetype == 'cif' && !compression) {
    warning("Consider using `get_pdb_file` with compression=TRUE for CIF files (it makes the file download faster!)")
  }

  # Construct the PDB URL based on input parameters
  pdb_url <- if (filetype == 'structfact') {
    paste0(download_base_url, pdb_id, "-sf.cif")
  } else {
    paste0(download_base_url, pdb_id, ".", filetype, ifelse(compression, ".gz", ""))
  }

  if (verbosity) {
    message(paste("Sending GET request to", pdb_url, "to fetch", pdb_id, "'s", filetype, "file."))
  }

  # Send API request using the core function
  response <- tryCatch(
    {
      send_api_request(url = pdb_url)
    },
    error = function(e) {
      stop("Network Error: Failed to retrieve data for PDB ID '", pdb_id, "'. Error: ", e$message)
    }
  )

  # Handle any potential HTTP errors using the core function
  tryCatch(
    {
      handle_api_errors(response, pdb_url)
    },
    error = function(e) {
      stop("API Error: Failed to retrieve data for PDB ID '", pdb_id, "'. Error: ", e$message)
    }
  )

  # Set the download path
  if (is.null(path)) {
    path <- file.path(tempdir(), paste0(pdb_id, ".", filetype, ifelse(compression, ".gz", "")))
  } else {
    if (!dir.exists(path)) {
      stop("Specified path does not exist: ", path)
    }
    path <- file.path(path, paste0(pdb_id, ".", filetype, ifelse(compression, ".gz", "")))
  }

  # Save the downloaded file
  tryCatch(
    {
      download.file(response$url, path, quiet = TRUE)
    },
    error = function(e) {
      stop("Failed to download the PDB file: ", e$message)
    }
  )

  # Attempt to read the file
  result <- tryCatch(
    {
      if (filetype == 'pdb') {
        read.pdb(path)
      } else if (filetype == 'cif') {
        read.cif(path)
      } else if (filetype == 'xml') {
        xml_file <- read_xml(path)
        as_list(xml_file)
      } else {
        stop("Unsupported filetype encountered during file reading: '", filetype, "'.")
      }
    },
    error = function(e) {
      stop("Failed to read the downloaded PDB file: ", e$message)
    }
  )

  if (!save) {
    file.remove(path)
  } else {
    message("The file has been saved to ", path)
  }

  if (is.null(result)) {
    return(NULL)
  }

  # Process the result if not XML
  if (filetype != "xml") {
    if (!is.null(result$error)) {
      stop("Error encountered in reading the file: ", path)
    } else {
      class(result) <- c("pdb")
    }

    result$xyz <- as.xyz(result$xyz)
    result$atom[result$atom == ""] <- NA
    result$atom[result$atom == "?"] <- NA
    result$atom[result$atom == "."] <- NA
    result$models <- NULL

    if (rm.alt) {
      if (sum(!is.na(result$atom$alt)) > 0) {
        first.alt <- sort(unique(na.omit(result$atom$alt)))[1]
        warning("PDB has ALT records; taking ", first.alt, " only, rm.alt=TRUE")
        alt.inds <- which(result$atom$alt != first.alt)
        if (length(alt.inds) > 0) {
          result$atom <- result$atom[-alt.inds, ]
          result$xyz <- trim.xyz(result$xyz, col.inds = -atom2xyz(alt.inds))
        }
      }
    }

    if (rm.insert) {
      if (sum(!is.na(result$atom$insert)) > 0) {
        warning("PDB has INSERT records; removing them, rm.insert=TRUE")
        insert.inds <- which(!is.na(result$atom$insert))
        result$atom <- result$atom[-insert.inds, ]
        result$xyz <- trim.xyz(result$xyz, col.inds = -atom2xyz(insert.inds))
      }
    }

    if (any(duplicated(result$atom$eleno))) {
      warning("Duplicated element numbers ('eleno') detected")
    }

    ca.inds <- atom.select.pdb(result, string = "calpha", verbose = FALSE)
    result$calpha <- seq(1, nrow(result$atom)) %in% ca.inds$atom
    result$call <- cl
    result$path <- path
  }

  return(result)
}
