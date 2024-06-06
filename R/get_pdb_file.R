#' Download PDB Files from RCSB Database
#'
#' This function downloads PDB files from the RCSB database, supporting different file types and optional compression.
#' It warns to consider compression for CIF files for faster download.
#'
#' @param pdb_id A 4-character string specifying a PDB entry of interest.
#' @param filetype The file type: 'pdb', 'cif', 'xml', or 'structfact'. Default is 'pdb'.pdb is the older file format and cif is the newer replacement. xlm is also can be obtained and parsed. structfact retrieves structure factors (only available for certain PDB entries).
#' @param rm.insert Logical, if TRUE PDB insert records are ignored.
#' @param rm.alt Logical, if TRUE PDB alternate records are ignored.
#' @param compression Logical indicating whether to request the data as a compressed version. Default is TRUE
#' @param save Logical, if TRUE saves PDB file to the desired path.
#' @param path The path where the file should be saved. If NULL, the file is saved in a temporary directory.
#' @return  Returns a list of class \code{"pdb"} with the following components:
#' \item{atom}{ a data.frame containing all atomic coordinate ATOM and
#' HETATM data, with a row per ATOM/HETATM and a column per record type.}
#' \item{xyz }{ a numeric matrix of class \code{"xyz"} containing the
#' ATOM and HETATM coordinate data. }
#' \item{calpha }{ logical vector with length equal to \code{nrow(atom)} with TRUE
#' values indicating a C-alpha \dQuote{elety}. }
#' \item{call }{ the matched call. }
#' @importFrom httr GET http_status
#' @importFrom utils download.file
#' @importFrom stats na.omit
#' @importFrom xml2 read_xml as_list
#' @importFrom bio3d read.pdb read.cif as.xyz trim.xyz atom2xyz atom.select.pdb
#' @examples
#' pdb_file <-  get_pdb_file(pdb_id = "4HHB", filetype = "cif")
#' @export

get_pdb_file <- function(pdb_id, filetype = 'cif', rm.insert = FALSE, rm.alt = TRUE, compression = TRUE, save = FALSE, path = NULL) {
  PDB_DOWNLOAD_BASE_URL <- "https://files.rcsb.org/download/"
  cl <- match.call()

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

  response <- tryCatch(
    {
      GET(pdb_url)
    },
    error = function(e) {
      warning("Failed to retrieve data: ", e$message)
      return(NULL)
    }
  )

  if (is.null(response) || http_status(response)$category != "Success") {
    warning("Retrieval failed, returning NULL")
    return(NULL)
  }

  if (is.null(path)) {
    if (compression) {
      path <- paste0(tempdir(), "/", pdb_id, ".", filetype, ".gz")
    } else {
      path <- paste0(tempdir(), "/", pdb_id, ".", filetype)
    }
  } else {
    if (compression) {
      path <- paste0(path, "/", pdb_id, ".", filetype, ".gz")
    } else {
      path <- paste0(path, "/", pdb_id, ".", filetype)
    }
  }

  download.file(response$url, path, quiet = TRUE)

  result <- tryCatch(
    {
      if (filetype == 'pdb') {
        read.pdb(path)
      } else if (filetype == 'cif') {
        read.cif(path)
      } else if (filetype == 'xml') {
        xml_file <- read_xml(path)
        as_list(xml_file)
      }
    },
    error = function(e) {
      warning("Failed to read file: ", e$message)
      return(NULL)
    }
  )

  if (!save) {
    file.remove(path)
  } else {
    message("The file saved to ", path)
  }

  if (is.null(result)) {
    return(NULL)
  }

  if (filetype != "xml") {
    if (!is.null(result$error)) {
      stop(paste("Error in reading file", path))
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
        warning("PDB has ALT records, taking ", first.alt, " only, rm.alt=TRUE")
        alt.inds <- which(result$atom$alt != first.alt)
        if (length(alt.inds) > 0) {
          result$atom <- result$atom[-alt.inds, ]
          result$xyz <- trim.xyz(result$xyz, col.inds = -atom2xyz(alt.inds))
        }
      }
    }

    if (rm.insert) {
      if (sum(!is.na(result$atom$insert)) > 0) {
        warning("PDB has INSERT records, removing, rm.insert=TRUE")
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
  }

  return(result)
}
