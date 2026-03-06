# Narrow, additive analysis helpers.

rpdbapi_safe_numeric <- function(x) {
  suppressWarnings(as.numeric(x))
}

rpdbapi_safe_min <- function(x) {
  x <- x[is.finite(x)]
  if (length(x) == 0) {
    return(NA_real_)
  }
  min(x)
}

rpdbapi_get_df <- function(x) {
  if (inherits(x, "rPDBapi_object")) {
    return(rpdbapi_to_tibble(x$data))
  }

  if (inherits(x, "data.frame")) {
    return(dplyr::as_tibble(x))
  }

  rpdbapi_to_tibble(x)
}

#' Summarize Entry-Level Data
#'
#' @param x Entry-like table or object.
#' @return One-row tibble with high-level summary fields.
#' @export
summarize_entries <- function(x) {
  df <- rpdbapi_get_df(x)

  has_res <- "resolution_combined" %in% names(df)
  has_mw <- "molecular_weight" %in% names(df)
  has_method <- "method" %in% names(df)

  res_vals <- if (has_res) rpdbapi_safe_numeric(df$resolution_combined) else numeric(0)
  mw_vals <- if (has_mw) rpdbapi_safe_numeric(df$molecular_weight) else numeric(0)

  dplyr::tibble(
    n_entries = nrow(df),
    n_methods = if (has_method) dplyr::n_distinct(df$method) else NA_integer_,
    best_resolution = rpdbapi_safe_min(res_vals),
    median_molecular_weight = if (length(mw_vals[is.finite(mw_vals)]) > 0) stats::median(mw_vals, na.rm = TRUE) else NA_real_
  )
}

#' Summarize Assembly-Level Data
#'
#' @param x Assembly-like table or object.
#' @return One-row tibble with assembly summary fields.
#' @export
summarize_assemblies <- function(x) {
  df <- rpdbapi_get_df(x)
  oligomeric <- if ("oligomeric_count" %in% names(df)) rpdbapi_safe_numeric(df$oligomeric_count) else numeric(0)

  dplyr::tibble(
    n_assemblies = nrow(df),
    median_oligomeric_count = if (length(oligomeric[is.finite(oligomeric)]) > 0) stats::median(oligomeric, na.rm = TRUE) else NA_real_,
    n_symmetry_labels = if ("symbol" %in% names(df)) dplyr::n_distinct(df$symbol) else NA_integer_
  )
}

#' Extract Taxonomy Table
#'
#' @param x Polymer-entity-like table or object.
#' @return Tibble with taxonomy columns when available.
#' @export
extract_taxonomy_table <- function(x) {
  df <- rpdbapi_get_df(x)
  keep <- intersect(c("rcsb_id", "ncbi_taxonomy_id", "ncbi_scientific_name"), names(df))
  if (length(keep) == 0) {
    return(dplyr::tibble())
  }
  dplyr::distinct(dplyr::select(df, dplyr::all_of(keep)))
}

#' Extract Ligand Table
#'
#' @param x Chemical-component-like table or object.
#' @return Tibble with ligand descriptor columns when available.
#' @export
extract_ligand_table <- function(x) {
  df <- rpdbapi_get_df(x)
  keep <- intersect(c("rcsb_id", "id", "name", "formula", "formula_weight", "type", "smiles"), names(df))
  if (length(keep) == 0) {
    return(dplyr::tibble())
  }
  dplyr::distinct(dplyr::select(df, dplyr::all_of(keep)))
}

#' Extract C-alpha Coordinates
#'
#' @param structure Structure object returned by \code{get_pdb_file()} for CIF/PDB.
#' @return Tibble with chain, residue, and coordinate columns for C-alpha atoms.
#' @export
extract_calpha_coordinates <- function(structure) {
  if (is.null(structure$atom) || is.null(structure$xyz) || is.null(structure$calpha)) {
    rpdbapi_abort(
      "Input structure does not contain atom/xyz/calpha fields.",
      class = "rPDBapi_error_invalid_input",
      function_name = "extract_calpha_coordinates"
    )
  }

  coords <- matrix(as.numeric(structure$xyz), ncol = 3, byrow = TRUE)
  idx <- which(isTRUE(structure$calpha) | structure$calpha)

  dplyr::tibble(
    chain = as.character(structure$atom$chain[idx]),
    resno = as.integer(structure$atom$resno[idx]),
    resid = as.character(structure$atom$resid[idx]),
    x = coords[idx, 1],
    y = coords[idx, 2],
    z = coords[idx, 3]
  )
}

rpdbapi_parse_sequence_name <- function(name) {
  if (is.null(name) || !nzchar(name)) {
    return(NA_character_)
  }

  m <- regexec("chain\\s*([A-Za-z0-9])", name, ignore.case = TRUE)
  hit <- regmatches(name, m)[[1]]
  if (length(hit) >= 2) {
    return(hit[2])
  }

  pieces <- strsplit(name, "\\|", fixed = FALSE)[[1]]
  one_char <- pieces[nchar(pieces) == 1]
  if (length(one_char) > 0) {
    return(one_char[1])
  }

  NA_character_
}

#' Join Structure and Sequence Summaries
#'
#' @param structure Structure object returned by \code{get_pdb_file()}.
#' @param sequences FASTA sequence list (typically from \code{get_fasta_from_rcsb_entry()}).
#' @return Tibble joining available chain-level C-alpha counts and sequence lengths.
#' @export
join_structure_sequence <- function(structure, sequences) {
  if (!is.list(sequences) || length(sequences) == 0) {
    rpdbapi_abort(
      "Sequences must be a non-empty list.",
      class = "rPDBapi_error_invalid_input",
      function_name = "join_structure_sequence"
    )
  }

  ca_tbl <- extract_calpha_coordinates(structure)
  ca_summary <- dplyr::summarise(
    dplyr::group_by(ca_tbl, chain),
    n_calpha = dplyr::n(),
    .groups = "drop"
  )

  seq_tbl <- dplyr::tibble(
    sequence_header = names(sequences),
    sequence = as.character(unlist(sequences, use.names = FALSE))
  )
  seq_tbl <- dplyr::mutate(
    seq_tbl,
    chain = vapply(sequence_header, rpdbapi_parse_sequence_name, character(1)),
    sequence_length = nchar(sequence)
  )

  dplyr::left_join(seq_tbl, ca_summary, by = "chain")
}
