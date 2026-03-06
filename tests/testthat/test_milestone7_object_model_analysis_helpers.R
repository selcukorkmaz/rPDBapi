# Milestone 7: rich object model and narrowly scoped analysis helpers.

test_that("rPDBapi object constructors preserve class and metadata", {
  entry_df <- data.frame(
    rcsb_id = "4HHB",
    method = "X-RAY DIFFRACTION",
    resolution_combined = "1.74",
    molecular_weight = "64500",
    stringsAsFactors = FALSE
  )

  obj <- as_rpdb_entry(entry_df, metadata = list(source = "unit-test"))

  expect_s3_class(obj, "rPDBapi_entry")
  expect_s3_class(obj, "rPDBapi_object")
  expect_identical(obj$metadata$source, "unit-test")
})

test_that("print and as_tibble methods work for rPDBapi objects", {
  obj <- as_rpdb_assembly(data.frame(rcsb_id = "4HHB-1", stringsAsFactors = FALSE))

  printed <- capture.output(print(obj))
  expect_true(any(grepl("<rPDBapi_assembly>", printed, fixed = TRUE)))

  tbl <- dplyr::as_tibble(obj)
  expect_s3_class(tbl, "tbl_df")
  expect_identical(tbl$rcsb_id, "4HHB-1")
})

test_that("summarize helpers compute robust entry and assembly summaries", {
  entry_df <- data.frame(
    rcsb_id = c("4HHB", "1CRN"),
    method = c("X-RAY DIFFRACTION", "SOLUTION NMR"),
    resolution_combined = c("1.74", NA),
    molecular_weight = c("64500", "5000"),
    stringsAsFactors = FALSE
  )

  entry_summary <- summarize_entries(as_rpdb_entry(entry_df))
  expect_equal(entry_summary$n_entries, 2L)
  expect_equal(entry_summary$n_methods, 2L)
  expect_equal(entry_summary$best_resolution, 1.74)
  expect_equal(entry_summary$median_molecular_weight, 34750)

  assembly_df <- data.frame(
    assembly_id = c("4HHB-1", "1CRN-1", "2PTC-1"),
    oligomeric_count = c("2", "4", "x"),
    symbol = c("C2", "C2", "P1"),
    stringsAsFactors = FALSE
  )

  assembly_summary <- summarize_assemblies(assembly_df)
  expect_equal(assembly_summary$n_assemblies, 3L)
  expect_equal(assembly_summary$median_oligomeric_count, 3)
  expect_equal(assembly_summary$n_symmetry_labels, 2L)
})

test_that("table extraction helpers keep relevant unique columns", {
  taxonomy_df <- data.frame(
    rcsb_id = c("4HHB_1", "4HHB_1", "1CRN_1"),
    ncbi_taxonomy_id = c("9606", "9606", "32630"),
    ncbi_scientific_name = c("Homo sapiens", "Homo sapiens", "Crambe hispanica"),
    extra = c("x", "x", "y"),
    stringsAsFactors = FALSE
  )

  tax_tbl <- extract_taxonomy_table(taxonomy_df)
  expect_true(all(c("rcsb_id", "ncbi_taxonomy_id", "ncbi_scientific_name") %in% names(tax_tbl)))
  expect_equal(nrow(tax_tbl), 2L)

  ligand_df <- data.frame(
    rcsb_id = c("ATP", "ATP", "HEM"),
    id = c("ATP", "ATP", "HEM"),
    name = c("ATP", "ATP", "HEME"),
    formula = c("C10H16N5O13P3", "C10H16N5O13P3", "C34H32FeN4O4"),
    formula_weight = c("507.18", "507.18", "616.49"),
    type = c("NON-POLYMER", "NON-POLYMER", "NON-POLYMER"),
    smiles = c("...", "...", "..."),
    stringsAsFactors = FALSE
  )

  lig_tbl <- extract_ligand_table(as_rpdb_chemical_component(ligand_df))
  expect_true(all(c("rcsb_id", "id", "name", "formula", "formula_weight", "type", "smiles") %in% names(lig_tbl)))
  expect_equal(nrow(lig_tbl), 2L)
})

test_that("extract_calpha_coordinates returns atom-level coordinate tibble", {
  structure <- list(
    atom = data.frame(
      chain = c("A", "A", "B"),
      resno = c(1L, 2L, 3L),
      resid = c("GLY", "ALA", "LYS"),
      stringsAsFactors = FALSE
    ),
    xyz = c(1, 2, 3, 4, 5, 6, 7, 8, 9),
    calpha = c(TRUE, FALSE, TRUE)
  )

  coords <- extract_calpha_coordinates(structure)
  expect_equal(nrow(coords), 2L)
  expect_identical(coords$chain, c("A", "B"))
  expect_equal(coords$x, c(1, 7))
  expect_equal(coords$y, c(2, 8))
  expect_equal(coords$z, c(3, 9))

  expect_error(
    extract_calpha_coordinates(list()),
    class = "rPDBapi_error_invalid_input"
  )
})

test_that("join_structure_sequence combines chain-level sequence and coordinate summaries", {
  structure <- list(
    atom = data.frame(
      chain = c("A", "A", "B"),
      resno = c(1L, 2L, 3L),
      resid = c("GLY", "ALA", "LYS"),
      stringsAsFactors = FALSE
    ),
    xyz = c(1, 2, 3, 4, 5, 6, 7, 8, 9),
    calpha = c(TRUE, FALSE, TRUE)
  )

  sequences <- list("MGLSD", "VLSPA")
  names(sequences) <- c("4HHB chain A", "4HHB chain B")

  joined <- join_structure_sequence(structure, sequences)
  expect_equal(nrow(joined), 2L)
  expect_true(all(c("sequence_header", "sequence", "chain", "sequence_length", "n_calpha") %in% names(joined)))
  expect_identical(unname(joined$chain), c("A", "B"))
  expect_equal(joined$sequence_length, c(5L, 5L))
  expect_equal(joined$n_calpha, c(1L, 1L))

  expect_error(
    join_structure_sequence(structure, list()),
    class = "rPDBapi_error_invalid_input"
  )
})
