# Testing get_pdb_file
test_that("get_pdb_file downloads and parses the PDB file correctly", {
  result <- get_pdb_file("4HHB", filetype = "pdb", save = FALSE)

  expect_type(result, "list")
  expect_true("atom" %in% names(result))
  expect_true("xyz" %in% names(result))
})

test_that("get_pdb_file warns for unsupported filetype", {
  expect_error(get_pdb_file("4HHB", filetype = "unsupported"), "Unsupported filetype")
})
