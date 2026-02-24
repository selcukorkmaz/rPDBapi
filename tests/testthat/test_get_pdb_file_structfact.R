# Testing get_pdb_file structfact handling

test_that("get_pdb_file supports structfact and skips coordinate post-processing", {
  captured_path <- NULL

  local_mocked_bindings(
    send_api_request = function(url, ...) {
      list(url = "https://example.org/4HHB-sf.cif")
    },
    handle_api_errors = function(response, url = "") {
      invisible(NULL)
    },
    download.file = function(url, destfile, quiet = TRUE, ...) {
      writeLines("dummy", destfile)
      invisible(NULL)
    },
    read.cif = function(path) {
      captured_path <<- path
      list(structure_factors = TRUE)
    },
    .package = "rPDBapi"
  )

  result <- get_pdb_file("4HHB", filetype = "structfact", save = FALSE, verbosity = FALSE)

  expect_true(is.list(result))
  expect_true(isTRUE(result$structure_factors))
  expect_match(captured_path, "-sf\\.cif$")
})

test_that("get_pdb_file keeps CIF processing behavior unchanged", {
  captured_path <- NULL

  local_mocked_bindings(
    send_api_request = function(url, ...) {
      list(url = "https://example.org/4HHB.cif.gz")
    },
    handle_api_errors = function(response, url = "") {
      invisible(NULL)
    },
    download.file = function(url, destfile, quiet = TRUE, ...) {
      writeLines("dummy", destfile)
      invisible(NULL)
    },
    read.cif = function(path) {
      captured_path <<- path
      list(
        atom = data.frame(alt = NA_character_, insert = NA_character_, eleno = 1, stringsAsFactors = FALSE),
        xyz = c(1, 2, 3)
      )
    },
    as.xyz = function(x) x,
    atom.select.pdb = function(result, string = "calpha", verbose = FALSE) list(atom = integer(0)),
    .package = "rPDBapi"
  )

  result <- get_pdb_file("4HHB", filetype = "cif", rm.alt = FALSE, rm.insert = FALSE, save = FALSE, verbosity = FALSE)

  expect_true(is.list(result))
  expect_true("calpha" %in% names(result))
  expect_match(captured_path, "\\.cif\\.gz$")
})
