

# Testing get_fasta_from_rcsb_entry
test_that("get_fasta_from_rcsb_entry returns correct FASTA sequences", {
  result <- get_fasta_from_rcsb_entry("4HHB")

  expect_type(result, "list")
  expect_true(length(result) > 0)
  expect_true(any(grepl("VLSPADKTNVKAAWGKV", unlist(result))))
})

test_that("get_fasta_from_rcsb_entry returns correct chain sequence", {
  result <- get_fasta_from_rcsb_entry("4HHB", chain_id = "A")

  expect_type(result, "character")
  expect_true(grepl("VLSPADKTNVKAAWGKV", result))
})
