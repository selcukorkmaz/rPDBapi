# Testing get_info
test_that("get_info retrieves correct PDB information", {
  skip_if_not_live_tests()
  result <- get_info("4HHB")

  expect_type(result, "list")
  expect_true("rcsb_id" %in% names(result))
  expect_equal(result$rcsb_id, "4HHB")
})
