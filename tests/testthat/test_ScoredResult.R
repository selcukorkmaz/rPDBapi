# Testing ScoredResult
test_that("ScoredResult returns correct structure", {
  entity_id <- "4HHB"
  score <- 95.5
  result <- ScoredResult(entity_id, score)
  expect_equal(result$entity_id, entity_id)
  expect_equal(result$score, score)
})
