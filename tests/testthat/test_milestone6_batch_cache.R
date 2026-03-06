# Milestone 6: caching, provenance, batch retrieval.

test_that("cache_info and clear_rpdbapi_cache manage cache files", {
  cache_dir <- file.path(tempdir(), "rpdbapi-cache-test")
  dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE)
  on.exit(unlink(cache_dir, recursive = TRUE, force = TRUE), add = TRUE)

  # Start empty.
  info0 <- cache_info(cache_dir = cache_dir)
  expect_equal(info0$total_entries, 0L)

  # Create cache-like files.
  saveRDS(list(a = 1), file.path(cache_dir, "cache-a.rds"))
  saveRDS(list(b = 2), file.path(cache_dir, "cache-b.rds"))

  info1 <- cache_info(cache_dir = cache_dir)
  expect_equal(info1$total_entries, 2L)
  expect_true(info1$total_size_bytes > 0)

  cleared <- clear_rpdbapi_cache(cache_dir = cache_dir)
  expect_true(cleared$removed >= 1L)

  info2 <- cache_info(cache_dir = cache_dir)
  expect_equal(info2$total_entries, 0L)
})

test_that("data_fetcher_batch splits IDs and combines dataframe outputs", {
  seen_batches <- list()

  local_mocked_bindings(
    data_fetcher = function(id, data_type = "ENTRY", properties = NULL, return_as_dataframe = TRUE, verbosity = FALSE) {
      seen_batches[[length(seen_batches) + 1]] <<- id
      data.frame(rcsb_id = id, stringsAsFactors = FALSE)
    },
    .package = "rPDBapi"
  )

  res <- data_fetcher_batch(
    id = c(" 4HHB ", "1CRN", "2PTC"),
    data_type = "ENTRY",
    properties = list(rcsb_id = list()),
    batch_size = 2,
    return_as_dataframe = TRUE,
    retry_attempts = 1,
    cache = FALSE,
    progress = FALSE
  )

  expect_s3_class(res, "rPDBapi_dataframe")
  expect_equal(res$rcsb_id, c("4HHB", "1CRN", "2PTC"))
  expect_equal(length(seen_batches), 2L)
  expect_equal(unlist(seen_batches, use.names = FALSE), c("4HHB", "1CRN", "2PTC"))
})

test_that("data_fetcher_batch retries transient failures", {
  attempts <- 0L

  local_mocked_bindings(
    data_fetcher = function(id, data_type = "ENTRY", properties = NULL, return_as_dataframe = TRUE, verbosity = FALSE) {
      attempts <<- attempts + 1L
      if (attempts < 2L) {
        stop("transient")
      }
      data.frame(rcsb_id = id, stringsAsFactors = FALSE)
    },
    .package = "rPDBapi"
  )

  res <- data_fetcher_batch(
    id = c("4HHB"),
    data_type = "ENTRY",
    properties = list(rcsb_id = list()),
    batch_size = 1,
    retry_attempts = 3,
    retry_backoff = 0,
    cache = FALSE
  )

  expect_equal(res$rcsb_id, "4HHB")
  expect_equal(attempts, 2L)
})

test_that("data_fetcher_batch uses cache when enabled", {
  call_count <- 0L
  cache_dir <- file.path(tempdir(), "rpdbapi-cache-batch")
  unlink(cache_dir, recursive = TRUE, force = TRUE)
  on.exit(unlink(cache_dir, recursive = TRUE, force = TRUE), add = TRUE)

  local_mocked_bindings(
    data_fetcher = function(id, data_type = "ENTRY", properties = NULL, return_as_dataframe = TRUE, verbosity = FALSE) {
      call_count <<- call_count + 1L
      data.frame(rcsb_id = id, stringsAsFactors = FALSE)
    },
    .package = "rPDBapi"
  )

  res1 <- data_fetcher_batch(
    id = c("4HHB", "1CRN"),
    data_type = "ENTRY",
    properties = list(rcsb_id = list()),
    batch_size = 2,
    retry_attempts = 1,
    cache = TRUE,
    cache_dir = cache_dir
  )
  res2 <- data_fetcher_batch(
    id = c("4HHB", "1CRN"),
    data_type = "ENTRY",
    properties = list(rcsb_id = list()),
    batch_size = 2,
    retry_attempts = 1,
    cache = TRUE,
    cache_dir = cache_dir
  )

  expect_equal(call_count, 1L)
  expect_equal(res1$rcsb_id, c("4HHB", "1CRN"))
  expect_equal(res2$rcsb_id, c("4HHB", "1CRN"))
})

test_that("data_fetcher_batch returns provenance metadata", {
  local_mocked_bindings(
    data_fetcher = function(id, data_type = "ENTRY", properties = NULL, return_as_dataframe = TRUE, verbosity = FALSE) {
      data.frame(rcsb_id = id, stringsAsFactors = FALSE)
    },
    .package = "rPDBapi"
  )

  res <- data_fetcher_batch(
    id = c("4HHB", "1CRN", "2PTC"),
    data_type = "ENTRY",
    properties = list(rcsb_id = list()),
    batch_size = 2,
    retry_attempts = 2,
    retry_backoff = 0,
    cache = FALSE
  )

  provenance <- attr(res, "provenance")
  expect_true(is.list(provenance))
  expect_equal(provenance$mode, "batch")
  expect_equal(provenance$data_type, "ENTRY")
  expect_equal(provenance$requested_ids, 3L)
  expect_equal(provenance$num_batches, 2L)
})
