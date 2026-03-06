#' Batch Fetch RCSB PDB Data with Optional Retry and Cache
#'
#' This additive helper retrieves data in batches and can retry failed batches.
#' Caching is optional and disabled by default, preserving existing package behavior.
#'
#' @param id Character vector of identifiers.
#' @param data_type Data type passed to \code{data_fetcher()}.
#' @param properties Property list passed to \code{data_fetcher()}.
#' @param return_as_dataframe Logical; passed through to \code{data_fetcher()}.
#' @param batch_size Number of IDs per batch.
#' @param retry_attempts Number of retry attempts per batch.
#' @param retry_backoff Backoff multiplier in seconds.
#' @param progress Logical; show progress messages when TRUE.
#' @param cache Logical; enable on-disk cache.
#' @param cache_dir Optional cache directory.
#' @param verbosity Logical; passed to \code{data_fetcher()}.
#' @return A combined object matching the chosen return mode with provenance metadata.
#' @export
data_fetcher_batch <- function(id,
                               data_type = "ENTRY",
                               properties,
                               return_as_dataframe = TRUE,
                               batch_size = 50,
                               retry_attempts = 3,
                               retry_backoff = 0.5,
                               progress = FALSE,
                               cache = FALSE,
                               cache_dir = NULL,
                               verbosity = FALSE) {
  if (is.null(id) || length(id) == 0) {
    rpdbapi_abort(
      "Invalid input: 'id' must not be NULL or empty.",
      class = "rPDBapi_error_invalid_input",
      function_name = "data_fetcher_batch"
    )
  }

  if (!is.numeric(batch_size) || length(batch_size) != 1 || batch_size < 1) {
    rpdbapi_abort(
      "Invalid input: 'batch_size' must be a positive integer.",
      class = "rPDBapi_error_invalid_input",
      function_name = "data_fetcher_batch",
      batch_size = batch_size
    )
  }

  ids <- rpdbapi_prepare_ids(id, data_type = data_type)
  batch_size <- as.integer(batch_size)
  split_group <- ceiling(seq_along(ids) / batch_size)
  id_batches <- split(ids, split_group)

  resolved_cache_dir <- NULL
  if (isTRUE(cache)) {
    resolved_cache_dir <- rpdbapi_cache_ensure_dir(rpdbapi_resolve_cache_dir(cache_dir))
  }

  batch_results <- vector("list", length(id_batches))
  batch_provenance <- vector("list", length(id_batches))
  cache_hits <- 0L

  for (i in seq_along(id_batches)) {
    batch_ids <- id_batches[[i]]
    key <- NULL
    cached_result <- NULL
    from_cache <- FALSE

    if (isTRUE(cache)) {
      key <- rpdbapi_cache_key(
        ids = batch_ids,
        data_type = data_type,
        properties = properties,
        return_as_dataframe = return_as_dataframe
      )
      cached_result <- rpdbapi_cache_read(resolved_cache_dir, key)
    }

    if (!is.null(cached_result)) {
      result <- cached_result
      from_cache <- TRUE
      attempts_used <- 0L
      cache_hits <- cache_hits + 1L
      if (isTRUE(progress)) {
        message("Batch ", i, "/", length(id_batches), " loaded from cache (", length(batch_ids), " IDs).")
      }
    } else {
      if (isTRUE(progress)) {
        message("Batch ", i, "/", length(id_batches), " fetching ", length(batch_ids), " IDs.")
      }

      fetched <- tryCatch(
        {
          rpdbapi_with_retry(
            expr_fn = function() {
              data_fetcher(
                id = batch_ids,
                data_type = data_type,
                properties = properties,
                return_as_dataframe = return_as_dataframe,
                verbosity = verbosity
              )
            },
            retry_attempts = retry_attempts,
            retry_backoff = retry_backoff
          )
        },
        error = function(e) {
          rpdbapi_rethrow(
            e,
            message_prefix = paste0("Batch ", i, " failed. Error: "),
            class = "rPDBapi_error_request_failed",
            function_name = "data_fetcher_batch",
            batch_index = i
          )
        }
      )

      result <- fetched$result
      attempts_used <- fetched$attempts

      if (isTRUE(cache)) {
        rpdbapi_cache_write(resolved_cache_dir, key, result)
      }
    }

    batch_results[[i]] <- result
    batch_provenance[[i]] <- list(
      batch_index = i,
      batch_size = length(batch_ids),
      ids = batch_ids,
      attempts = attempts_used,
      cache_hit = from_cache
    )
  }

  if (isTRUE(return_as_dataframe)) {
    combined <- dplyr::bind_rows(batch_results)
    combined <- rpdbapi_add_class(combined, "rPDBapi_dataframe")
  } else {
    combined_payload <- list()
    for (res in batch_results) {
      if (!is.null(res$data) && length(res$data) > 0 && is.list(res$data[[1]])) {
        combined_payload <- c(combined_payload, res$data[[1]])
      }
    }

    combined <- list(data = list(combined_payload))
    combined <- rpdbapi_add_class(combined, "rPDBapi_fetch_response")
  }

  attr(combined, "data_type") <- data_type
  attr(combined, "ids") <- ids
  attr(combined, "provenance") <- list(
    fetched_at = as.character(Sys.time()),
    mode = "batch",
    data_type = data_type,
    requested_ids = length(ids),
    batch_size = batch_size,
    num_batches = length(id_batches),
    retry_attempts = retry_attempts,
    retry_backoff = retry_backoff,
    cache_enabled = isTRUE(cache),
    cache_dir = resolved_cache_dir,
    cache_hits = cache_hits,
    cache_misses = length(id_batches) - cache_hits,
    batches = batch_provenance
  )

  combined
}

#' Inspect rPDBapi Cache Contents
#'
#' @param cache_dir Optional cache directory.
#' @return A list with cache summary and entry table.
#' @export
cache_info <- function(cache_dir = NULL) {
  resolved <- rpdbapi_resolve_cache_dir(cache_dir)
  if (!dir.exists(resolved)) {
    return(list(
      cache_dir = resolved,
      total_entries = 0L,
      total_size_bytes = 0,
      entries = data.frame(file = character(0), size_bytes = numeric(0), modified = character(0), stringsAsFactors = FALSE)
    ))
  }

  files <- list.files(resolved, pattern = "^cache-.*\\.rds$", full.names = TRUE)
  if (length(files) == 0) {
    return(list(
      cache_dir = resolved,
      total_entries = 0L,
      total_size_bytes = 0,
      entries = data.frame(file = character(0), size_bytes = numeric(0), modified = character(0), stringsAsFactors = FALSE)
    ))
  }

  finfo <- file.info(files)
  entries <- data.frame(
    file = basename(files),
    size_bytes = as.numeric(finfo$size),
    modified = as.character(finfo$mtime),
    stringsAsFactors = FALSE
  )

  list(
    cache_dir = resolved,
    total_entries = nrow(entries),
    total_size_bytes = sum(entries$size_bytes, na.rm = TRUE),
    entries = entries
  )
}

#' Clear rPDBapi Cache Directory
#'
#' @param cache_dir Optional cache directory.
#' @return Invisible list with removal summary.
#' @export
clear_rpdbapi_cache <- function(cache_dir = NULL) {
  resolved <- rpdbapi_resolve_cache_dir(cache_dir)
  if (!dir.exists(resolved)) {
    return(invisible(list(cache_dir = resolved, removed = 0L)))
  }

  files <- list.files(resolved, pattern = "^cache-.*\\.rds$", full.names = TRUE)
  removed <- 0L
  if (length(files) > 0) {
    removed <- sum(file.remove(files))
  }

  invisible(list(cache_dir = resolved, removed = removed))
}
