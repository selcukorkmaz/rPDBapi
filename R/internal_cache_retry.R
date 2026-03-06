# Internal cache and retry helpers for batch workflows.

rpdbapi_default_cache_dir <- function() {
  file.path(tempdir(), "rPDBapi-cache")
}

rpdbapi_resolve_cache_dir <- function(cache_dir = NULL) {
  if (is.null(cache_dir) || !nzchar(cache_dir)) {
    cache_dir <- getOption("rPDBapi.cache_dir", rpdbapi_default_cache_dir())
  }
  normalizePath(cache_dir, winslash = "/", mustWork = FALSE)
}

rpdbapi_cache_ensure_dir <- function(cache_dir) {
  if (!dir.exists(cache_dir)) {
    dir.create(cache_dir, recursive = TRUE, showWarnings = FALSE)
  }
  cache_dir
}

rpdbapi_hash_text <- function(text) {
  tf <- tempfile(fileext = ".txt")
  on.exit(unlink(tf), add = TRUE)
  writeLines(text, tf, useBytes = TRUE)
  unname(tools::md5sum(tf))
}

rpdbapi_cache_key <- function(ids, data_type, properties, return_as_dataframe) {
  payload <- jsonlite::toJSON(
    list(
      ids = as.character(ids),
      data_type = data_type,
      properties = properties,
      return_as_dataframe = return_as_dataframe
    ),
    auto_unbox = TRUE,
    null = "null"
  )
  rpdbapi_hash_text(payload)
}

rpdbapi_cache_path <- function(cache_dir, key) {
  file.path(cache_dir, paste0("cache-", key, ".rds"))
}

rpdbapi_cache_read <- function(cache_dir, key) {
  path <- rpdbapi_cache_path(cache_dir, key)
  if (!file.exists(path)) {
    return(NULL)
  }
  readRDS(path)
}

rpdbapi_cache_write <- function(cache_dir, key, value) {
  path <- rpdbapi_cache_path(cache_dir, key)
  saveRDS(value, path)
  invisible(path)
}

rpdbapi_with_retry <- function(expr_fn, retry_attempts = 1, retry_backoff = 0.5) {
  last_error <- NULL
  attempts <- max(1L, as.integer(retry_attempts))

  for (attempt in seq_len(attempts)) {
    result <- tryCatch(
      expr_fn(),
      error = function(e) {
        last_error <<- e
        NULL
      }
    )

    if (!is.null(result)) {
      return(list(result = result, attempts = attempt))
    }

    if (attempt < attempts) {
      Sys.sleep(retry_backoff * attempt)
    }
  }

  stop(last_error)
}
