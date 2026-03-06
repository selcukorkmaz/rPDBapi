local({
  autoload_enabled <- tolower(Sys.getenv("RENV_CONFIG_AUTOLOADER_ENABLED", "true"))
  if (autoload_enabled %in% c("false", "0", "no", "off")) {
    return(invisible(NULL))
  }

  activate <- file.path("renv", "activate.R")
  if (!file.exists(activate)) {
    return(invisible(NULL))
  }

  tryCatch(
    source(activate),
    error = function(e) {
      packageStartupMessage("renv activation skipped: ", conditionMessage(e))
      invisible(NULL)
    }
  )
})
