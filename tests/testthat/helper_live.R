is_live_tests_enabled <- function() {
  val <- tolower(Sys.getenv("RPDBAPI_RUN_LIVE", "false"))
  val %in% c("true", "1", "yes", "on")
}

skip_if_not_live_tests <- function() {
  testthat::skip_if_not(
    is_live_tests_enabled(),
    "Live API tests are disabled. Set RPDBAPI_RUN_LIVE=true to enable."
  )
  testthat::skip_if_offline()
}
