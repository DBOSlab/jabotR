skip_if_no_jabot <- function() {
  if (!identical(Sys.getenv("JABOT_LIVE_TESTS"), "true")) {
    testthat::skip(
      "Live JABOT integration test; set JABOT_LIVE_TESTS=true to run."
    )
  }
}
