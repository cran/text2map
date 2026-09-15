if (requireNamespace("testthat", quietly = TRUE)) {
  library(testthat)
  library(text2map)
  options(useFancyQuotes = FALSE)
  test_check("text2map")
} else {
  message("Package 'testthat' not available. Skipping tests.")
}
