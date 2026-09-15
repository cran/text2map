# ON LOAD ----
# default threads to 2 for CRAN
# users will need to set threads
.onLoad <- function(libname, pkgname) {
  options("text2vec.mc.cores" = 2L)
}

# placeholder binding so testthat::local_mocked_bindings() can mock this base
# function in tests (R skips non-function bindings when resolving a call, so
# this does not change normal behavior)
requireNamespace <- NULL
