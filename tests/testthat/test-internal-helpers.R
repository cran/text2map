# Tests for internal helper functions

# -----------------------------------------------------------------------------
# Tests for .split_dtm (CMDist.R)
# -----------------------------------------------------------------------------

test_that(".split_dtm splits DTM indices correctly", {
  # Test basic splitting
  result <- .split_dtm(n_docs = 100, threads = 4)

  # Returns a base matrix
  expect_true(is.matrix(result))
  expect_identical(ncol(result), 3L)  # lower, upper, size

  # Check dimensions: 4 rows (one per thread), 3 columns
  expect_identical(dim(result), c(4L, 3L))

  # Check that lower bounds start at 1
  expect_equal(unname(result[1, "lower"]), 1)

  # Check that upper bounds are sequential and increasing
  expect_true(all(diff(result[, "upper"]) >= 0))
})

test_that(".split_dtm handles edge cases", {
  # Single thread
  result1 <- .split_dtm(n_docs = 100, threads = 1)
  expect_equal(unname(result1[1, "lower"]), 1)
  expect_equal(unname(result1[1, "upper"]), 100)

  # More threads than docs
  result2 <- .split_dtm(n_docs = 5, threads = 10)
  expect_identical(nrow(result2), 5L)  # Should cap at n_docs

  # Zero threads
  result3 <- .split_dtm(n_docs = 100, threads = 0)
  expect_identical(nrow(result3), 1L)  # Should default to 1

  # Threads equal to docs
  result4 <- .split_dtm(n_docs = 5, threads = 5)
  expect_identical(nrow(result4), 5L)
})

test_that(".split_dtm boundary conditions", {
  # Very small DTM
  result1 <- .split_dtm(n_docs = 2, threads = 2)
  expect_equal(unname(result1[, "lower"]), c(1, 2))
  expect_equal(unname(result1[, "upper"]), c(1, 2))
  expect_equal(unname(result1[, "size"]), c(1, 1))

  # Single document
  result2 <- .split_dtm(n_docs = 1, threads = 1)
  expect_equal(unname(result2[1, "lower"]), 1)
  expect_equal(unname(result2[1, "upper"]), 1)
})

# -----------------------------------------------------------------------------
# Tests for .conf_int (utils.R)
# -----------------------------------------------------------------------------

test_that(".conf_int calculates confidence intervals correctly", {
  set.seed(42)
  x <- rnorm(100, mean = 10, sd = 2)

  result <- .conf_int(x, ci = 0.95)

  expect_type(result, "double")
  expect_length(result, 3)

  # Mean should be approximately correct
  expect_equal(result[[1]], mean(x), tolerance = 1e-10)

  # CI should be symmetric around mean
  expect_equal(result[[1]] - result[[2]], result[[3]] - result[[1]], tolerance = 1e-10)
})

test_that(".conf_int handles different confidence levels", {
  set.seed(42)
  x <- rnorm(100)

  # 95% CI
  result_95 <- .conf_int(x, ci = 0.95)
  expect_true(result_95[[2]] < result_95[[1]])
  expect_true(result_95[[3]] > result_95[[1]])

  # 99% CI - should be wider
  result_99 <- .conf_int(x, ci = 0.99)
  expect_true(result_99[[2]] < result_95[[2]])
  expect_true(result_99[[3]] > result_95[[3]])

  # 90% CI - should be narrower
  result_90 <- .conf_int(x, ci = 0.90)
  expect_true((result_90[[3]] - result_90[[2]]) < (result_95[[3]] - result_95[[2]]))
})

# -----------------------------------------------------------------------------
# Tests for .check_whole_num (utils.R)
# -----------------------------------------------------------------------------

test_that(".check_whole_num correctly identifies whole numbers", {
  # Whole numbers (both integer storage and numeric) return TRUE
  expect_true(.check_whole_num(5L))
  expect_true(.check_whole_num(0L))
  expect_true(.check_whole_num(-10L))
  expect_true(.check_whole_num(5.0))
  expect_true(.check_whole_num(0.0))
  expect_true(.check_whole_num(-10.0))

  # Non-whole numbers return FALSE
  expect_false(.check_whole_num(5.5))
  expect_false(.check_whole_num(pi))
  expect_false(.check_whole_num(0.1))
  expect_false(.check_whole_num(-3.14))
})

# -----------------------------------------------------------------------------
# Tests for .make_cormat (CoCA.R)
# -----------------------------------------------------------------------------

test_that(".make_cormat creates correlation matrix", {
  set.seed(123)
  n_docs <- 20
  n_dirs <- 3
  cmds <- matrix(rnorm(n_docs * n_dirs), nrow = n_docs, ncol = n_dirs)
  rownames(cmds) <- paste0("doc_", seq_len(n_docs))

  result <- .make_cormat(cmds, zero_action = "drop")

  expect_true(is.matrix(result))
  expect_identical(dim(result), c(20L, 20L))

  # Should be symmetric
  expect_equal(result, t(result))

  # Diagonal should be 0 (ignore names)
  expect_equal(unname(diag(result)), rep(0, n_docs))
})

test_that(".make_cormat handles ownclass zero_action", {
  set.seed(123)
  n_docs <- 20
  n_dirs <- 3
  cmds <- matrix(rnorm(n_docs * n_dirs), nrow = n_docs, ncol = n_dirs)
  rownames(cmds) <- paste0("doc_", seq_len(n_docs))

  result <- .make_cormat(cmds, zero_action = "ownclass")

  expect_true(is.matrix(result))
  expect_identical(dim(result), c(20L, 20L))
  expect_equal(unname(diag(result)), rep(0, n_docs))
})

# -----------------------------------------------------------------------------
# Tests for .filter.insignif (CoCA.R)
# -----------------------------------------------------------------------------

test_that(".filter.insignif filters correlations", {
  set.seed(123)
  n_dirs <- 5

  cormat <- matrix(0.3, nrow = n_dirs, ncol = n_dirs)
  diag(cormat) <- 0
  rownames(cormat) <- paste0("dir_", seq_len(n_dirs))
  colnames(cormat) <- paste0("dir_", seq_len(n_dirs))

  expect_warning(
    result <- .filter.insignif(cormat, n_dirs = n_dirs, filter_value = 0.05),
    "Significance filtering left"
  )

  expect_true(is.matrix(result))
  expect_identical(dim(result), c(5L, 5L))
})

test_that(".filter.insignif warning for isolates", {
  set.seed(123)
  n_dirs <- 5

  # Create matrix with isolated nodes
  cormat <- matrix(0, nrow = n_dirs, ncol = n_dirs)
  cormat[1, 2] <- cormat[2, 1] <- 0.5
  cormat[1, 3] <- cormat[3, 1] <- 0.5
  cormat[2, 3] <- cormat[3, 2] <- 0.5
  diag(cormat) <- 0
  rownames(cormat) <- paste0("dir_", seq_len(n_dirs))
  colnames(cormat) <- paste0("dir_", seq_len(n_dirs))

  # Should warn about isolates
  expect_warning(
    result <- .filter.insignif(cormat, n_dirs = n_dirs, filter_value = 0.05)
  )
})

# -----------------------------------------------------------------------------
# Tests for .get_cor_class (CoCA.R)
# -----------------------------------------------------------------------------

test_that(".get_cor_class identifies classes", {
  set.seed(123)
  n_docs <- 30
  n_dirs <- 4

  # Create data with clear structure
  cmds1 <- cbind(
    rnorm(15, mean = 2),
    rnorm(15, mean = 0),
    rnorm(15, mean = 0),
    rnorm(15, mean = 0)
  )
  cmds2 <- cbind(
    rnorm(15, mean = 0),
    rnorm(15, mean = 2),
    rnorm(15, mean = 0),
    rnorm(15, mean = 0)
  )
  cmds <- rbind(cmds1, cmds2)
  rownames(cmds) <- paste0("doc_", seq_len(n_docs))
  colnames(cmds) <- paste0("dir_", seq_len(n_dirs))

  result <- .get_cor_class(cmds, filter_sig = FALSE, filter_value = 0.05, zero_action = "drop")

  expect_type(result, "list")
  expect_named(result, c("membership", "modules", "cormat"))
  expect_length(result$membership, n_docs)
  expect_true(length(result$modules) >= 1)
})

# -----------------------------------------------------------------------------
# Tests for dtm_melter (utils-dtm.R)
# -----------------------------------------------------------------------------

test_that("dtm_melter converts DTM to triplet format", {
  result <- dtm_melter(dtm_dgc)

  expect_s3_class(result, "data.frame")
  expect_named(result, c("doc_id", "term", "freq"))

  # Check doc_ids come from rownames of DTM
  expect_true(all(result$doc_id %in% rownames(dtm_dgc)))

  # Check terms come from colnames of DTM
  expect_true(all(result$term %in% colnames(dtm_dgc)))

  # Check all frequencies are positive integers
  expect_true(all(result$freq >= 1))
  expect_true(all(result$freq == floor(result$freq)))
})

test_that("dtm_melter handles dense matrices", {
  result_dense <- dtm_melter(dtm_bse)

  expect_s3_class(result_dense, "data.frame")
  expect_named(result_dense, c("doc_id", "term", "freq"))
})

# -----------------------------------------------------------------------------
# Tests for dtm_resampler (utils-dtm.R)
# -----------------------------------------------------------------------------

test_that("dtm_resampler resamples DTM rows", {
  set.seed(42)

  result <- dtm_resampler(dtm_dgc, alpha = 1)

  expect_s4_class(result, "dgCMatrix")
  expect_identical(dim(result), dim(dtm_dgc))

  # Same total terms (approximately with sampling)
  expect_equal(sum(result), sum(dtm_dgc), tolerance = 5)
})

test_that("dtm_resampler handles alpha parameter", {
  set.seed(42)

  result_05 <- dtm_resampler(dtm_dgc, alpha = 0.5)
  expect_s4_class(result_05, "dgCMatrix")
})

test_that("dtm_resampler warning for alpha and n", {
  set.seed(42)

  # n parameter overrides alpha - should warn
  expect_warning(dtm_resampler(dtm_dgc, alpha = 0.5, n = 50L))
})

# -----------------------------------------------------------------------------
# Tests for .remove_empty_rows (utils-dtm.R)
# -----------------------------------------------------------------------------

test_that(".remove_empty_rows removes empty documents", {
  dtm_test <- dtm_dgc
  dtm_test[1, ] <- 0

  result <- .remove_empty_rows(dtm_test)

  expect_s4_class(result, "dgCMatrix")
  expect_equal(nrow(result), nrow(dtm_test) - 1)
})

test_that(".remove_empty_rows does nothing when no empty rows", {
  result <- .remove_empty_rows(dtm_dgc)

  expect_s4_class(result, "dgCMatrix")
  expect_identical(nrow(result), nrow(dtm_dgc))
})

test_that(".remove_empty_rows produces message", {
  dtm_test <- dtm_dgc
  dtm_test[1, ] <- 0
  dtm_test[3, ] <- 0

  expect_message(result <- .remove_empty_rows(dtm_test))
})

# -----------------------------------------------------------------------------
# Tests for .check_term_in_embeddings (utils-embedding-matrices.R)
# -----------------------------------------------------------------------------

test_that(".check_term_in_embeddings validates terms", {
  terms <- c("choose", "moon")
  result <- .check_term_in_embeddings(terms, fake_word_vectors, action = "stop")
  expect_identical(result, terms)
})

test_that(".check_term_in_embeddings stops on missing terms", {
  terms_missing <- c("choose", "nonexistent_word_xyz")
  expect_error(.check_term_in_embeddings(terms_missing, fake_word_vectors, action = "stop"))
})

test_that(".check_term_in_embeddings removes missing terms", {
  terms_missing <- c("choose", "nonexistent_word_xyz")
  result <- .check_term_in_embeddings(terms_missing, fake_word_vectors, action = "remove")
  expect_identical(result, "choose")
})

test_that(".check_term_in_embeddings removes a multi-word phrase when only part of it is missing", {
  terms_missing <- c("choose", "nonexistent_word_xyz moon")
  result <- .check_term_in_embeddings(terms_missing, fake_word_vectors, action = "remove")
  expect_identical(result, "choose")
})

test_that(".check_term_in_embeddings handles data.frame with valid terms", {
  terms_df <- data.frame(pole1 = c("choose", "moon"), pole2 = c("decade", "moon"))
  result <- .check_term_in_embeddings(terms_df, fake_word_vectors, action = "stop")
  expect_identical(result, terms_df)
})

test_that(".check_term_in_embeddings handles data.frame with missing terms", {
  terms_df <- data.frame(pole1 = c("choose", "moon"), pole2 = c("decade", "xyz_missing"))
  result <- .check_term_in_embeddings(terms_df, fake_word_vectors, action = "remove")
  expect_equal(nrow(result), 1)
})

# -----------------------------------------------------------------------------
# Tests for .convert_mat_to_dgCMatrix (utils-dtm.R)
# -----------------------------------------------------------------------------

test_that(".convert_mat_to_dgCMatrix handles base matrix", {
  result <- .convert_mat_to_dgCMatrix(dtm_bse)

  expect_s4_class(result, "dgCMatrix")
  expect_identical(dim(result), dim(dtm_bse))
})

test_that(".convert_mat_to_dgCMatrix handles dgCMatrix", {
  result <- .convert_mat_to_dgCMatrix(dtm_dgc)

  expect_s4_class(result, "dgCMatrix")
  expect_identical(dim(result), dim(dtm_dgc))
})

test_that(".convert_mat_to_dgCMatrix preserves dimnames", {
  result <- .convert_mat_to_dgCMatrix(dtm_bse)

  expect_identical(rownames(result), rownames(dtm_bse))
  expect_identical(colnames(result), colnames(dtm_bse))
})

# -----------------------------------------------------------------------------
# Tests for .rnorm_trunc (utils.R)
# -----------------------------------------------------------------------------

test_that(".rnorm_trunc generates truncated normal values", {
  set.seed(42)

  result <- .rnorm_trunc(n = 1000, mean = 0, sd = 1, min = -2, max = 2)

  expect_length(result, 1000)
  # All values should be within bounds
  expect_true(all(result >= -2 & result <= 2))
  # Should have normal-like distribution
  expect_true(abs(mean(result)) < 0.2)  # Should be close to 0
})

# -----------------------------------------------------------------------------
# Tests for .kurtosis and .skewness (utils.R)
# -----------------------------------------------------------------------------

test_that(".kurtosis calculates correctly", {
  set.seed(42)
  x <- rnorm(1000, mean = 0, sd = 1)

  result <- .kurtosis(x)

  expect_type(result, "double")
  expect_length(result, 1)
  # Standard normal has kurtosis close to 0
  expect_true(abs(result) < 0.5)
})

test_that(".skewness calculates correctly", {
  set.seed(42)
  x <- rnorm(1000, mean = 0, sd = 1)

  result <- .skewness(x)

  expect_type(result, "double")
  expect_length(result, 1)
  # Standard normal has skewness close to 0
  expect_true(abs(result) < 0.2)
})

# -----------------------------------------------------------------------------
# Tests for .n_decimal_places (utils.R)
# -----------------------------------------------------------------------------

test_that(".n_decimal_places counts decimals correctly", {
  expect_equal(.n_decimal_places(1.234), 3L)
  expect_equal(.n_decimal_places(1.0), 0L)
  # pi has decimal representation that varies by R version
  expect_gte(.n_decimal_places(pi), 1L)
  expect_equal(.n_decimal_places(100), 0L)
})
