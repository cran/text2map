test_that("doc_centrality, degree", {
    # dtm_dgc contains a genuinely isolated document, so degree centrality
    # warns (tested explicitly in "doc_centrality degree warns about isolated
    # (untied) documents" below); suppress here since this test is only
    # checking values, not the warning itself
    out1 <- suppressWarnings(doc_centrality(dtm_dgc, method = "degree"))
    out2 <- suppressWarnings(doc_centrality(dtm_dgc, method = "degree", alpha = 0.5))
    out3 <- suppressWarnings(doc_centrality(dtm_bse, method = "degree"))

    dsm <- Matrix::tcrossprod(dtm_dgc)
    out4 <- suppressWarnings(doc_centrality(dsm, method = "degree", two_mode = FALSE))

    expect_s3_class(out1, "data.frame")
    expect_s3_class(out2, "data.frame")
    expect_s3_class(out3, "data.frame")
    expect_s3_class(out4, "data.frame")

    expect_identical(out1, out4)
    expect_identical(out1, out3)
    expect_identical(dim(out1), c(10L, 2L))
    expect_identical(dim(out2), c(10L, 2L))
    expect_identical(dim(out3), c(10L, 2L))
    expect_identical(dim(out4), c(10L, 2L))

    expect_false(all(out1$degree == out2$degree))

    expect_equal(out1$degree[1], 20)
    expect_equal(round(out2$degree[1], 3), 10.954)
    expect_equal(out3$degree[1], 20)
    expect_equal(out4$degree[1], 20)
})

test_that("doc_centrality errors with a clear message when two_mode=FALSE gets a non-square matrix", {
    # with two_mode = FALSE, `mat` is assumed to already be a square
    # document-document matrix; a non-square input silently produced
    # plausible-looking but meaningless numbers for method = "degree"
    # (no shape check existed before dispatch)
    non_square <- matrix(runif(5 * 8), nrow = 5, ncol = 8)

    expect_error(
        doc_centrality(non_square, method = "degree", two_mode = FALSE),
        "square"
    )
})

test_that("doc_centrality degree warns about isolated (untied) documents", {
    # doc1 and doc3 share term "a" (so both have a tie); doc2 uses only "c",
    # shared with no other document, so it has no ties at all
    mat <- Matrix::Matrix(
        c(
            1, 1, 0, 0,
            0, 0, 1, 0,
            1, 0, 0, 1
        ),
        nrow = 3, byrow = TRUE, sparse = TRUE,
        dimnames = list(c("doc1", "doc2", "doc3"), c("a", "b", "c", "d"))
    )

    expect_warning(
        out <- doc_centrality(mat, method = "degree"),
        "1 document\\(s\\) have no ties"
    )
    expect_true(is.nan(out$degree[out$doc_id == "doc2"]))
    expect_false(is.nan(out$degree[out$doc_id == "doc1"]))
})

test_that("doc_centrality, eigen", {
    skip_if_not_installed("RSpectra")
    out1 <- doc_centrality(dtm_dgc, method = "eigen")
    out3 <- doc_centrality(dtm_bse, method = "eigen")

    dsm <- Matrix::tcrossprod(dtm_dgc)
    out4 <- doc_centrality(dsm, method = "eigen", two_mode = FALSE)

    expect_s3_class(out1, "data.frame")
    expect_s3_class(out3, "data.frame")
    expect_s3_class(out4, "data.frame")

    expect_identical(out1, out4)
    expect_identical(out1, out3)
    expect_identical(dim(out1), c(10L, 2L))
    expect_identical(dim(out3), c(10L, 2L))
    expect_identical(dim(out4), c(10L, 2L))

    expect_equal(round(out1$eigen[1], 3), 0.464)
    expect_equal(round(out3$eigen[1], 3), 0.464)
    expect_equal(round(out4$eigen[1], 3), 0.464)
})

test_that("doc_centrality, between", {
    out1 <- doc_centrality(dtm_dgc, method = "between")
    out2 <- doc_centrality(dtm_dgc, method = "between", alpha = 0.5)
    out3 <- doc_centrality(dtm_bse, method = "between")

    dsm <- Matrix::tcrossprod(dtm_dgc)
    out4 <- doc_centrality(dsm, method = "between", two_mode = FALSE)

    expect_s3_class(out1, "data.frame")
    expect_s3_class(out2, "data.frame")
    expect_s3_class(out3, "data.frame")
    expect_s3_class(out4, "data.frame")

    expect_identical(out1, out4)
    expect_identical(out1, out3)
    expect_identical(dim(out1), c(10L, 2L))
    expect_identical(dim(out2), c(10L, 2L))
    expect_identical(dim(out3), c(10L, 2L))
    expect_identical(dim(out4), c(10L, 2L))

    expect_false(all(out1$between == out2$between))

    expect_equal(round(out1$between[2], 3), 4)
    expect_equal(round(out3$between[2], 3), 4)
    expect_equal(round(out4$between[2], 3), 4)
})

test_that("doc_centrality, span", {
    # spanning is (currently) undefined for
    # disconnected graphs
    dtm_dgc1 <- cbind(dtm_dgc, 1)
    dtm_bse1 <- as.matrix(dtm_dgc1)

    out1 <- doc_centrality(dtm_dgc1, method = "span")
    out2 <- doc_centrality(dtm_dgc1, method = "span", alpha = 0.5)
    out3 <- doc_centrality(dtm_bse1, method = "span")

    dsm <- Matrix::tcrossprod(dtm_dgc1)
    out4 <- doc_centrality(dsm, method = "span", two_mode = FALSE)

    expect_s3_class(out1, "data.frame")
    expect_s3_class(out2, "data.frame")
    expect_s3_class(out3, "data.frame")
    expect_s3_class(out4, "data.frame")

    expect_identical(out1, out4)
    expect_identical(out1, out3)
    expect_identical(dim(out1), c(10L, 2L))
    expect_identical(dim(out2), c(10L, 2L))
    expect_identical(dim(out3), c(10L, 2L))
    expect_identical(dim(out4), c(10L, 2L))

    expect_false(all(out1$span == out2$span))

    expect_equal(round(out1$span[1], 3), -1.276)
    expect_equal(round(out2$span[1], 3), 0.193)
    expect_equal(round(out3$span[1], 3), -1.276)
    expect_equal(round(out4$span[1], 3), -1.276)
})

test_that("doc_centrality span works without igraph installed", {
    testthat::local_mocked_bindings(
        requireNamespace = function(package, ...) FALSE,
        .package = "text2map"
    )

    dtm_dgc1 <- cbind(dtm_dgc, 1)

    expect_no_error(doc_centrality(dtm_dgc1, method = "span"))

    out <- doc_centrality(dtm_dgc1, method = "span")
    expect_equal(round(out$span[1], 3), -1.276)
})

test_that("doc_similarity, projection", {
    out1 <- doc_similarity(dtm_dgc, method = "projection")
    expect_true(ncol(out1) == nrow(out1))
})

test_that("doc_similarity, cosine", {
    out1 <- doc_similarity(dtm_dgc, method = "cosine")
    expect_true(ncol(out1) == nrow(out1))
})


test_that("doc_similarity, cosine, two DTMs", {
    out1 <- doc_similarity(dtm_dgc, method = "cosine")
    expect_true(ncol(out1) == nrow(out1))
})


test_that("doc_similarity, wmd", {
    out1 <- doc_similarity(dtm_dgc,
        wv = fake_word_vectors,
        method = "wmd"
    )
    expect_true(ncol(out1) == nrow(out1))
})

test_that("doc_similarity, centroid", {
    out1 <- doc_similarity(dtm_dgc,
        wv = fake_word_vectors,
        method = "centroid"
    )
    expect_true(ncol(out1) == nrow(out1))
})

test_that("doc_similarity errors on invalid method", {
    expect_error(
        doc_similarity(dtm_dgc, method = "jaccard"),
        class = "simpleError"
    )
})

test_that("doc_similarity cosine values are in valid range", {
    out <- doc_similarity(dtm_dgc, method = "cosine")
    vals <- as.numeric(out[upper.tri(out)])
    expect_true(all(vals >= -1 & vals <= 1))
    diag_vals <- diag(out)
    expect_true(all(abs(diag_vals - 1) < 1e-10))
})

test_that("doc_centrality errors on invalid method", {
    expect_error(
        doc_centrality(dtm_dgc, method = "closeness"),
        class = "simpleError"
    )
})

test_that("doc_similarity projection diagonal is non-negative", {
    out <- doc_similarity(dtm_dgc, method = "projection")
    diag_vals <- diag(out)
    expect_true(all(diag_vals >= 0))
    expect_true(ncol(out) == nrow(out))
})

test_that("doc_similarity cosine matrix is symmetric", {
    out <- doc_similarity(dtm_dgc, method = "cosine")
    diff <- abs(as.matrix(out) - as.matrix(t(out)))
    expect_true(max(diff) < 1e-10)
})

test_that("doc_centrality degree with alpha=1 approximates rowSums of tcrossprod minus diagonal", {
    out <- suppressWarnings(doc_centrality(dtm_dgc, method = "degree", alpha = 1L))
    dsm <- Matrix::tcrossprod(dtm_dgc)
    expected <- as.numeric(Matrix::rowSums(dsm) - Matrix::diag(dsm))
    valid <- !is.nan(out$degree) & !is.nan(expected)
    expect_equal(out$degree[valid], expected[valid], tolerance = 1e-10)
})

test_that("doc_centrality eigen values are bounded", {
    skip_if_not_installed("RSpectra")
    out <- doc_centrality(dtm_dgc, method = "eigen")
    expect_true(all(out$eigen >= -1))
    expect_true(all(out$eigen <= 1))
})
