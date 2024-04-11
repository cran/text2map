test_that("doc_centrality, degree", {
    out1 <- doc_centrality(dtm_dgc, method = "degree")
    out2 <- doc_centrality(dtm_dgc, method = "degree", alpha = 0.5)
    out3 <- doc_centrality(dtm_bse, method = "degree")

    dsm <- Matrix::tcrossprod(dtm_dgc)
    out4 <- doc_centrality(dsm, method = "degree", two_mode = FALSE)

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

test_that("doc_centrality, eigen", {
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


test_that("doc_similarity, projection", {
    out1 <- doc_similarity(dtm_dgc, method = "projection")
    expect_true(ncol(out1) == nrow(out1))
})

test_that("doc_similarity, cosine", {
    out1 <- doc_similarity(dtm_dgc, method = "cosine")
    expect_true(ncol(out1) == nrow(out1))
})


test_that("doc_similarity, cosine, x and y", {
    out1 <- doc_similarity(dtm_dgc, dtm_dgc, method = "cosine")
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
