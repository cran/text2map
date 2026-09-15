test_that("dtm_stats returns expected output", {
    # simplified, base R dtm
    out <- dtm_stats(dtm_bse,
        richness = TRUE,
        distribution = TRUE,
        central = TRUE,
        character = TRUE,
        simplify = TRUE
    )
    expect_s3_class(out, "data.frame")
    expect_equal(as.integer(out$n_docs), nrow(dtm_bse))
    expect_equal(as.integer(out$n_types), ncol(dtm_bse))
})

test_that("dtm_stats warns on an empty DTM", {
    # computing stats on a truly empty DTM also triggers several downstream,
    # pre-existing warnings (min/max of an empty vector, NA coercion); capture
    # and muffle all of them so only the specific assertion below matters and
    # nothing leaks as unhandled noise in the test run
    capture_warnings <- function(expr) {
        warnings <- character(0)
        withCallingHandlers(
            expr,
            warning = function(w) {
                warnings <<- c(warnings, conditionMessage(w))
                invokeRestart("muffleWarning")
            }
        )
        warnings
    }

    w1 <- capture_warnings(dtm_stats(dtm_dgc[0, , drop = FALSE], simplify = TRUE))
    expect_true(any(grepl("0 row", w1, fixed = TRUE)))

    w2 <- capture_warnings(dtm_stats(dtm_dgc[, 0, drop = FALSE], simplify = TRUE))
    expect_true(any(grepl("0 column", w2, fixed = TRUE)))
})

test_that("seq_builder with maxlen truncates long documents", {
    out <- seq_builder(my_corpus, text, line_id, maxlen = 3L)
    expect_identical(dim(out), c(6L, 3L))
})

test_that("seq_builder without doc_id generates default doc names", {
    out <- seq_builder(my_corpus, text)
    expect_true(all(grepl("^doc_", rownames(out))))
    expect_identical(nrow(out), nrow(my_corpus))
})

test_that("seq_builder with matrix=FALSE returns named list", {
    out <- seq_builder(my_corpus, text, line_id, matrix = FALSE)
    expect_type(out, "list")
    expect_identical(length(out), nrow(my_corpus))
    expect_true(!is.null(attr(out, "dic")))
})

test_that("dtm_resampler with n argument produces equal row sums", {
    out <- dtm_resampler(dtm_dgc, n = 10)
    expect_s4_class(out, "dgCMatrix")
    expect_true(all(abs(as.numeric(Matrix::rowSums(out)) - 10) < 1e-10))
})

test_that("dtm_melter excludes zero-frequency entries", {
    out <- dtm_melter(dtm_dgc)
    expect_true(all(out$freq > 0))
    expect_s3_class(out, "data.frame")
    expect_identical(colnames(out), c("doc_id", "term", "freq"))
})



test_that("dtm_stats with simplify = FALSE returns named list of data.frames", {
    out <- dtm_stats(dtm_dgc, simplify = FALSE)
    expect_type(out, "list")
    expect_true("Basic Information" %in% names(out))
    expect_s3_class(out[[1]], "data.frame")
    expect_true("Measure" %in% colnames(out[[1]]))
    expect_true("Value" %in% colnames(out[[1]]))
})

test_that("dtm_stats with individual flags toggled off excludes those columns", {
    out_full <- dtm_stats(dtm_dgc, richness = TRUE, distribution = TRUE,
                          central = TRUE, character = TRUE, simplify = TRUE)
    out_min <- dtm_stats(dtm_dgc, richness = FALSE, distribution = FALSE,
                         central = FALSE, character = FALSE, simplify = TRUE)
    expect_true("n_docs" %in% colnames(out_min))
    expect_true("sparsity" %in% colnames(out_min))
    expect_true("n_types" %in% colnames(out_min))
    expect_false("hapax" %in% colnames(out_min))
    expect_false("ttr" %in% colnames(out_min))
})

test_that("dtm_stats with all-zero DTM produces sparsity 1 and n_tokens 0", {
    zero_dtm <- Matrix::Matrix(0, nrow = 3, ncol = 5, sparse = TRUE)
    rownames(zero_dtm) <- c("d1", "d2", "d3")
    colnames(zero_dtm) <- c("t1", "t2", "t3", "t4", "t5")
    out <- dtm_stats(zero_dtm, simplify = TRUE)
    expect_equal(as.numeric(out$sparsity), 1)
    expect_equal(as.integer(out$n_docs), 3L)
    expect_equal(as.integer(out$n_types), 5L)
    expect_equal(as.integer(out$n_tokens), 0L)
})


test_that("dtm_builder omit_empty removes empty rows", {
    empty_corpus <- data.frame(
        text = c("the the the", "", "a a", "", "of of"),
        doc_id = c("d1", "d2", "d3", "d4", "d5"),
        stringsAsFactors = FALSE
    )

    dtm_with <- dtm_builder(empty_corpus, text, doc_id)
    dtm_without <- dtm_builder(empty_corpus, text, doc_id, omit_empty = TRUE)

    expect_equal(nrow(dtm_with), 5L)
    expect_true(nrow(dtm_without) < 5L)
    expect_true(all(rowSums(dtm_without) > 0))
})

test_that("dtm_builder omit_empty with vocab removes empty rows", {
    empty_corpus <- data.frame(
        text = c("the the the", "", "a a", "", "of of"),
        doc_id = c("d1", "d2", "d3", "d4", "d5"),
        stringsAsFactors = FALSE
    )
    myvocab <- c("the", "a", "of")

    dtm_with <- dtm_builder(empty_corpus, text, doc_id, vocab = myvocab)
    dtm_without <- dtm_builder(empty_corpus, text, doc_id, vocab = myvocab, omit_empty = TRUE)

    expect_true(nrow(dtm_without) < nrow(dtm_with))
    expect_true(all(rowSums(dtm_without) > 0))
})


test_that("dtm_melter returns correct columns and types", {
    out <- dtm_melter(dtm_dgc)

    expect_s3_class(out, "data.frame")
    expect_identical(colnames(out), c("doc_id", "term", "freq"))
    expect_type(out$doc_id, "character")
    expect_type(out$term, "character")
    expect_true(is.numeric(out$freq))
})

test_that("dtm_melter works with dense matrix", {
    dtm_dense <- dtm_builder(my_corpus, clean_text, line_id, dense = TRUE)
    out_dense <- dtm_melter(dtm_dense)

    expect_s3_class(out_dense, "data.frame")
    expect_identical(colnames(out_dense), c("doc_id", "term", "freq"))
})

test_that(".remove_empty_rows removes empty rows and messages", {
    dtm1 <- dtm_builder(jfk_corpus, text, doc_id)

    empty_row <- Matrix::Matrix(0, nrow = 1, ncol = ncol(dtm1),
                                 sparse = TRUE)
    colnames(empty_row) <- colnames(dtm1)
    rownames(empty_row) <- "empty_doc"
    dtm_with_empty <- rbind(dtm1, empty_row)

    expect_message(
        dtm0 <- .remove_empty_rows(dtm_with_empty),
        "empty"
    )

    expect_equal(nrow(dtm_with_empty) - 1, nrow(dtm0))
    expect_equal(ncol(dtm_with_empty), ncol(dtm0))
    expect_false("empty_doc" %in% rownames(dtm0))
})

test_that(".remove_empty_rows returns original DTM when no empty rows", {
    dtm1 <- dtm_builder(jfk_corpus, text, doc_id)

    dtm0 <- .remove_empty_rows(dtm1)
    expect_identical(dtm1, dtm0)
})


test_that("dtm_melter works on both base and sparse", {
    out_a <- dtm_melter(dtm = dtm_bse)
    out_b <- dtm_melter(dtm = dtm_dgc)

    expect_type(out_a, "list")
    expect_type(out_b, "list")
    expect_equal(ncol(out_a), as.integer(3L))
    expect_equal(ncol(out_b), as.integer(3L))
    expect_equal(out_a, out_b)
    expect_equal(sum(out_a$freq), sum(dtm_bse))
    expect_equal(sum(out_b$freq), sum(dtm_dgc))
})


test_that("dtm_builder succeeds when the last row is blank", {
    my_corpus <- data.frame(
        my_text = c(
            "I hear babies I watch them grow",
            "They'll learn much more than I'll ever know",
            "And I think to myself",
            "What a wonderful world",
            "Yes I think to myself",
            "What wonderful worlds"
        ),
        line_id = paste0("line", seq_len(6))
    )

    my_corpus$my_text[6] <- ""

    dtm <- dtm_builder(my_corpus, my_text, line_id)
    expect_s4_class(dtm, "dgCMatrix")
    expect_equal(dtm@Dim[1L], 6L)
    expect_true(sum(dtm[6L, ]) == 0)
})

test_that("dtm_builder handles a blank row the same way regardless of position", {
    # a blank row used to error only when it was the *last* row — an
    # incidental side effect of Matrix::sparseMatrix() inferring dims
    # from max(i), which under-counts when the blank row is last.
    # Now that dims are passed explicitly, blank rows produce valid
    # all-zero rows regardless of position.
    my_corpus <- data.frame(
        my_text = c(
            "I hear barbies I watch them grow",
            "",
            "And I think to oneself",
            "What a bountiful world"
        ),
        line_id = paste0("line", seq_len(4))
    )

    dtm <- dtm_builder(my_corpus, my_text, line_id)
    expect_s4_class(dtm, "dgCMatrix")
    expect_equal(dtm@Dim[1L], 4L)
    expect_true(sum(dtm[2L, ]) == 0)
})


test_that("dtm_builder works with vocab", {
    vocab <- vocab_builder(jfk_corpus, text)
    new.vocab <- vocab[!vocab %in% c("moon")]

    expect_identical(
        dim(dtm_builder(jfk_corpus, text, doc_id,
            vocab = new.vocab
        )),
        as.integer(c(10, 43))
    )

    expect_identical(
        dim(dtm_builder(jfk_corpus, text, doc_id,
            vocab = new.vocab, chunk = 4L
        )),
        as.integer(c(19, 43))
    )

    expect_identical(
        dim(dtm_builder(jfk_corpus, text, doc_id,
            vocab = new.vocab
        )),
        as.integer(c(10, 43))
    )

    expect_error(
        dtm_builder(jfk_corpus, text, doc_ids,
            vocab = c(
                "hear", "babies",
                "world", "picklespit"
            )
        ),
        "not in the data frame"
    )
    expect_error(
        dtm_builder(jfk_corpus, text, doc_ids,
            vocab = new.vocab, chunk = 5L
        ),
        "not in the data frame"
    )
    expect_error(
        dtm_builder(jfk_corpus, text, doc_ids,
            vocab = new.vocab
        ),
        "not in the data frame"
    )
})

test_that("dtm_builder errors with a clear message when text contains NA", {
    corpus_na <- jfk_corpus
    corpus_na$text[1] <- NA

    expect_error(
        dtm_builder(corpus_na, text, doc_id),
        "NA"
    )
})

test_that("dtm_builder errors with a clear message when doc_id contains NA", {
    corpus_na <- jfk_corpus
    corpus_na$doc_id[1] <- NA

    expect_error(
        dtm_builder(corpus_na, text, doc_id),
        "NA"
    )
})

test_that("dtm_builder errors with a clear message when vocab has duplicate terms", {
    expect_error(
        dtm_builder(jfk_corpus, text, doc_id, vocab = c("moon", "moon", "hear")),
        "duplicate"
    )
})

test_that("dtm_builder errors with a clear message when vocab is empty", {
    expect_error(
        dtm_builder(jfk_corpus, text, doc_id, vocab = character(0)),
        "empty"
    )
})

test_that("dtm_builder with vocab + chunk does not create a spurious extra chunk row", {
    # 10 real, in-vocabulary tokens with chunk = 5 is an exact multiple:
    # the internal "INT_VC_MISS" sentinel used for OOV handling must not be
    # positionally grouped into its own bogus trailing chunk
    my_corpus <- data.frame(text = "a b c d e f g h i j", line_id = "line1")
    vocab <- letters[1:10]

    dtm <- dtm_builder(my_corpus, text, line_id, vocab = vocab, chunk = 5L)

    expect_equal(nrow(dtm), 2L)
    expect_true(all(Matrix::rowSums(dtm) == 5))
})

test_that("dtm_builder with vocab + chunk handles a vocabulary term absent from the text", {
    my_corpus <- data.frame(text = "a b c d e", line_id = "line1")
    vocab <- c(letters[1:5], "zzz_not_in_text")

    dtm <- dtm_builder(my_corpus, text, line_id, vocab = vocab, chunk = 5L)

    expect_equal(ncol(dtm), length(vocab))
    expect_equal(nrow(dtm), 1L)
    expect_equal(unname(as.matrix(dtm)[1, "zzz_not_in_text"]), 0)
})

test_that("dtm_builder with vocab + chunk buckets out-of-vocabulary tokens instead of skipping them for chunk boundaries", {
    # matches the non-chunk vocab branch's semantics: an out-of-vocabulary
    # token still occupies a token position (bucketed into the internal
    # "INT_VC_MISS" column, later dropped), rather than being skipped when
    # computing which chunk each subsequent token falls into. Without this,
    # `chunk = 5` silently means "5 in-vocabulary tokens" instead of
    # "5 tokens of the text", and chunk contents shift depending on how many
    # OOV words happen to appear.
    my_corpus <- data.frame(
        text = "a b c d zzznotinvocab e f g h i",
        line_id = "line1"
    )
    vocab <- letters[1:9]

    dtm <- dtm_builder(my_corpus, text, line_id, vocab = vocab, chunk = 5L)
    dtm <- as.matrix(dtm)

    expect_equal(nrow(dtm), 2L)
    # "e" is the 6th raw token (after the 5th-position OOV word), so it must
    # fall in chunk_2, not leak into chunk_1
    expect_equal(unname(dtm["chunk_1", "e"]), 0)
    expect_equal(unname(dtm["chunk_2", "e"]), 1)
    expect_equal(sum(dtm["chunk_1", ]), 4)
    expect_equal(sum(dtm["chunk_2", ]), 5)
})

test_that("dtm_builder error/message if doc_id is wrong...", {
    my_corpus <- data.frame(
        my_text = c(
            "I hear babies crying I watch them grow",
            "They'll learn much more than I'll ever know",
            "And I think to myself",
            "What a wonderful world",
            "Yes I think to myself",
            "What a wonderful world"
        ),
        line_id = paste0("line", seq_len(6))
    )

    expect_error(
        dtm_builder(my_corpus, my_text, doc_id = line_ids),
        "not in the data frame"
    )
})

test_that("dtm_builder chunks correctly", {
    chunk <- 3L
    dtm.e <- jfk_corpus %>%
        dtm_builder(text, doc_id, chunk = chunk)
    expect_equal(sum(dtm.e[1, ]), chunk)

    dtm.f <- jfk_corpus %>%
        dtm_builder(text, doc_id)
    expect_equal(sum(dtm.e), sum(dtm.f))

    chunk <- 100L
    dtm.g <- jfk_corpus %>%
        dtm_builder(text, doc_id, chunk = chunk)
    expect_equal(sum(dtm.g[1, ]), sum(dtm.f))
})

test_that("dtm resampler creates DTM of the same dimensions", {
    out <- dtm_resampler(dtm_dgc, alpha = 0.2)
    expect_s4_class(out, "dgCMatrix")
    expect_identical(dim(out), dim(dtm_dgc))
    expect_equal(sum(out), sum(dtm_dgc) * 0.2, tolerance = 0.1)

    out <- dtm_resampler(dtm_dgc, alpha = 0.5)
    expect_s4_class(out, "dgCMatrix")
    expect_identical(dim(out), dim(dtm_dgc))
    expect_equal(sum(out), sum(dtm_dgc) * 0.5, tolerance = 0.1)

    out <- dtm_resampler(dtm_dgc, alpha = 0.7)
    expect_s4_class(out, "dgCMatrix")
    expect_identical(dim(out), dim(dtm_dgc))
    expect_equal(sum(out), sum(dtm_dgc) * 0.7, tolerance = 0.1)

    out <- dtm_resampler(dtm_dgc, alpha = 1)
    expect_s4_class(out, "dgCMatrix")
    expect_identical(dim(out), dim(dtm_dgc))
    expect_equal(sum(out), sum(dtm_dgc) * 1, tolerance = 0.1)

    out <- dtm_resampler(dtm_dgc, n = 20)
    expect_s4_class(out, "dgCMatrix")
    expect_true(all(rowSums(out) == 20))

    out <- dtm_resampler(dtm_dgc)
    expect_s4_class(out, "dgCMatrix")
    expect_true(all(rowSums(out) == rowSums(dtm_dgc)))
    expect_equal(sum(out), sum(dtm_dgc))

    expect_warning(dtm_resampler(dtm_dgc, alpha = 0.5, n = 20))
})


# tests for internal functions
test_that("dtm resampler works on output of .prep_cmd_INPUT", {
    ## dgCMatrix matrix ##
    out <- .prep_cmd_INPUT(
        dtm = dtm_dgc[, seq_len(35)],
        cw = cw_oov,
        cv = NULL,
        wv = fake_word_vectors_oov,
        missing = "stop"
    )

    dtm.samp <- dtm_resampler(out$DTM, alpha = 1)
    expect_s4_class(dtm.samp, "dgCMatrix")
    expect_equal(nrow(dtm.samp), nrow(out$DTM))
    expect_equal(ncol(dtm.samp), ncol(out$DTM))
})

test_that("compare dtm dense and sparse", {
    dtm_sp <- jfk_corpus |>
        dtm_builder(text, doc_id, dense = FALSE)

    dtm_de <- jfk_corpus |>
        dtm_builder(text, doc_id, dense = TRUE)

    expect_s4_class(dtm_sp, "dgCMatrix")
    expect_identical(
        class(dtm_de),
        c("matrix", "array")
    )
    expect_equal(nrow(dtm_sp), nrow(dtm_de))
    expect_equal(ncol(dtm_sp), ncol(dtm_de))

    # compare with vocab
    vocab <- vocab_builder(jfk_corpus, text)
    new.vocab <- vocab[!vocab %in% c("moon")]

    dtm_sp <- dtm_builder(jfk_corpus, text, doc_id,
        vocab = new.vocab,
        dense = FALSE
    )

    dtm_de <- dtm_builder(jfk_corpus, text, doc_id,
        vocab = new.vocab,
        dense = TRUE
    )

    expect_s4_class(dtm_sp, "dgCMatrix")
    expect_identical(
        class(dtm_de),
        c("matrix", "array")
    )
    expect_equal(nrow(dtm_sp), nrow(dtm_de))
    expect_equal(ncol(dtm_sp), ncol(dtm_de))
})

test_that("works w/o doc_id", {
    dtm1 <- jfk_corpus %>%
        dtm_builder(text, doc_id, dense = FALSE)
    dtm2 <- jfk_corpus %>%
        dtm_builder(text, dense = FALSE)

    expect_equal(nrow(dtm1), nrow(dtm2))
    expect_equal(ncol(dtm1), ncol(dtm2))
})

test_that("dtm functions with one document", {
    dtm1 <- dtm_builder(jfk_corpus[1, ], text, doc_id, dense = FALSE)
    expect_identical(dim(dtm1), as.integer(c(1, 6)))

    dtm2 <- dtm_stopper(dtm1, stop_list = c("we", "moon"))
    expect_identical(dim(dtm2), as.integer(c(1, 4)))

    expect_identical(
        dim(dtm_melter(dtm1)),
        as.integer(c(6, 3))
    )

    dtm_c <- dtm_builder(jfk_corpus[1, ], text, chunk = 1L, dense = FALSE)
    expect_identical(dim(dtm_c), as.integer(c(7, 6)))

    expect_identical(
        dim(dtm_melter(dtm_c)),
        as.integer(c(7, 3))
    )
})

test_that("seq_builder", {
    out1 <- seq_builder(my_corpus, text, line_id)
    out2 <- seq_builder(my_corpus,
        text = text,
        doc_id = line_id,
        vocab = vocab_builder(my_corpus, text)
    )
    out3 <- seq_builder(my_corpus, text, line_id, maxlen = 5L)
    out4 <- seq_builder(my_corpus, text, line_id, matrix = FALSE)
    out5 <- seq_builder(my_corpus,
        text = text,
        doc_id = line_id,
        maxlen = 5L,
        vocab = vocab_builder(my_corpus, text)
    )
    out6 <- seq_builder(my_corpus, text)


    expect_identical(dim(out1), c(6L, 8L))
    expect_identical(dim(out3), c(6L, 5L))
    expect_identical(length(out4), 6L)
    expect_identical(length(out4$line1), 8L)
    expect_identical(dim(out5), c(6L, 5L))
    expect_identical(dim(out6), c(6L, 8L))

    expect_equal(
        length(attr(out1, "dic")), max(out1)
    )
    expect_equal(
        length(attr(out2, "dic")), max(out2)
    )
    expect_equal(
        length(attr(out3, "dic")), max(out3)
    )
    expect_equal(
        length(attr(out4, "dic")), max(unlist(out4))
    )
    expect_identical(out1, out2)
    expect_identical(
        rownames(out6),
        c(
            "doc_1", "doc_2", "doc_3",
            "doc_4", "doc_5", "doc_6"
        )
    )
})




