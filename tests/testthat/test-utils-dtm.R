test_that("dtm_stats returns expect output", {
    # simplified, base R dtm
    out <- dtm_stats(dtm_bse,
        richness = TRUE,
        distribution = TRUE,
        central = TRUE,
        character = TRUE,
        simplify = TRUE
    )
    expect_equal(dim(out), as.integer(c(1L, 26L)))
    expect_type(out, "list")
    expect_equal(unlist(out$n_docs), nrow(dtm_bse))
    expect_equal(unlist(out$n_types), ncol(dtm_bse))
    expect_equal(unlist(out$n_tokens), sum(dtm_bse))

    # not simplified, base R dtm
    out <- dtm_stats(dtm_bse)
    expect_equal(length(out), 5L)
    expect_equal(dim(out[[1]]), as.integer(c(5L, 2L)))
    expect_equal(dim(out[[2]]), as.integer(c(4L, 2L)))
    expect_equal(dim(out[[3]]), as.integer(c(10L, 2L)))
    expect_equal(dim(out[[4]]), as.integer(c(4L, 2L)))
    expect_equal(dim(out[[5]]), as.integer(c(3L, 2L)))
    expect_type(out, "list")
    expect_equal(as.character(out[[1]][1, 2]), as.character(nrow(dtm_bse)))
    expect_equal(as.character(out[[1]][3, 2]), as.character(ncol(dtm_bse)))
    expect_equal(as.character(out[[1]][4, 2]), as.character(sum(dtm_bse)))

    # simplified, dgCMatrix R dtm
    out <- dtm_stats(dtm_dgc,
        richness = TRUE,
        distribution = TRUE,
        central = TRUE,
        character = TRUE,
        simplify = TRUE
    )
    expect_equal(dim(out), as.integer(c(1L, 26L)))
    expect_type(out, "list")
    expect_equal(unlist(out$n_docs), nrow(dtm_bse))
    expect_equal(unlist(out$n_types), ncol(dtm_bse))
    expect_equal(unlist(out$n_tokens), sum(dtm_bse))

    # not simplified, dgCMatrix R dtm
    out <- dtm_stats(dtm_dgc)
    expect_equal(dim(out[[1]]), as.integer(c(5L, 2L)))
    expect_equal(dim(out[[2]]), as.integer(c(4L, 2L)))
    expect_equal(dim(out[[3]]), as.integer(c(10L, 2L)))
    expect_equal(dim(out[[4]]), as.integer(c(4L, 2L)))
    expect_equal(dim(out[[5]]), as.integer(c(3L, 2L)))
    expect_type(out, "list")
    expect_equal(as.character(out[[1]][1, 2]), as.character(nrow(dtm_bse)))
    expect_equal(as.character(out[[1]][3, 2]), as.character(ncol(dtm_bse)))
    expect_equal(as.character(out[[1]][4, 2]), as.character(sum(dtm_bse)))
})

test_that("dtm_stats returns basic output alone", {
    out <- dtm_stats(
        dtm = dtm_bse,
        richness = FALSE,
        distribution = FALSE,
        central = FALSE,
        character = FALSE,
        simplify = FALSE
    )
    expect_type(out, "list")
    expect_equal(length(out), as.integer(1L))
    expect_equal(dim(out[[1]]), as.integer(c(5L, 2L)))
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


test_that("dtm_builder error/message if last row is blank", {
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

    my_corpus$my_text[6] <- ""

    expect_error(
        expect_message(dtm_builder(my_corpus, my_text, line_id))
    )
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
        expect_message(
            dtm_builder(jfk_corpus, text, doc_ids,
                vocab = c(
                    "hear", "babies",
                    "world", "picklespit"
                )
            )
        )
    )
    expect_error(
        expect_message(
            dtm_builder(jfk_corpus, text, doc_ids,
                vocab = new.vocab, chunk = 5L
            )
        )
    )
    expect_error(
        expect_message(
            dtm_builder(jfk_corpus, text, doc_ids,
                vocab = new.vocab
            )
        )
    )
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
        expect_message(
            dtm_builder(my_corpus, my_text, doc_id = line_ids)
        )
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



# test_that(".remove_empty_rows works", {
#     dtm1 <- jfk_corpus |>
#         dtm_builder(text, doc_id, dense = FALSE)

#     dtm1 <- rbind(dtm1, rep(0, ncol(dtm1)))
#     rownames(dtm1)[nrow(dtm1)] <- "empty"


#     expect_message(
#         dtm0 <- .remove_empty_rows(dtm1),
#         "The following rows were empty and removed from the DTM: empty"
#     )

#     expect_equal(nrow(dtm1) - 1, nrow(dtm0))
#     expect_equal(ncol(dtm1), ncol(dtm0))

#     jfk_corpus0 <- rbind(
#         jfk_corpus,
#         data.frame(
#             text = "",
#             clean_text = "",
#             doc_id = "empty"
#         )
#     )

#     jfk_corpus0 |>
#         dtm_builder(text, doc_id,
#              dense = FALSE,
#              omit_empty = TRUE
#              )


# })
