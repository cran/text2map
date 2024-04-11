test_that("sens_interval = TRUE works", {
    ## works with cw
    out_sens <- CMDist(
        dtm = dtm_dgc,
        cw = cw,
        cv = NULL,
        wv = fake_word_vectors,
        sens_interval = TRUE,
        alpha = 1,
        n_iters = 10L,
        missing = "remove"
    )


    expect_s3_class(out_sens, "data.frame")
    expect_type(out_sens[, 2], "double")
    expect_identical(out_sens$doc_id, rownames(dtm_dgc))
    expect_identical(ncol(out_sens), 4L)
    expect_identical(colnames(out_sens), c(
        "doc_id",
        "choose",
        "choose_upper",
        "choose_lower"
    ))

    ## works with multiple cw
    out_sens <- CMDist(
        dtm = dtm_dgc,
        cw = cw_2,
        cv = NULL,
        wv = fake_word_vectors,
        sens_interval = TRUE,
        alpha = 1,
        n_iters = 10L,
        missing = "remove"
    )

    expect_s3_class(out_sens, "data.frame")
    expect_type(out_sens[, 2], "double")
    expect_identical(out_sens$doc_id, rownames(dtm_dgc))
    expect_identical(ncol(out_sens), 7L)
    expect_identical(colnames(out_sens), c(
        "doc_id",
        "choose",
        "choose_upper",
        "choose_lower",
        "decade",
        "decade_upper",
        "decade_lower"
    ))

    ## works with multiple cw and cv
    out_sens <- CMDist(
        dtm = dtm_dgc,
        cw = cw_2,
        cv = sd_01,
        wv = fake_word_vectors,
        sens_interval = TRUE,
        alpha = 1,
        n_iters = 10L,
        missing = "remove"
    )

    expect_s3_class(out_sens, "data.frame")
    expect_type(out_sens[, 2], "double")
    expect_identical(out_sens$doc_id, rownames(dtm_dgc))
    expect_identical(ncol(out_sens), 10L)
    expect_identical(colnames(out_sens), c(
        "doc_id",
        "choose",
        "choose_upper",
        "choose_lower",
        "decade",
        "decade_upper",
        "decade_lower",
        "rich_pole",
        "rich_pole_upper",
        "rich_pole_lower"
    ))
})

test_that("check internal .get_sensitivity_intervals()", {
    ## prep for test
    prep <- .prep_cmd_INPUT(
        dtm = dtm_dgc,
        cw = cw,
        cv = NULL,
        wv = fake_word_vectors,
        missing = "stop"
    )

    fullDist <- text2vec::RWMD$new(prep$DTM, prep$wem)$sim2(prep$pDTM)
    fullDist <- t(fullDist[seq_len(prep$n_pd), , drop = FALSE])

    sampList <- lapply(seq_len(10L), function(x) {
        sampDTM <- dtm_resampler(prep$DTM, alpha = 1L)
        sampDist <- text2vec::RWMD$new(sampDTM, prep$wem)$sim2(prep$pDTM)
        sampDist <- t(sampDist[seq_len(prep$n_pd), , drop = FALSE])
        return(sampDist)
    })

    # actual test
    out.sen <- .get_sensitivity_intervals(sampList,
        fullDist,
        prep,
        probs = c(0.025, 0.975),
        type = 7,
        scale = FALSE
    )

    # expect_s3_class(out.sen, "data.frame")
    expect_type(out.sen[, 2], "double")
    expect_identical(out.sen$doc_id, rownames(dtm_dgc))
    expect_identical(ncol(out.sen), 4L)
    expect_identical(colnames(out.sen), c(
        "doc_id",
        "choose",
        "choose_upper",
        "choose_lower"
    ))


    ## prep for test with multiple cws ----------------------------------------
    prep <- .prep_cmd_INPUT(
        dtm = dtm_dgc,
        cw = cw_2,
        cv = NULL,
        wv = fake_word_vectors,
        missing = "stop"
    )

    fullDist <- text2vec::RWMD$new(prep$DTM, prep$wem)$sim2(prep$pDTM)
    fullDist <- t(fullDist[seq_len(prep$n_pd), , drop = FALSE])

    sampList <- lapply(seq_len(10L), function(x) {
        sampDTM <- dtm_resampler(prep$DTM, alpha = 1L)
        sampDist <- text2vec::RWMD$new(sampDTM, prep$wem)$sim2(prep$pDTM)
        sampDist <- t(sampDist[seq_len(prep$n_pd), , drop = FALSE])
        return(sampDist)
    })

    # actual test
    out.sen <- .get_sensitivity_intervals(sampList,
        fullDist,
        prep,
        probs = c(0.025, 0.975),
        type = 7,
        scale = FALSE
    )

    expect_s3_class(out.sen, "data.frame")
    expect_type(out.sen[, 2], "double")
    expect_identical(out.sen$doc_id, rownames(dtm_dgc))
    expect_identical(ncol(out.sen), 7L)
    expect_identical(colnames(out.sen), c(
        "doc_id",
        "choose",
        "choose_upper",
        "choose_lower",
        "decade",
        "decade_upper",
        "decade_lower"
    ))
})
