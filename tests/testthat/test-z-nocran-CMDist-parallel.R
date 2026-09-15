test_that("check parallel and serial CMDist gives the same output", {
    testthat::skip_on_cran()
    testthat::skip_if(parallel::detectCores() < 3,
        "Need at least 3 cores for parallel comparison tests")

    # check that single **cw** outputs the same in serial and parallel
    paralleled <- CMDist(
        dtm = dtm_dgc,
        cw = cw,
        wv = fake_word_vectors,
        parallel = TRUE,
        sens_interval = FALSE
    )

    serialed <- CMDist(
        dtm = dtm_dgc,
        cw = cw,
        wv = fake_word_vectors,
        parallel = FALSE,
        sens_interval = FALSE
    )

    expect_identical(paralleled, serialed)

    # check that multiple **cw** outputs the same in serial and parallel
    paralleled <- CMDist(
        dtm = dtm_dgc,
        cw = cw_2,
        wv = fake_word_vectors,
        parallel = TRUE,
        sens_interval = FALSE
    )

    serialed <- CMDist(
        dtm = dtm_dgc,
        cw = cw_2,
        wv = fake_word_vectors,
        parallel = FALSE,
        sens_interval = FALSE
    )

    expect_identical(paralleled, serialed)

    # check that cw with **cv** outputs the same in serial and parallel
    paralleled <- CMDist(
        dtm = dtm_dgc,
        cw = cw,
        cv = get_centroid(anchor_solo_c, fake_word_vectors),
        wv = fake_word_vectors,
        parallel = TRUE
    )

    serialed <- CMDist(
        dtm = dtm_dgc,
        cw = cw,
        cv = get_centroid(anchor_solo_c, fake_word_vectors),
        wv = fake_word_vectors,
        parallel = FALSE
    )

    expect_identical(paralleled, serialed)

    # check that multiple **cw** and cv outputs the same in serial and parallel
    paralleled <- CMDist(
        dtm = dtm_dgc,
        cw = cw_2,
        cv = sd_01,
        wv = fake_word_vectors,
        parallel = TRUE,
        sens_interval = FALSE
    )

    serialed <- CMDist(
        dtm = dtm_dgc,
        cw = cw_2,
        cv = sd_01,
        wv = fake_word_vectors,
        parallel = FALSE,
        sens_interval = FALSE
    )

    expect_identical(paralleled, serialed)
})


test_that("compare parallel and serial sens_interval output", {
    testthat::skip_on_cran()
    testthat::skip_if(parallel::detectCores() < 3,
        "Need at least 3 cores for parallel comparison tests")

    # check main function works
    paralleled <- CMDist(
        dtm = dtm_dgc,
        cw = cw,
        wv = fake_word_vectors,
        parallel = TRUE, ###
        sens_interval = TRUE,
        n_iters = 10L,
        alpha = 1L,
        scale = FALSE
    )

    serialed <- CMDist(
        dtm = dtm_dgc,
        cw = cw,
        wv = fake_word_vectors,
        parallel = FALSE,
        sens_interval = TRUE,
        n_iters = 10L,
        alpha = 1L,
        scale = FALSE
    )

    expect_identical(paralleled$choose, serialed$choose)
})

test_that("check internal 'resampler' function works with parallel", {
    testthat::skip_on_cran()
    testthat::skip_if(parallel::detectCores() < 3,
        "Need at least 3 cores for parallel comparison tests")

    prep <- .prep_cmd_INPUT(
        dtm = dtm_dgc,
        cw = cw_2,
        cv = NULL,
        wv = fake_word_vectors,
        missing = "stop"
    )


    # set variables
    n_iters <- 10L
    alpha <- 1L
    threads <- 2L
    setup_timeout <- 120L

    sampList.serial <- lapply(seq_len(n_iters), function(x) {
        sampDTM <- dtm_resampler(prep$DTM, alpha = alpha)
        sampDist <- text2vec::RWMD$new(sampDTM, prep$wem)$sim2(prep$pDTM)
        sampDist <- t(sampDist[seq_len(prep$n_pd), , drop = FALSE])

        return(sampDist)
    })

    sampList.parallel <- .parDist2(
        prep = prep,
        threads = threads,
        setup_timeout = setup_timeout,
        sens_interval = TRUE,
        n_iters = n_iters,
        alpha = alpha
    )

    expect_identical(
        length(sampList.serial),
        length(sampList.parallel)
    )

    expect_identical(
        dim(sampList.serial[1]),
        dim(sampList.parallel[1])
    )


    # check that internal works with cw and cv
    prep <- .prep_cmd_INPUT(
        dtm = dtm_dgc,
        cw = cw,
        cv = sd_01,
        wv = fake_word_vectors,
        missing = "stop"
    )

    out <- .parDist2(
        prep = prep,
        threads = 2L,
        setup_timeout = 120L,
        sens_interval = TRUE,
        n_iters = 10L,
        alpha = 1
    )

    expect_equal(length(out), 10L)
    expect_equal(colnames(out[[1]]), c("choose", "rich_pole"))
})


test_that("parallel sens_interval works when a thread's chunk is a single document", {
    testthat::skip_on_cran()

    # threads == nrow(dtm) forces .split_dtm() to hand each worker exactly
    # 1 row; prep$DTM[a:b, ] (no drop = FALSE) then collapsed that 1-row
    # dgCMatrix chunk to a bare numeric vector before passing it to
    # dtm_resampler(), which requires a matrix
    out <- CMDist(
        dtm = dtm_dgc[1:2, ],
        cw = cw,
        wv = fake_word_vectors,
        parallel = TRUE,
        threads = 2L,
        sens_interval = TRUE,
        n_iters = 3L
    )

    expect_s3_class(out, "data.frame")
    expect_equal(nrow(out), 2L)
})

test_that("check error if threads greater than DTM rows after reduction", {
    testthat::skip_on_cran()

    # .parDist2's reduction step is `threads <- min(max_cores, nrow(DTM))`,
    # which always caps threads at or below nrow(DTM) -- so the "still too
    # many threads after reduction" error can only fire when the *requested*
    # thread count already fits under max_cores (skipping reduction
    # entirely) while still exceeding a tiny DTM's row count. Requesting a
    # large thread count (e.g. 4L) only reliably hits this when max_cores
    # happens to be >= 4, which R CMD check's core-limiting environment
    # variables are not guaranteed to allow (they can cap max_cores to 2).
    # threads = 2L stays under max_cores in every realistic environment
    # while still exceeding this 1-row DTM, so it exercises the intended
    # path consistently.
    dtm_single <- dtm_dgc[1, , drop = FALSE]
    expect_error(paralleled <- CMDist(
        dtm = dtm_single,
        cw = cw,
        wv = fake_word_vectors,
        parallel = TRUE,
        threads = 2L,
        sens_interval = FALSE
    ))
})

test_that("check message when threads reduced to available cores", {
    testthat::skip_on_cran()

    # use a thread count greater than detectCores but less than DTM rows
    n_cores <- parallel::detectCores()
    n_threads <- n_cores + 2
    # ensure DTM has more rows than threads
    dtm_big <- dtm_dgc
    while (nrow(dtm_big) < n_threads) {
      dtm_big <- rbind(dtm_big, dtm_dgc)
    }
    # more threads than available on computer triggers auto-reduction message
    expect_message(paralleled <- CMDist(
        dtm = dtm_big,
        cw = cw,
        wv = fake_word_vectors,
        parallel = TRUE,
        threads = n_threads,
        setup_timeout = 120L,
        sens_interval = FALSE
    ))
})
