testthat::skip_on_ci()

test_that("check parallel and serial CMDist gives the same output", {

    testthat::skip_on_cran()

    # check that single **cw** outputs the same in serial and parallel
    paralleled <- CMDist(
        dtm=dtm.dgc,
        cw = cw,
        wv = fake_word_vectors,
        parallel = TRUE,
        sens_interval = FALSE)

    serialed <- CMDist(
        dtm=dtm.dgc,
        cw = cw,
        wv = fake_word_vectors,
        parallel = FALSE,
        sens_interval = FALSE)

    expect_identical(paralleled, serialed)

    # check that multiple **cw** outputs the same in serial and parallel
    paralleled <- CMDist(
        dtm=dtm.dgc,
        cw = cw.2,
        wv = fake_word_vectors,
        parallel = TRUE,
        sens_interval = FALSE)

    serialed <- CMDist(
        dtm=dtm.dgc,
        cw = cw.2,
        wv = fake_word_vectors,
        parallel = FALSE,
        sens_interval = FALSE)

    expect_identical(paralleled, serialed)

    # check that cw with **cv** outputs the same in serial and parallel
    paralleled <- CMDist(
        dtm=dtm.dgc,
        cw = cw,
        cv = get_centroid(anchor.solo.c, fake_word_vectors),
        wv = fake_word_vectors,
        parallel = TRUE)

    serialed <- CMDist(
        dtm=dtm.dgc,
        cw = cw,
        cv = get_centroid(anchor.solo.c, fake_word_vectors),
        wv = fake_word_vectors,
        parallel = FALSE)

    expect_identical(paralleled, serialed)

    # check that multiple **cw** and cv outputs the same in serial and parallel
    paralleled <- CMDist(
        dtm=dtm.dgc,
        cw = cw.2,
        cv = sd.01,
        wv = fake_word_vectors,
        parallel = TRUE,
        sens_interval = FALSE)

    serialed <- CMDist(
        dtm=dtm.dgc,
        cw = cw.2,
        cv = sd.01,
        wv = fake_word_vectors,
        parallel = FALSE,
        sens_interval = FALSE)

    expect_identical(paralleled, serialed)
})


test_that("compare parallel and serial sens_interval output", {

    testthat::skip_on_cran()

    # check main function works
    paralleled <- CMDist(
        dtm=dtm.dgc,
        cw = cw,
        wv = fake_word_vectors,
        parallel = TRUE, ###
        sens_interval = TRUE,
        n_iters = 10L,
        alpha = 1L,
        scale = FALSE)

    serialed <- CMDist(
        dtm=dtm.dgc,
        cw = cw,
        wv = fake_word_vectors,
        parallel = FALSE,
        sens_interval = TRUE,
        n_iters = 10L,
        alpha = 1L,
        scale = FALSE)

    expect_identical(paralleled$choose, serialed$choose)

})

test_that("check internal 'resampler' function works with parallel", {

    testthat::skip_on_cran()

    prep <- .prep_cmd_INPUT(dtm=dtm.dgc,
                            cw = cw.2,
                            cv = NULL,
                            wv = fake_word_vectors,
                            missing = "stop")


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

    sampList.parallel <- .parDist2(prep=prep,
                          threads=threads,
                          setup_timeout=setup_timeout,
                          sens_interval=TRUE,
                          n_iters=n_iters,
                          alpha=alpha)

    expect_identical(length(sampList.serial),
                     length(sampList.parallel))

    expect_identical(dim(sampList.serial[1]),
                     dim(sampList.parallel[1]))


    # check that internal works with cw and cv
    prep <- .prep_cmd_INPUT(dtm=dtm.dgc,
                            cw = cw,
                            cv = sd.01,
                            wv = fake_word_vectors,
                            missing = "stop")

    out <- .parDist2(prep=prep,
                     threads=2L,
                     setup_timeout = 120L,
                     sens_interval = TRUE,
                     n_iters = 10L,
                     alpha = 1)

    expect_equal(length(out), 10L)
    expect_equal(colnames(out[[1]]), c("choose", "rich_pole"))

})


test_that("check error if threads greater than available", {

    testthat::skip_on_cran()

    n_cores <- parallel::detectCores()
    n_cores <- n_cores + 1
    # more threads than DTM rows
    expect_error(paralleled <- CMDist(
        dtm=dtm.dgc,
        cw = cw,
        wv = fake_word_vectors,
        parallel = TRUE,
        threads = n_cores,
        sens_interval = FALSE))

    dtm.triple <- rbind(dtm.dgc, dtm.dgc, dtm.dgc)
    # more threads than available on computer
    expect_message(paralleled <- CMDist(
        dtm=dtm.triple,
        cw = cw,
        wv = fake_word_vectors,
        parallel = TRUE,
        threads = n_cores,
        setup_timeout = 120L,
        sens_interval = FALSE) )


})
