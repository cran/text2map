test_that("dtm_stopper works with stop_list", {
    expect_identical(
        dim(dtm_stopper(dtm_dgc,
            stop_list = c("we", "moon")
        )),
        as.integer(c(10, 41))
    )
    expect_identical(
        dim(dtm_stopper(dtm_dgc,
            stop_list = c("We", "we", "moon"),
            ignore_case = FALSE
        )),
        as.integer(c(10, 41))
    )
    expect_identical(
        dim(dtm_stopper(dtm_dgc,
            stop_list = c("we", "moon"),
            ignore_case = FALSE
        )),
        as.integer(c(10, 42))
    )
})

test_that("dtm_stopper works with dense", {
    out1 <- dtm_stopper(dtm_dgc,
        stop_list = c("we", "moon"),
        ignore_case = FALSE,
        dense = TRUE
    )

    expect_identical(
        dim(out1),
        as.integer(c(10, 42))
    )

    expect_identical(class(out1), c("matrix", "array"))
})

test_that("dtm_stopper works with stop_termfreq", {
    expect_identical(
        dim(dtm_stopper(dtm_dgc, stop_termfreq = c(2L, 5L))),
        as.integer(c(10, 12))
    )
    expect_identical(
        dim(dtm_stopper(dtm_dgc, stop_termfreq = c(1L, 2L))),
        as.integer(c(10, 35))
    )
    expect_identical(
        dim(dtm_stopper(dtm_dgc, stop_termfreq = c(1, 2))),
        as.integer(c(10, 35))
    )
    expect_identical(
        dim(dtm_stopper(dtm_dgc, stop_termfreq = c(1, Inf))),
        as.integer(c(10, 44))
    )
})

test_that("dtm_stopper works with stop_termprop", {
    expect_identical(
        dim(dtm_stopper(dtm_dgc, stop_termprop = c(0.04, 0.99))),
        as.integer(c(10, 7))
    )
    expect_identical(
        dim(dtm_stopper(dtm_dgc, stop_termprop = c(0.01, 0.1))),
        as.integer(c(10, 43))
    )
    expect_identical(
        dim(dtm_stopper(dtm_dgc, stop_termprop = c(0.01, 0.06))),
        as.integer(c(10, 41))
    )
    expect_identical(
        dim(dtm_stopper(dtm_dgc, stop_termprop = c(Inf, 0.1))),
        as.integer(c(10, 44))
    )
    expect_identical(
        dim(dtm_stopper(dtm_dgc, stop_termprop = c(Inf, Inf))),
        as.integer(c(10, 44))
    )
})

test_that("dtm_stopper works with stop_docfreq", {
    expect_identical(
        dim(dtm_stopper(dtm_dgc, stop_docfreq = c(1L, 3L))),
        as.integer(c(10, 39))
    )
    expect_identical(
        dim(dtm_stopper(dtm_dgc, stop_docfreq = c(2L, 4L))),
        as.integer(c(10, 12))
    )
    expect_identical(
        dim(dtm_stopper(dtm_dgc, stop_docfreq = c(2L, Inf))),
        as.integer(c(10, 13))
    )
    expect_identical(
        dim(dtm_stopper(dtm_dgc, stop_docfreq = c(Inf, Inf))),
        as.integer(c(10, 44))
    )
})

test_that("dtm_stopper works with stop_docprop", {
    expect_identical(
        dim(dtm_stopper(dtm_dgc, stop_docprop = c(0.2, .98))),
        as.integer(c(10, 13))
    )
    expect_identical(
        dim(dtm_stopper(dtm_dgc, stop_docprop = c(0.1, 0.4))),
        as.integer(c(10, 43))
    )
})

test_that("dtm_stopper works with Inf", {
    # docprop
    expect_identical(
        dim(dtm_stopper(dtm_dgc, stop_docprop = c(0.2, Inf))),
        as.integer(c(10, 13))
    )
    expect_identical(
        dim(dtm_stopper(dtm_dgc, stop_docprop = c(Inf, 1.0))),
        as.integer(c(10, 44))
    )
    # docfreq
    expect_identical(
        dim(dtm_stopper(dtm_dgc, stop_docfreq = c(1L, 3L))),
        as.integer(c(10, 39))
    )
    # termfreq
    expect_identical(
        dim(dtm_stopper(dtm_dgc, stop_termfreq = c(Inf, 2))),
        as.integer(c(10, 35))
    )
    # termprop
    expect_identical(
        dim(dtm_stopper(dtm_dgc, stop_termprop = c(0.04, Inf))),
        as.integer(c(10, 7))
    )
})

test_that("dtm_stopper works with happax and null", {
    # add empty column to dtm
    mat <- as.matrix(rep(0, nrow(dtm_dgc)))
    colnames(mat) <- "empty"
    dtm_a <- cbind(dtm_dgc, mat)

    expect_identical(
        dim(dtm_stopper(dtm_a, stop_hapax = TRUE)),
        as.integer(c(10, 13))
    )

    # should remove the one null column
    expect_identical(
        dim(dtm_stopper(dtm_a, stop_null = TRUE)),
        as.integer(c(10, 44))
    )
})

test_that("dtm_stopper works with stoprank", {
    expect_identical(
        dim(dtm_stopper(dtm_dgc, stop_termrank = 2L)),
        as.integer(c(10, 42))
    )

    expect_identical(
        dim(dtm_stopper(dtm_dgc, stop_termrank = 5L)),
        as.integer(c(10, 39))
    )

    expect_identical(
        dim(dtm_stopper(dtm_dgc, stop_termrank = 20L)),
        as.integer(c(10, 24))
    )
})


test_that("dtm_stopper errors work", {
    expect_error(
        expect_message(dtm_stopper(dtm_dgc, stop_termfreq = c("picklespit")))
    )
    expect_error(
        expect_message(dtm_stopper(dtm_dgc, stop_docfreq = c("picklespit")))
    )
    expect_error(
        expect_message(dtm_stopper(dtm_dgc))
    )
    expect_error(
        expect_message(dtm_stopper(as.matrix(dtm_dgc)))
    )
    expect_error(
        expect_message(dtm_stopper(dtm_dgc, stop_termrank = "picklespit"))
    )
    expect_error(
        expect_message(dtm_stopper(dtm_dgc, stop_termfreq = "picklespit"))
    )
})


test_that("dtm_stopper omit_empty works", {
    expect_identical(
        dim(dtm_stopper(dtm_dgc, stop_list = "too", omit_empty = FALSE)),
        as.integer(c(10, 43))
    )

    suppressMessages(
        expect_identical(
            dim(dtm_stopper(dtm_dgc, stop_list = "too", omit_empty = TRUE)),
            as.integer(c(9, 43))
        )
    )
})
