test_that("CoCA returns expected structure", {
    classes <- suppressWarnings(CoCA(
        dtm = dtm_bse,
        wv = fake_word_vectors_coca,
        directions = sem_dirs,
        filter_sig = TRUE,
        filter_value = 0.05,
        zero_action = "drop"
    ))

    expect_s3_class(classes, "CoCA")
    expect_named(classes, c("membership", "modules", "cormat"))
    expect_true(is.list(classes$modules))
    expect_equal(ncol(classes$cormat), nrow(dtm_bse))
    expect_equal(
        ncol(classes$modules[[1]]$cormat),
        nrow(sem_dirs)
    )
})

test_that("CoCA with zero_action ownclass returns expected structure", {
    classes <- suppressWarnings(CoCA(
        dtm = dtm_bse,
        wv = fake_word_vectors_coca,
        directions = sem_dirs,
        filter_sig = TRUE,
        filter_value = 0.05,
        zero_action = "ownclass"
    ))

    expect_s3_class(classes, "CoCA")
    expect_named(classes, c("membership", "modules", "cormat"))
})

test_that("CoCA warns and disables filter_sig with exactly 2 directions", {
    msgs <- character()
    withCallingHandlers(
        classes <- CoCA(
            dtm = dtm_bse,
            wv = fake_word_vectors_coca,
            directions = sem_dirs[1:2, , drop = FALSE],
            filter_sig = TRUE,
            filter_value = 0.05,
            zero_action = "drop"
        ),
        warning = function(w) {
            msgs <<- c(msgs, conditionMessage(w))
            invokeRestart("muffleWarning")
        }
    )

    expect_true(any(grepl("N directions must be", msgs)))
    expect_false(any(grepl("NaNs produced", msgs)))
    expect_s3_class(classes, "CoCA")
})

test_that("CoCA errors with a clear message when only 1 direction is provided", {
    expect_error(
        CoCA(
            dtm = dtm_bse,
            wv = fake_word_vectors_coca,
            directions = sem_dirs[1, , drop = FALSE],
            zero_action = "drop"
        ),
        "at least 2"
    )
})

test_that("print.CoCA produces output", {
    classes <- suppressWarnings(CoCA(
        dtm = dtm_bse,
        wv = fake_word_vectors_coca,
        directions = sem_dirs,
        filter_sig = TRUE,
        filter_value = 0.05,
        zero_action = "drop"
    ))

    expect_output(print(classes), "CoCA found")
})

test_that("coca alias produces identical results to CoCA", {
    args <- list(
        dtm = dtm_bse,
        wv = fake_word_vectors_coca,
        directions = sem_dirs,
        filter_sig = FALSE,
        filter_value = 0.05,
        zero_action = "drop"
    )
    result_coca <- do.call(coca, args)
    result_CoCA <- do.call(CoCA, args)
    expect_identical(result_coca$membership, result_CoCA$membership)
})

test_that("cmdist alias produces identical results to CMDist", {
    result_cmdist <- cmdist(
        dtm = dtm_dgc,
        cv = sd_01,
        wv = fake_word_vectors_coca,
        scale = FALSE
    )
    result_CMDist <- CMDist(
        dtm = dtm_dgc,
        cv = sd_01,
        wv = fake_word_vectors_coca,
        scale = FALSE
    )
    expect_identical(result_cmdist, result_CMDist)
})

test_that("plot.CoCA errors when module is missing", {
    classes <- suppressWarnings(CoCA(
        dtm = dtm_bse,
        wv = fake_word_vectors_coca,
        directions = sem_dirs,
        filter_sig = TRUE,
        filter_value = 0.05,
        zero_action = "drop"
    ))

    expect_error(plot(classes), "module")
})

test_that("plot.CoCA gives a clear error when module is explicitly NULL", {
    classes <- suppressWarnings(CoCA(
        dtm = dtm_bse,
        wv = fake_word_vectors_coca,
        directions = sem_dirs,
        filter_sig = TRUE,
        filter_value = 0.05,
        zero_action = "drop"
    ))

    # explicitly passing NULL bypasses missing(module), so this must not
    # fall through to an internal indexing error
    expect_error(plot(classes, module = NULL), "module")
})

test_that("plot.CoCA gives a clear error when module is out of range", {
    classes <- suppressWarnings(CoCA(
        dtm = dtm_bse,
        wv = fake_word_vectors_coca,
        directions = sem_dirs,
        filter_sig = TRUE,
        filter_value = 0.05,
        zero_action = "drop"
    ))

    n_modules <- length(classes$modules)
    expect_error(plot(classes, module = n_modules + 99), "between")
})

test_that("coca alias works", {
    classes <- suppressWarnings(coca(
        dtm = dtm_bse,
        wv = fake_word_vectors_coca,
        directions = sem_dirs,
        filter_sig = TRUE,
        filter_value = 0.05,
        zero_action = "drop"
    ))

    expect_s3_class(classes, "CoCA")
})

test_that("cmdist alias works", {
    result <- cmdist(
        dtm = dtm_dgc,
        cv = sd_01,
        wv = fake_word_vectors_coca,
        scale = FALSE
    )

    expect_s3_class(result, "data.frame")
    expect_true("doc_id" %in% colnames(result))
})

test_that("plot.CoCA with module=1 returns igraph object", {
    classes <- suppressWarnings(CoCA(
        dtm = dtm_bse,
        wv = fake_word_vectors_coca,
        directions = sem_dirs,
        filter_sig = FALSE,
        zero_action = "drop"
    ))

    pdf(NULL)
    on.exit(dev.off())
    result <- plot(classes, module = 1)
    expect_s3_class(result, "igraph")
})

test_that("plot.CoCA works with its default seed when .Random.seed does not exist yet", {
    classes <- suppressWarnings(CoCA(
        dtm = dtm_bse,
        wv = fake_word_vectors_coca,
        directions = sem_dirs,
        filter_sig = FALSE,
        zero_action = "drop"
    ))

    had_seed <- exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
    old_seed <- if (had_seed) get(".Random.seed", envir = .GlobalEnv) else NULL
    on.exit({
        if (had_seed) {
            assign(".Random.seed", old_seed, envir = .GlobalEnv)
        } else if (exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)) {
            rm(".Random.seed", envir = .GlobalEnv)
        }
    }, add = TRUE)
    if (had_seed) rm(".Random.seed", envir = .GlobalEnv)

    pdf(NULL)
    on.exit(dev.off(), add = TRUE)
    expect_no_error(plot(classes, module = 1))
})

test_that("CoCA with zero_action=ownclass produces valid membership", {
    classes <- suppressWarnings(CoCA(
        dtm = dtm_bse,
        wv = fake_word_vectors_coca,
        directions = sem_dirs,
        filter_sig = TRUE,
        filter_value = 0.05,
        zero_action = "ownclass"
    ))

    expect_s3_class(classes, "CoCA")
    expect_true("membership" %in% names(classes))
    expect_true(length(classes$membership) == nrow(dtm_bse))
})