# tests for test_anchors.relco()

df_anchors_relco <- data.frame(
    a = c("rest", "rested", "stay", "stand"),
    z = c("coming", "embarked", "fast", "move")
)
non_anchors_relco <- c("writ", "alloys", "ills", "atlas", "saturn", "cape", "unfolds")

test_that("relco centroid returns expected structure", {
    result <- test_anchors(
        df_anchors_relco[, 1], ft_wv_sample,
        method = "relco", type = "centroid",
        non_anchors = non_anchors_relco, n_runs = 15, seed = 1701
    )

    expect_s3_class(result, "relco")
    expect_s3_class(result, "data.frame")
    expect_true(is.data.frame(result))
    expect_equal(ncol(result), 4L)
    expect_equal(colnames(result), c("term", "mean", "lower_ci", "upper_ci"))
    expect_equal(nrow(result), 4L)
    expect_true(is.character(result$term))
    expect_true(is.numeric(result$mean))
    expect_true(is.numeric(result$lower_ci))
    expect_true(is.numeric(result$upper_ci))
})

test_that("relco compound returns expected structure", {
    result <- test_anchors(
        df_anchors_relco$a, ft_wv_sample,
        method = "relco", type = "compound",
        non_anchors = non_anchors_relco, n_runs = 15, seed = 1701
    )

    expect_s3_class(result, "relco")
    expect_s3_class(result, "data.frame")
    expect_equal(ncol(result), 4L)
    expect_equal(colnames(result), c("term", "mean", "lower_ci", "upper_ci"))
    expect_equal(nrow(result), 4L)
})

test_that("relco direction paired returns expected structure", {
    result <- test_anchors(
        df_anchors_relco, ft_wv_sample,
        method = "relco", type = "direction", dir_method = "paired",
        non_anchors = non_anchors_relco, n_runs = 15, seed = 1701
    )

    expect_s3_class(result, "relco")
    expect_s3_class(result, "data.frame")
    expect_equal(ncol(result), 5L)
    expect_true("term" %in% colnames(result))
    expect_true("mean" %in% colnames(result))
    expect_true("lower_ci" %in% colnames(result))
    expect_true("upper_ci" %in% colnames(result))
    expect_true("pole" %in% colnames(result))
    expect_equal(nrow(result), 8L)
})

test_that("relco direction pooled returns expected structure", {
    result <- test_anchors(
        df_anchors_relco, ft_wv_sample,
        method = "relco", type = "direction", dir_method = "pooled",
        non_anchors = non_anchors_relco, n_runs = 15, seed = 1701
    )

    expect_s3_class(result, "relco")
    expect_s3_class(result, "data.frame")
    expect_equal(ncol(result), 5L)
    expect_equal(nrow(result), 8L)
})

test_that("relco direction L2 returns expected structure", {
    result <- test_anchors(
        df_anchors_relco, ft_wv_sample,
        method = "relco", type = "direction", dir_method = "L2",
        non_anchors = non_anchors_relco, n_runs = 15, seed = 1701
    )

    expect_s3_class(result, "relco")
    expect_s3_class(result, "data.frame")
    expect_equal(ncol(result), 5L)
    expect_equal(nrow(result), 8L)
})

test_that("relco attributes are attached correctly", {
    result <- test_anchors(
        df_anchors_relco[, 1], ft_wv_sample,
        method = "relco", type = "centroid",
        non_anchors = non_anchors_relco, n_runs = 15, seed = 1701
    )

    expect_true("relation_type" %in% names(attributes(result)))
    expect_equal(attr(result, "relation_type"), "centroid")

    expect_true("confidence_level" %in% names(attributes(result)))
    expect_equal(attr(result, "confidence_level"), 0.95)

    expect_true("null" %in% names(attributes(result)))
    expect_equal(attr(result, "null"), 0)

    expect_true("alpha" %in% names(attributes(result)))
    expect_equal(attr(result, "alpha"), 0.5)

    expect_true("seed" %in% names(attributes(result)))
    expect_equal(attr(result, "seed"), 1701)

    expect_true("n_runs" %in% names(attributes(result)))
    expect_equal(attr(result, "n_runs"), 15)

    expect_true("total_score" %in% names(attributes(result)))
    expect_true(is.numeric(attr(result, "total_score")))

    expect_true("total_intercept" %in% names(attributes(result)))
    expect_true(is.numeric(attr(result, "total_intercept")))

    expect_true("t_test" %in% names(attributes(result)))
    expect_s3_class(attr(result, "t_test"), "htest")

    expect_true("confidence_int" %in% names(attributes(result)))
    expect_true(is.numeric(attr(result, "confidence_int")))

    expect_true("term_runs" %in% names(attributes(result)))
    expect_true("run_scores" %in% names(attributes(result)))
    expect_true("raw_scores" %in% names(attributes(result)))
    expect_true("anchor_terms" %in% names(attributes(result)))

    expect_true("top_terms" %in% names(attributes(result)))
})

test_that("relco direction pole1 and pole2 anchor terms do not overlap", {
    n_runs <- 15
    result <- test_anchors(
        anchors = df_anchors_relco, 
        wv = ft_wv_sample,
        method = "relco", 
        type = "direction", 
        dir_method = "paired",
        non_anchors = non_anchors_relco, 
        n_runs = n_runs, 
        seed = 1701
    )

    anchor_terms <- attr(result, "anchor_terms")

    for (rdx in seq_len(n_runs)) {
        pole1 <- anchor_terms$pole1[[rdx]]
        pole2 <- anchor_terms$pole2[[rdx]]
        overlap <- intersect(pole1, pole2)
        expect_true(length(overlap) == 0,
            info = paste0("Run ", rdx, ": pole1 and pole2 share terms: ",
                          paste(overlap, collapse = ", ")))
    }
})

test_that("relco centroid accepts 1-column data.frame input", {
    df_single_col <- data.frame(a = c("rest", "rested", "stay", "stand"))
    result <- test_anchors(
        df_single_col, ft_wv_sample,
        method = "relco", type = "centroid",
        non_anchors = non_anchors_relco, n_runs = 15, seed = 1701
    )
    expect_s3_class(result, "relco")
    expect_equal(nrow(result), 4L)
    expect_equal(sort(result$term), sort(c("rest", "rested", "stay", "stand")))
})

test_that("relco centroid accepts list of one character vector", {
    list_single <- list(c("rest", "rested", "stay", "stand"))
    result <- test_anchors(
        list_single, ft_wv_sample,
        method = "relco", type = "centroid",
        non_anchors = non_anchors_relco, n_runs = 15, seed = 1701
    )
    expect_s3_class(result, "relco")
    expect_equal(nrow(result), 4L)
    expect_equal(sort(result$term), sort(c("rest", "rested", "stay", "stand")))
})

test_that("relco centroid errors on list of length > 1", {
    list_multi <- list(c("rest", "rested"), c("stay", "stand"))
    expect_error(
        test_anchors(
            list_multi, ft_wv_sample,
            method = "relco", type = "centroid",
            non_anchors = non_anchors_relco, n_runs = 15
        )
    )
})

test_that("relco centroid intercepts and non_anchor_terms attributes are not NULL", {
    result <- test_anchors(
        df_anchors_relco[, 1], ft_wv_sample,
        method = "relco", type = "centroid",
        non_anchors = non_anchors_relco, n_runs = 15, seed = 1701
    )
    expect_true("intercepts" %in% names(attributes(result)))
    expect_true(is.numeric(unlist(attr(result, "intercepts"))))
    expect_true("non_anchor_terms" %in% names(attributes(result)))
    expect_true(is.character(unlist(attr(result, "non_anchor_terms"))))
})

test_that("relco direction attributes include pole info", {
    result <- test_anchors(
        df_anchors_relco, ft_wv_sample,
        method = "relco", type = "direction", dir_method = "paired",
        non_anchors = non_anchors_relco, n_runs = 15, seed = 1701
    )

    expect_equal(attr(result, "relation_type"), "direction")
    expect_true("top_terms" %in% names(attributes(result)))
    expect_true("pole1" %in% names(attr(result, "top_terms")))
    expect_true("pole2" %in% names(attr(result, "top_terms")))
})

test_that("relco seed produces reproducible results", {
    result1 <- test_anchors(
        df_anchors_relco[, 1], ft_wv_sample,
        method = "relco", type = "centroid",
        non_anchors = non_anchors_relco, n_runs = 15, seed = 123
    )
    result2 <- test_anchors(
        df_anchors_relco[, 1], ft_wv_sample,
        method = "relco", type = "centroid",
        non_anchors = non_anchors_relco, n_runs = 15, seed = 123
    )

    expect_identical(result1$term, result2$term)
    expect_equal(result1$mean, result2$mean)
})

test_that("relco errors when non_anchors overlap anchors", {
    overlapping <- c("writ", "stand", "ills", "atlas", "saturn", "cape", "unfolds")
    expect_error(
        test_anchors(
            df_anchors_relco[, 1], ft_wv_sample,
            method = "relco", type = "centroid",
            non_anchors = overlapping, n_runs = 15
        )
    )
})

test_that("relco errors when non_anchors is too short", {
    too_short <- c("writ")
    expect_error(
        test_anchors(
            df_anchors_relco[, 1], ft_wv_sample,
            method = "relco", type = "centroid",
            non_anchors = too_short, n_runs = 15
        ),
        "must have a length"
    )
})

test_that("relco errors when anchors has too many columns for centroid", {
    df_error <- cbind(df_anchors_relco, df_anchors_relco)
    expect_error(
        test_anchors(
            df_error, ft_wv_sample,
            method = "relco", type = "centroid",
            non_anchors = non_anchors_relco, n_runs = 15
        ),
        "ncol"
    )
})

test_that("relco errors when anchors has wrong columns for direction", {
    vec_only <- df_anchors_relco[, 1]
    expect_error(
        test_anchors(
            vec_only, ft_wv_sample,
            method = "relco", type = "direction",
            non_anchors = non_anchors_relco, n_runs = 15
        )
    )
})

test_that("relco errors when non_anchors has duplicates", {
    dup_na <- c("writ", "writ", "ills", "atlas", "saturn", "cape", "unfolds")
    expect_error(
        test_anchors(
            df_anchors_relco[, 1], ft_wv_sample,
            method = "relco", type = "centroid",
            non_anchors = dup_na, n_runs = 15
        ),
        "duplicated"
    )
})

test_that("relco order_non_anchors produces results", {
    result <- test_anchors(
        df_anchors_relco[, 1], ft_wv_sample,
        method = "relco", type = "centroid",
        non_anchors = non_anchors_relco,
        n_runs = 15, seed = 1701, order_non_anchors = TRUE
    )

    expect_s3_class(result, "relco")
    expect_s3_class(result, "data.frame")
    expect_equal(ncol(result), 4L)
    expect_equal(nrow(result), 4L)
})

test_that("relco t_test attribute has expected structure", {
    result <- test_anchors(
        df_anchors_relco[, 1], ft_wv_sample,
        method = "relco", type = "centroid",
        non_anchors = non_anchors_relco, n_runs = 15, seed = 1701
    )

    tt <- attr(result, "t_test")
    expect_s3_class(tt, "htest")
    expect_true("statistic" %in% names(tt))
    expect_true("parameter" %in% names(tt))
    expect_true("p.value" %in% names(tt))
    expect_true("alternative" %in% names(tt))
})

test_that("relco sample() bug with length-1 non_anchors does not produce invalid terms", {
    minimal_anchors <- c("rest", "stay", "stand")
    non_anchors_minimal <- c("writ", "ills", "atlas", "saturn", "cape", "unfolds")

    result <- test_anchors(
        minimal_anchors, ft_wv_sample,
        method = "relco", type = "centroid",
        non_anchors = non_anchors_minimal, n_runs = 15, seed = 1701
    )

    all_valid_terms <- c(minimal_anchors, non_anchors_minimal)

    anchor_terms <- attr(result, "anchor_terms")
    for (i in seq_along(anchor_terms)) {
        expect_true(all(anchor_terms[[i]] %in% all_valid_terms),
                    info = paste("anchor_terms run", i, "contains invalid terms:",
                                 paste(setdiff(anchor_terms[[i]], all_valid_terms), collapse = ", ")))
    }

    non_anchor_terms <- attr(result, "non_anchor_terms")
    expect_true(all(non_anchor_terms %in% all_valid_terms),
                info = paste("non_anchor_terms contains invalid terms:",
                             paste(setdiff(non_anchor_terms, all_valid_terms), collapse = ", ")))
})

test_that("relco non_anchor_terms attribute has same list-per-run structure as anchor_terms", {
    n_runs <- 15
    result <- test_anchors(
        anchors = df_anchors_relco[, 1], ft_wv_sample,
        method = "relco", type = "centroid",
        non_anchors = non_anchors_relco, n_runs = n_runs, seed = 1701
    )

    anchor_terms <- attr(result, "anchor_terms")
    non_anchor_terms <- attr(result, "non_anchor_terms")

    expect_type(anchor_terms, "list")
    expect_length(anchor_terms, n_runs)

    expect_true(is.list(non_anchor_terms),
                info = "non_anchor_terms should be a list (one element per run), not a single vector")
    expect_length(non_anchor_terms, n_runs)
    if (is.list(non_anchor_terms) && length(non_anchor_terms) == n_runs) {
        for (i in seq_len(n_runs)) {
            expect_true(all(non_anchor_terms[[i]] %in% non_anchors_relco),
                        info = paste("non_anchor_terms run", i, "contains invalid terms"))
        }
    }
})


test_that("relco terms match input anchors", {
    result <- test_anchors(
        df_anchors_relco[, 1], ft_wv_sample,
        method = "relco", type = "centroid",
        non_anchors = non_anchors_relco, n_runs = 15, seed = 1701
    )

    expect_true(all(result$term %in% df_anchors_relco[, 1]))
    expect_equal(sort(result$term), sort(df_anchors_relco[, 1]))
})

test_that("relco errors on invalid type", {
    expect_error(
        test_anchors(
            df_anchors_relco[, 1], ft_wv_sample,
            method = "relco", type = "centroidXX",
            non_anchors = non_anchors_relco, n_runs = 5
        ),
        class = "simpleError"
    )
})

test_that("relco errors on invalid dir_method", {
    expect_error(
        test_anchors(
            df_anchors_relco, ft_wv_sample,
            method = "relco", type = "direction", dir_method = "pairedXX",
            non_anchors = non_anchors_relco, n_runs = 5
        ),
        class = "simpleError"
    )
})

test_that("test_anchors errors on invalid method", {
    expect_error(
        test_anchors(
            df_anchors_relco[, 1], ft_wv_sample,
            method = "invalid",
            non_anchors = non_anchors_relco, n_runs = 5
        ),
        class = "simpleError"
    )
})

test_that("relco warns and coerces n_runs < 5 to 5", {
    warnings <- character(0)
    withCallingHandlers(
        result <- test_anchors(
            df_anchors_relco[, 1], ft_wv_sample,
            method = "relco", type = "centroid",
            non_anchors = non_anchors_relco, n_runs = 1, seed = 1701
        ),
        warning = function(w) {
            warnings <<- c(warnings, conditionMessage(w))
            invokeRestart("muffleWarning")
        }
    )
    expect_true(any(grepl("n_runs.*5", warnings)),
        info = "Expected n_runs warning about coercing to 5")
    expect_s3_class(result, "relco")
    expect_equal(attr(result, "n_runs"), 5)
})

test_that("relco warns and coerces n_runs = 4 to 5", {
    warnings <- character(0)
    withCallingHandlers(
        result <- test_anchors(
            df_anchors_relco[, 1], ft_wv_sample,
            method = "relco", type = "centroid",
            non_anchors = non_anchors_relco, n_runs = 4, seed = 1701
        ),
        warning = function(w) {
            warnings <<- c(warnings, conditionMessage(w))
            invokeRestart("muffleWarning")
        }
    )
    expect_true(any(grepl("n_runs.*5", warnings)),
        info = "Expected n_runs warning about coercing to 5")
    expect_equal(attr(result, "n_runs"), 5)
})

test_that("relco does not warn about n_runs when n_runs >= 5", {
    expect_no_warning(
        test_anchors(
            df_anchors_relco[, 1], ft_wv_sample,
            method = "relco", type = "centroid",
            non_anchors = non_anchors_relco, n_runs = 15, seed = 1701
        ),
        message = "n_runs"
    )
})

test_that("relco with coerced n_runs produces no NaN in confidence intervals", {
    suppressWarnings(
        result <- test_anchors(
            df_anchors_relco[, 1], ft_wv_sample,
            method = "relco", type = "centroid",
            non_anchors = non_anchors_relco, n_runs = 1, seed = 1701
        )
    )
    expect_true(!any(is.nan(result$lower_ci)),
        info = "lower_ci should not contain NaN with coerced n_runs")
    expect_true(!any(is.nan(result$upper_ci)),
        info = "upper_ci should not contain NaN with coerced n_runs")
    ci_global <- attr(result, "confidence_int")
    expect_true(!any(is.nan(ci_global)),
        info = "global confidence_int should not contain NaN with coerced n_runs")
})

test_that("relco term CIs use NA (not NaN) when term has too few observations", {
    suppressWarnings(
        result <- test_anchors(
            df_anchors_relco[, 1], ft_wv_sample,
            method = "relco", type = "centroid",
            non_anchors = non_anchors_relco, n_runs = 15, seed = 1701
        )
    )
    expect_true(all(is.na(result$lower_ci) | is.finite(result$lower_ci)),
        info = "lower_ci should be NA or finite, never NaN")
    expect_true(all(is.na(result$upper_ci) | is.finite(result$upper_ci)),
        info = "upper_ci should be NA or finite, never NaN")
})

test_that("relco with coerced n_runs produces valid t-test", {
    suppressWarnings(
        result <- test_anchors(
            df_anchors_relco[, 1], ft_wv_sample,
            method = "relco", type = "centroid",
            non_anchors = non_anchors_relco, n_runs = 1, seed = 1701
        )
    )
    tt <- attr(result, "t_test")
    expect_true(is.finite(tt$statistic),
        info = "t-test statistic should be finite with coerced n_runs")
    expect_true(is.finite(tt$p.value),
        info = "t-test p-value should be finite with coerced n_runs")
})

test_that("relco confidence_int has correct named elements", {
    result <- test_anchors(
        df_anchors_relco[, 1], ft_wv_sample,
        method = "relco", type = "centroid",
        non_anchors = non_anchors_relco, n_runs = 15, seed = 1701
    )
    ci <- attr(result, "confidence_int")
    expect_true("mean" %in% names(ci))
    expect_true("lower_ci" %in% names(ci))
    expect_true("upper_ci" %in% names(ci))
})

test_that("relco confidence_int lower_ci <= mean <= upper_ci", {
    result <- test_anchors(
        df_anchors_relco[, 1], ft_wv_sample,
        method = "relco", type = "centroid",
        non_anchors = non_anchors_relco, n_runs = 15, seed = 1701
    )
    ci <- attr(result, "confidence_int")
    expect_true(ci["lower_ci"] <= ci["mean"],
        info = "lower_ci should be <= mean")
    expect_true(ci["mean"] <= ci["upper_ci"],
        info = "mean should be <= upper_ci")
})

test_that("relco direction confidence_int lower_ci <= mean <= upper_ci", {
    result <- test_anchors(
        df_anchors_relco, ft_wv_sample,
        method = "relco", type = "direction", dir_method = "paired",
        non_anchors = non_anchors_relco, n_runs = 15, seed = 1701
    )
    ci <- attr(result, "confidence_int")
    expect_true(ci["lower_ci"] <= ci["mean"],
        info = "lower_ci should be <= mean (direction)")
    expect_true(ci["mean"] <= ci["upper_ci"],
        info = "mean should be <= upper_ci (direction)")
})

test_that("relco .conf_int returns named vector", {
    ci <- text2map:::.conf_int(c(1, 2, 3, 4, 5), ci = 0.95)
    expect_equal(names(ci), c("mean", "lower_ci", "upper_ci"))
    expect_true(ci["lower_ci"] <= ci["mean"])
    expect_true(ci["mean"] <= ci["upper_ci"])
})

test_that("relco centroid errors when anchors has fewer than 3 terms", {
    expect_error(
        test_anchors(
            c("rest", "stay"), ft_wv_sample,
            method = "relco", type = "centroid",
            non_anchors = non_anchors_relco, n_runs = 15
        ),
        "at least 3"
    )
})

test_that("relco centroid errors when anchors has 1 term", {
    expect_error(
        test_anchors(
            c("rest"), ft_wv_sample,
            method = "relco", type = "centroid",
            non_anchors = non_anchors_relco, n_runs = 15
        ),
        "at least 3"
    )
})

test_that("relco compound errors when anchors has fewer than 3 terms", {
    expect_error(
        test_anchors(
            c("rest", "stay"), ft_wv_sample,
            method = "relco", type = "compound",
            non_anchors = non_anchors_relco, n_runs = 15
        ),
        "at least 3"
    )
})

test_that("relco direction errors when a pole has fewer than 3 terms", {
    df_short <- data.frame(
        a = c("rest", "stay"),
        z = c("coming", "move")
    )
    expect_error(
        test_anchors(
            df_short, ft_wv_sample,
            method = "relco", type = "direction", dir_method = "paired",
            non_anchors = non_anchors_relco, n_runs = 15
        ),
        "at least 3"
    )
})

test_that("relco direction errors when one pole has 1 term", {
    expect_error(
        test_anchors(
            list(c("rest"), c("coming", "move", "fast")), ft_wv_sample,
            method = "relco", type = "direction", dir_method = "paired",
            non_anchors = non_anchors_relco, n_runs = 15
        ),
        "at least 3"
    )
})

test_that("relco centroid accepts exactly 3 terms", {
    result <- test_anchors(
        c("rest", "stay", "stand"), ft_wv_sample,
        method = "relco", type = "centroid",
        non_anchors = non_anchors_relco, n_runs = 15, seed = 1701
    )
    expect_s3_class(result, "relco")
    expect_equal(nrow(result), 3L)
})

test_that("relco works with an explicit seed when .Random.seed does not exist yet", {
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

    expect_no_error(
        test_anchors(
            df_anchors_relco[, 1], ft_wv_sample,
            method = "relco", type = "centroid",
            non_anchors = non_anchors_relco, n_runs = 15, seed = 1701
        )
    )
})

