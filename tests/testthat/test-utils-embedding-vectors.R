test_that("get_anchor returns data object", {
    gend <- get_anchors(relation = "gender")
    expect_equal(ncol(gend), 2L)
})


test_that("test_anchor, pairdir", {

    df_anchors <- data.frame(
        a = c("rest", "rested", "stay", "stand"),
        z = c("coming", "embarked", "fast", "move")
    )

    test_anch1 <- test_anchors(df_anchors, ft_wv_sample, method = "pairdir")
    test_anch2 <- test_anchors(df_anchors, ft_wv_sample, method = "pairdir", all = TRUE)

    expect_type(test_anch1, "list")
    expect_s3_class(test_anch1, "data.frame")
    expect_identical(dim(test_anch1), c(5L, 2L))

    expect_type(test_anch2, "list")
    expect_s3_class(test_anch2, "data.frame")
    expect_identical(dim(test_anch2), c(17L, 2L))

})

test_that("test_anchors pairdir errors with a clear message with only 1 anchor pair", {
    df_anchors <- data.frame(a = "poor", z = "rich")

    expect_error(
        test_anchors(df_anchors, fake_word_vectors_coca, method = "pairdir"),
        "at least 2"
    )
})



test_that("test_anchor, pairdir", {

    df_anchors <- data.frame(
        a = c("rest", "rested", "stay", "stand"),
        z = c("coming", "embarked", "fast", "move")
    )
    non_anchors <- c("writ", "alloys", "ills", "atlas", "saturn", "cape", "unfolds")

    test_anch1 <- test_anchors(df_anchors[, 1], ft_wv_sample, method = "relco", 
                               type = "centroid", non_anchors = non_anchors)
    test_anch2 <- test_anchors(df_anchors$a, ft_wv_sample, method = "relco", 
                               type = "compound", non_anchors = non_anchors)
    test_anch3 <- test_anchors(df_anchors, ft_wv_sample, method = "relco", 
                               type = "direction", dir_method = "paired", 
                               non_anchors = non_anchors)
    test_anch4 <- test_anchors(df_anchors, ft_wv_sample, method = "relco", 
                               type = "direction", dir_method = "pooled", 
                               non_anchors = non_anchors, seed = 18015)
    test_anch5 <- test_anchors(df_anchors, ft_wv_sample, method = "relco", 
                               type = "direction", dir_method = "pooled", 
                               non_anchors = non_anchors, seed = 18015, )
    expect_type(test_anch1, "list")
    expect_s3_class(test_anch1, "data.frame")
    expect_identical(dim(test_anch1), c(4L, 4L))

    expect_type(test_anch2, "list")
    expect_s3_class(test_anch2, "data.frame")
    expect_identical(dim(test_anch2), c(4L, 4L))

    expect_type(test_anch3, "list")
    expect_s3_class(test_anch3, "data.frame")
    expect_identical(dim(test_anch3), c(8L, 5L))

    expect_type(test_anch4, "list")
    expect_s3_class(test_anch4, "data.frame")
    expect_identical(dim(test_anch4), c(8L, 5L))

    expect_no_error(capture_output(print(test_anch1)))
    expect_no_error(capture_output(print(test_anch3)))

    # printing a relco object must dispatch to the package's custom
    # tbl_sum/tbl_format_footer methods (registered as S3 methods for
    # pillar's generics), not fall back to the generic tibble header
    out1 <- capture_output(print(test_anch1))
    expect_true(any(grepl("Relation Type", out1, fixed = TRUE)))
    expect_true(any(grepl("Global Reliability Coefficient", out1, fixed = TRUE)))
    expect_false(any(grepl("A data frame", out1, fixed = TRUE)))

    out3 <- capture_output(print(test_anch3))
    expect_true(any(grepl("Highest Contributors (Pole 1)", out3, fixed = TRUE)))
    expect_true(any(grepl("Highest Contributors (Pole 2)", out3, fixed = TRUE)))

    df_error <- cbind(df_anchors, df_anchors)
    non_anchors_errors1 <- 1:4
    non_anchors_errors2 <- c(non_anchors, "stand")

    expect_error(test_anchors(df_error, ft_wv_sample, method = "relco", 
                              type = "centroid", non_anchors = non_anchors))
    expect_error(test_anchors(df_error, ft_wv_sample, method = "relco", 
                              type = "direction", non_anchors = non_anchors))
    expect_error(test_anchors(df_anchors, ft_wv_sample, method = "relco", 
                              type = "direction", non_anchors = non_anchors_errors1))
    expect_error(test_anchors(df_anchors, ft_wv_sample, method = "relco", 
                              type = "direction", non_anchors = non_anchors_errors2))
    
    
    
})

test_that("get_centroid works on different data types", {
    ## single c() list of terms ##
    out.c <- get_centroid(
        anchors = anchor_solo_c,
        wv = fake_word_vectors
    )

    expect_type(out.c, "double")
    expect_identical(dim(out.c), ce_dims)
    expect_identical(rownames(out.c), ce_name)

    ## order of single c() list of terms doesn't matter ##
    out.d <- get_centroid(
        anchors = anchor_solo_d,
        wv = fake_word_vectors
    )

    expect_identical(out.c[1], out.d[1])

    # actual list list
    out <- get_centroid(
        anchors = anchor_solo_list,
        wv = fake_word_vectors
    )

    expect_type(out, "double")
    expect_identical(dim(out), ce_dims)
    expect_identical(rownames(out), ce_name)
    expect_identical(out.c[1], out[1])

    # data.frame
    out <- get_centroid(
        anchors = anchor_solo_df,
        wv = fake_word_vectors
    )
    expect_type(out, "double")
    expect_identical(dim(out), ce_dims)
    expect_identical(rownames(out), ce_name)
    expect_identical(out.c[1], out[1])

    # tibble
    out <- get_centroid(
        anchors = anchor_solo_tbl,
        wv = fake_word_vectors
    )

    expect_type(out, "double")
    expect_identical(dim(out), ce_dims)
    expect_identical(rownames(out), ce_name)
    expect_identical(out.c[1], out[1])

    ## paired list of terms ##
    ## get_centroid should only use first lsit

    # actual list list
    out <- get_centroid(
        anchors = anchor_pair_list,
        wv = fake_word_vectors
    )
    expect_type(out, "double")
    expect_identical(dim(out), ce_dims)
    expect_identical(rownames(out), ce_name)
    expect_identical(out.c[1], out[1])

    # data.frame
    out <- get_centroid(
        anchors = anchor_pair_df,
        wv = fake_word_vectors
    )
    expect_type(out, "double")
    expect_identical(dim(out), ce_dims)
    expect_identical(rownames(out), ce_name)
    expect_identical(out.c[1], out[1])

    # tibble
    out <- get_centroid(
        anchors = anchor_pair_tbl,
        wv = fake_word_vectors
    )
    expect_type(out, "double")
    expect_identical(dim(out), ce_dims)
    expect_identical(rownames(out), ce_name)
    expect_identical(out.c[1], out[1])
})


test_that("get_direction works on different data types", {
    ## paired list of terms ##
    # actual list list
    out <- get_direction(
        anchors = anchor_pair_list,
        wv = fake_word_vectors
    )
    expect_type(out, "double")
    expect_identical(dim(out), di_dims)
    expect_identical(rownames(out), di_name)

    # data.frame
    out <- get_direction(
        anchors = anchor_pair_df,
        wv = fake_word_vectors
    )
    expect_type(out, "double")
    expect_identical(dim(out), di_dims)
    expect_identical(rownames(out), di_name)

    # tibble
    out <- get_direction(
        anchors = anchor_pair_tbl,
        wv = fake_word_vectors
    )
    expect_type(out, "double")
    expect_identical(dim(out), di_dims)
    expect_identical(rownames(out), di_name)
})

test_that("get_direction errors if only one set of terms", {
    out <- get_direction(
        anchors = anchor_pair_list,
        wv = fake_word_vectors,
        method = "paired"
    )
    expect_type(out, "double")
    expect_identical(dim(out), di_dims)
    expect_identical(rownames(out), di_name)

    out <- get_direction(
        anchors = anchor_pair_list,
        wv = fake_word_vectors,
        method = "pooled"
    )
    expect_type(out, "double")
    expect_identical(dim(out), di_dims)
    expect_identical(rownames(out), di_name)

    out <- get_direction(
        anchors = anchor_pair_list,
        wv = fake_word_vectors,
        method = "L2"
    )
    expect_type(out, "double")
    expect_identical(dim(out), di_dims)
    expect_identical(rownames(out), di_name)

    out <- get_direction(
        anchors = anchor_pair_list,
        wv = fake_word_vectors,
        method = "PCA"
    )
    expect_type(out, "double")
    expect_identical(dim(out), di_dims)
    expect_identical(rownames(out), di_name)

    expect_error(
        get_direction(
            anchors = anchor_pair_list,
            wv = fake_word_vectors,
            method = "not_a_method"
        )
    )
})

test_that("get_direction gives a clear error when n_dirs exceeds available anchor pairs", {
    # anchor_pair_list has only 2 anchor pairs, so at most 2 PCA directions
    # can be computed; requesting more must not fall through to an
    # unrelated dimnames-assignment error
    expect_error(
        get_direction(
            anchors = anchor_pair_list,
            wv = fake_word_vectors,
            method = "PCA",
            n_dirs = 3
        ),
        "n_dirs"
    )
})

test_that("get_direction PCA gives a clear error with only 1 anchor pair", {
    # with 1 anchor pair, prcomp() centers a single row against itself,
    # degenerating to sdev = 0 and an arbitrary rotation vector -- silent
    # nonsense rather than a real principal direction
    df_anchors <- data.frame(a = "rich", z = "poor")

    expect_error(
        get_direction(
            anchors = df_anchors,
            wv = fake_word_vectors_coca,
            method = "PCA"
        ),
        "at least 2"
    )
})

test_that("check if different get_direction methods work", {
    expect_error(
        get_direction(
            anchors = anchor_solo_list,
            wv = fake_word_vectors
        ),
        "get_direction requires two sets of juxtaposing terms"
    )

    expect_error(
        get_direction(
            anchors = anchor_solo_df,
            wv = fake_word_vectors
        ),
        "get_direction requires two sets of juxtaposing terms"
    )

    expect_error(
        get_direction(
            anchors = anchor_solo_tbl,
            wv = fake_word_vectors
        ),
        "get_direction requires two sets of juxtaposing terms"
    )
})


test_that("get_regions seed works", {
    skip_if_not_installed("ClusterR")
    my.regions <- get_regions(
        wv = fake_word_vectors,
        k_regions = 20L,
        max_iter = 10L,
        seed = 442
    )

    my.regions2 <- get_regions(
        wv = fake_word_vectors,
        k_regions = 20L,
        max_iter = 10L,
        seed = 442
    )

    expect_identical(my.regions, my.regions2)
})

test_that("get_direction with missing='remove' drops OOV terms", {
    anchors_oov <- data.frame(
        a = c("choose", "victory"),
        z = c("moon", "back")
    )

    out <- suppressMessages(get_direction(
        anchors = anchors_oov,
        wv = fake_word_vectors,
        missing = "remove"
    ))

    expect_type(out, "double")
    expect_identical(nrow(out), 1L)
    expect_identical(ncol(out), as.integer(ncol(fake_word_vectors)))
})

test_that("get_anchors returns empty tibble for invalid relation", {
    result <- get_anchors(relation = "nonexistent_relation")
    expect_s3_class(result, "tbl_df")
    expect_equal(nrow(result), 0L)
    expect_equal(ncol(result), 2L)
})

test_that("get_anchors returns two columns for valid relation", {
    gend <- get_anchors(relation = "gender")
    expect_equal(ncol(gend), 2L)
    expect_true(nrow(gend) > 0L)
})


## get_centroids tests ##

test_that("get_centroids works with named list of anchors (word-vector mode)", {
    anchors <- list(
        space = c("choose", "moon"),
        water = c("decade", "this")
    )
    out <- get_centroids(fake_word_vectors, anchors = anchors)
    expect_type(out, "double")
    expect_identical(dim(out), c(2L, as.integer(ncol(fake_word_vectors))))
    expect_identical(rownames(out), c("space", "water"))
    expect_equal(out["space", ], as.vector(colMeans(
        fake_word_vectors[c("choose", "moon"), , drop = FALSE]
    )))
})

test_that("get_centroids groups branch returns matrix with levels as rownames", {
    mat <- fake_word_vectors[1:10, , drop = FALSE]
    labs <- rep(c("a", "b"), each = 5)
    out <- get_centroids(mat, groups = labs)
    expect_type(out, "double")
    expect_identical(dim(out), c(2L, as.integer(ncol(mat))))
    expect_identical(rownames(out), c("a", "b"))
    expect_equal(out["a", ], as.vector(colMeans(mat[1:5, , drop = FALSE])))
})

test_that("get_centroids groups branch works with dgCMatrix sparse input", {
    mat <- methods::as(fake_word_vectors[1:10, , drop = FALSE], "dgCMatrix")
    labs <- rep(c("a", "b"), each = 5)
    out <- get_centroids(mat, groups = labs)
    ## vapply with numeric() returns a base matrix, even for sparse input
    expect_type(out, "double")
    expect_identical(dim(out), c(2L, as.integer(ncol(mat))))
    expect_identical(rownames(out), c("a", "b"))
    expect_equal(out["a", ], as.vector(Matrix::colMeans(mat[1:5, , drop = FALSE])))
})

test_that("get_centroids errors on groups length mismatch", {
    mat <- fake_word_vectors[1:10, , drop = FALSE]
    expect_error(
        get_centroids(mat, groups = c("a", "b")),
        class = "rlang_error"
    )
})

test_that("get_centroids errors when fv contains NAs", {
    mat <- fake_word_vectors[1:10, , drop = FALSE]
    mat[1, 1] <- NA
    labs <- rep(c("a", "b"), each = 5)
    expect_error(
        get_centroids(mat, groups = labs),
        class = "rlang_error"
    )
})

test_that("get_centroids errors when neither anchors nor groups provided", {
    expect_error(
        get_centroids(fake_word_vectors),
        class = "rlang_error"
    )
})

test_that("get_centroids uses integer indices for unnamed list anchors", {
    anchors <- list(c("choose", "moon"), c("decade", "this"))
    out <- get_centroids(fake_word_vectors, anchors = anchors)
    expect_identical(rownames(out), c("1", "2"))
})

test_that("get_centroids passes missing argument through in list branch", {
    anchors <- list(
        keep = c("choose", "moon"),
        with_oov = c("choose", "picklespit", "moon")
    )
    # get_centroid() used to do `anchors[] <- .check_term_in_embeddings(...)`,
    # which recycles instead of shrinking when action = "remove" drops a term
    # -- must not warn now that it's a plain reassignment
    out <- expect_no_warning(
        suppressMessages(get_centroids(fake_word_vectors, anchors = anchors, missing = "remove"))
    )
    expect_identical(dim(out), c(2L, as.integer(ncol(fake_word_vectors))))
    ## the with_oov centroid should match a direct get_centroid call with
    ## missing = "remove" (existing OOV-handling behavior is preserved)
    direct <- suppressMessages(get_centroid(
        c("choose", "picklespit", "moon"),
        fake_word_vectors,
        missing = "remove"
    ))
    expect_equal(out["with_oov", ], as.vector(direct))
})

test_that("get_centroid with missing='remove' excludes (not duplicates) the OOV term", {
    # regression test: `anchors[] <- ...` used to recycle the shortened
    # result back to the original length, silently duplicating "choose"
    # instead of correctly averaging only "choose" and "moon"
    cen <- expect_no_warning(
        suppressMessages(get_centroid(
            c("choose", "picklespit", "moon"),
            fake_word_vectors,
            missing = "remove"
        ))
    )
    expected <- (fake_word_vectors["choose", ] + fake_word_vectors["moon", ]) / 2
    expect_equal(as.vector(cen), unname(expected))
})

test_that("get_direction with missing='remove' does not crash when row count shrinks unevenly", {
    # regression test: `anchors[] <- ...` used to error ("replacement element
    # 1 has 2 rows, need 3") or silently recycle when dropping an
    # OOV-containing row left a data.frame whose row count didn't evenly
    # divide the original
    gen <- data.frame(
        pole1 = c("choose", "picklespit", "decade"),
        pole2 = c("decade", "this", "choose")
    )
    dir <- expect_no_warning(
        suppressMessages(get_direction(anchors = gen, wv = fake_word_vectors, missing = "remove"))
    )
    expect_identical(dim(dir), c(1L, as.integer(ncol(fake_word_vectors))))
})

test_that("get_centroids drops empty factor levels with a message", {
    mat <- fake_word_vectors[1:10, , drop = FALSE]
    labs <- factor(rep(c("a", "b"), each = 5), levels = c("a", "b", "c"))
    expect_message(
        out <- get_centroids(mat, groups = labs),
        "Dropping empty group level"
    )
    expect_identical(rownames(out), c("a", "b"))
})

test_that("get_centroids excludes rows with NA group labels, with a message", {
    mat <- fake_word_vectors[1:5, , drop = FALSE]
    labs <- c("a", "a", NA, "b", "b")

    expect_message(
        out <- get_centroids(mat, groups = labs),
        "1 row"
    )

    expect_identical(rownames(out), c("a", "b"))
    expect_equal(out["a", ], colMeans(mat[c(1, 2), , drop = FALSE]))
    expect_equal(out["b", ], colMeans(mat[c(4, 5), , drop = FALSE]))
})
