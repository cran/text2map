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

    df_error <- cbind(df_anchors, df_anchors)
    non_anchors_errors1 <- 1:4
    non_anchors_errors2 <- c(non_anchors, "stand")

    expect_error(test_anchors(df, ft_wv_sample, method = "relco", 
                              type = "centroid", non_anchors = non_anchors))
    expect_error(test_anchors(df, ft_wv_sample, method = "relco", 
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
        ),
        "method must be 'pooled', 'paired', 'L2' or 'PCA'"
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
