test_that("CMDist errors on invalid missing argument", {
    expect_error(
        CMDist(dtm_dgc, cw = "choose", wv = fake_word_vectors, missing = "stpo"),
        class = "simpleError"
    )
})

test_that("CMDist errors when both cw and cv are NULL", {
    expect_error(
        CMDist(dtm_dgc, cw = NULL, cv = NULL, wv = fake_word_vectors),
        "concept"
    )
})

test_that("CMDist errors with a clear message when cv has 0 rows", {
    expect_error(
        CMDist(dtm_dgc, cv = sd_01[0, , drop = FALSE], wv = fake_word_vectors),
        "0 rows"
    )
})

test_that("CoCA errors when directions is NULL", {
    expect_error(
        CoCA(dtm_bse, wv = fake_word_vectors_coca, directions = NULL),
        "directions"
    )
})

test_that("CoCA errors when wv is NULL", {
    expect_error(
        CoCA(dtm_bse, wv = NULL, directions = sem_dirs),
        "word embedding"
    )
})

test_that("get_direction errors on invalid missing argument", {
    expect_error(
        get_direction(anchor_pair_df, fake_word_vectors, missing = "droop"),
        class = "simpleError"
    )
})

test_that("get_centroid errors on invalid missing argument", {
    expect_error(
        get_centroid(anchor_solo_c, fake_word_vectors, missing = "dropx"),
        class = "simpleError"
    )
})

test_that("perm_tester errors on invalid alternative", {
    data(meta_shakespeare)
    model <- lm(body_count ~ year, data = meta_shakespeare)

    expect_error(
        perm_tester(
            data = meta_shakespeare,
            model = model,
            statistic = "coefficients",
            perm_n = 10,
            alternative = "upwards",
            seed = 1
        ),
        class = "simpleError"
    )
})

test_that("find_transformation errors on invalid method", {
    expect_error(
        find_transformation(fake_word_vectors, method = "rotate"),
        class = "simpleError"
    )
})

test_that("CoCA errors on invalid zero_action", {
    expect_error(
        CoCA(dtm_bse, wv = fake_word_vectors_coca, directions = sem_dirs,
             zero_action = "invalid"),
        class = "simpleError"
    )
})

test_that("find_transformation errors when ref is NULL for align method", {
    expect_error(
        find_transformation(fake_word_vectors, ref = NULL, method = "align"),
        "ref"
    )
})

test_that("find_transformation align gives a clear error when wv and ref have different row counts", {
    # align requires wv and ref to represent the same terms in the same
    # row order; a mismatch used to fall through to an opaque
    # "non-conformable arguments" error from rsvd::rsvd(crossprod(...))
    mismatched_ref <- fake_word_vectors[1:5, , drop = FALSE]

    expect_error(
        find_transformation(fake_word_vectors, ref = mismatched_ref, method = "align"),
        "same number of rows"
    )
})

test_that("find_transformation errors when ref is NULL for retrofit method", {
    expect_error(
        find_transformation(fake_word_vectors, ref = NULL, method = "retrofit"),
        "ref"
    )
})

test_that("find_transformation retrofit gives a clear error when wv and ref share no vocabulary", {
    no_overlap_ref <- matrix(
        rnorm(9),
        nrow = 3,
        dimnames = list(c("zzz1", "zzz2", "zzz3"), c("zzz1", "zzz2", "zzz3"))
    )

    # rownames(fake_word_vectors) and rownames(no_overlap_ref) are disjoint,
    # so this must not fall through to an opaque rsvd() failure
    expect_error(
        find_transformation(fake_word_vectors, ref = no_overlap_ref, method = "retrofit"),
        "vocabulary"
    )
})

test_that("find_transformation retrofit gives a clear error when shared vocab is smaller than ncol(wv)", {
    # only 1 shared term ("choose"), but fake_word_vectors has 10 dimensions;
    # rsvd can recover at most 1 component from a 1x1 matrix, so this must
    # not fall through to an opaque downstream rsvd/procrustes dims mismatch
    one_term_ref <- matrix(
        rnorm(1),
        nrow = 1,
        dimnames = list("choose", "choose")
    )

    expect_error(
        find_transformation(fake_word_vectors, ref = one_term_ref, method = "retrofit"),
        "shared"
    )
})
