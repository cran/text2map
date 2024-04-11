## ----------------------------------------------------------------------------
#
# tests for CMDist
#
## ----------------------------------------------------------------------------

## Quanteda tests
test_that("CMDist works on tm/quanteda DTMtypes", {
  testthat::skip_on_cran()
  testthat::skip_if_not_installed(c("quanteda"))
  require(quanteda)
  # if (requireNamespace("quanteda", quietly = TRUE))

  ## FOR COMPARISONS
  ## base R matrix ##
  cmd_out_bse <- CMDist(
    dtm = dtm_bse,
    cw = cw,
    cv = NULL,
    wv = fake_word_vectors,
    missing = "stop"
  )
  ## dgCMatrix matrix ##
  cmd_out_dgc <- CMDist(
    dtm = dtm_dgc,
    cw = cw,
    cv = NULL,
    wv = fake_word_vectors,
    missing = "stop"
  )

  ## dfm//dgCMatrix matrix ##
  cmd_out_dfm <- CMDist(
    dtm = dtm_dfm,
    cw = cw,
    cv = NULL,
    wv = fake_word_vectors,
    missing = "stop"
  )
  expect_s3_class(cmd_out_dfm, "data.frame")
  expect_type(cmd_out_dfm[, 2], "double")
  expect_identical(cmd_out_dfm$doc_id, rownames(dtm_dfm))

  ## all are identical
  expect_identical(cmd_out_bse, cmd_out_dgc, cmd_out_dfm)
})

## TM tests
test_that("CMDist works on tm/quanteda DTMtypes", {
  testthat::skip_on_cran()
  testthat::skip_if_not_installed("tm")
  require(tm)
  # if (requireNamespace("quanteda", quietly = TRUE))

  ## FOR COMPARISONS
  ## base R matrix ##
  cmd_out_bse <- CMDist(
    dtm = dtm_bse,
    cw = cw,
    cv = NULL,
    wv = fake_word_vectors,
    missing = "stop"
  )
  ## dgCMatrix matrix ##
  cmd_out_dgc <- CMDist(
    dtm = dtm_dgc,
    cw = cw,
    cv = NULL,
    wv = fake_word_vectors,
    missing = "stop"
  )

  ## tm//simple_triplet_matrix matrix ##
  cmd_out_tm <- CMDist(
    dtm = dtm_tm,
    cw = cw,
    cv = NULL,
    wv = fake_word_vectors,
    missing = "stop"
  )
  expect_s3_class(cmd_out_tm, "data.frame")
  expect_type(cmd_out_tm[, 2], "double")
  expect_identical(cmd_out_tm$doc_id, rownames(dtm_tm))

  ## all are identical
  expect_identical(cmd_out_bse, cmd_out_dgc, cmd_out_tm)
})

## ----------------------------------------------------------------------------
#
# tests for internal functions
#
## ----------------------------------------------------------------------------

## convert
test_that(".convert_mat_to_dgCMatrix convert all dtms to dgCMatrix", {
  testthat::skip_on_cran()
  testthat::skip_if_not_installed("tm")
  testthat::skip_if_not_installed("quanteda")
  require(tm)
  require(quanteda)

  ## base R matrix ##
  expect_s4_class(.convert_mat_to_dgCMatrix(dtm_bse), "dgCMatrix")
  ## dgCMatrix matrix ##
  expect_s4_class(.convert_mat_to_dgCMatrix(dtm_dgc), "dgCMatrix")
  ## dfm//dgCMatrix matrix ##
  expect_s4_class(.convert_mat_to_dgCMatrix(dtm_dfm), "dgCMatrix")
  ## tm//simple_triplet_matrix matrix ##
  expect_s4_class(.convert_mat_to_dgCMatrix(dtm_tm), "dgCMatrix")
  ## TermDocumentMatrix //  tm//simple_triplet_matrix matrix ##
  expect_s4_class(.convert_mat_to_dgCMatrix(dtm_tdm), "dgCMatrix")
})


## .prep_cmd_INPUT
test_that(".prep_cmd_INPUT works on dfm DTM", {
  testthat::skip_on_cran()
  testthat::skip_if_not_installed("quanteda")
  require(quanteda)
  ## quanteda dfm//dgCMatrix matrix ##
  prep_cmd_out <- .prep_cmd_INPUT(
    dtm = dtm_dfm,
    cw = cw,
    cv = NULL,
    wv = fake_word_vectors,
    missing = "stop"
  )

  expect_s4_class(prep_cmd_out$DTM, "dgCMatrix")
  expect_identical(dim(prep_cmd_out$wem), wv_dims)
})


test_that(".prep_cmd_INPUT works on tm DTM", {
  testthat::skip_on_cran()
  testthat::skip_if_not_installed("tm")
  require(tm)
  ## tm//simple_triplet_matrix matrix ##
  prep_cmd_out <- .prep_cmd_INPUT(
    dtm = dtm_tm,
    cw = cw,
    cv = NULL,
    wv = fake_word_vectors,
    missing = "stop"
  )

  expect_s4_class(prep_cmd_out$DTM, "dgCMatrix")
  expect_identical(dim(prep_cmd_out$wem), wv_dims)
})


test_that(".prep_cmd_INPUT adds OOV words on tm/quanteda DTM types", {
  testthat::skip_on_cran()
  testthat::skip_if_not_installed("tm")
  testthat::skip_if_not_installed("quanteda")
  require(tm)
  require(quanteda)
  ## dfm//dgCMatrix matrix ##
  prep_cmd_out <- .prep_cmd_INPUT(
    dtm = dtm_dfm,
    cw = cw_oov,
    cv = NULL,
    wv = fake_word_vectors_oov,
    missing = "stop"
  )

  expect_s4_class(prep_cmd_out$DTM, "dgCMatrix")
  expect_identical(dim(prep_cmd_out$wem), wv_oov_dims)

  ## tm//simple_triplet_matrix matrix ##
  prep_cmd_out <- .prep_cmd_INPUT(
    dtm = dtm_tm,
    cw = cw_oov,
    cv = NULL,
    wv = fake_word_vectors_oov,
    missing = "stop"
  )

  expect_s4_class(prep_cmd_out$DTM, "dgCMatrix")
  expect_identical(dim(prep_cmd_out$wem), wv_oov_dims)
})


test_that(".prep_cmd_INPUT adds concept vectors on tm/quanteda DTM types", {
  testthat::skip_on_cran()
  testthat::skip_if_not_installed("tm")
  testthat::skip_if_not_installed("quanteda")
  require(tm)
  require(quanteda)
  ## dfm//dgCMatrix matrix ##
  prep_cmd_out <- .prep_cmd_INPUT(
    dtm = dtm_dfm,
    cw = NULL,
    cv = get_centroid(anchor_solo_c, fake_word_vectors),
    wv = fake_word_vectors_oov,
    missing = "stop"
  )

  expect_s4_class(prep_cmd_out$DTM, "dgCMatrix")
  expect_identical(dim(prep_cmd_out$wem), wv_cv_dims)

  ## tm//simple_triplet_matrix matrix ##
  prep_cmd_out <- .prep_cmd_INPUT(
    dtm = dtm_tm,
    cw = NULL,
    cv = get_centroid(anchor_solo_c, fake_word_vectors),
    wv = fake_word_vectors_oov,
    missing = "stop"
  )

  expect_s4_class(prep_cmd_out$DTM, "dgCMatrix")
  expect_identical(dim(prep_cmd_out$wem), wv_cv_dims)
})


test_that(".prep_cmd_INPUT adds concept vectors and
            OOV words on tm/quanteda DTM types", {
  testthat::skip_on_cran()
  testthat::skip_if_not_installed("tm")
  testthat::skip_if_not_installed("quanteda")
  require(tm)
  require(quanteda)
  ## dfm//dgCMatrix matrix ##
  prep_cmd_out <- .prep_cmd_INPUT(
    dtm = dtm_dfm,
    cw = cw_oov,
    cv = get_centroid(anchor_solo_c, fake_word_vectors),
    wv = fake_word_vectors_oov,
    missing = "stop"
  )

  expect_s4_class(prep_cmd_out$DTM, "dgCMatrix")
  expect_identical(dim(prep_cmd_out$wem), wv_cv_cw_dims)

  ## tm//simple_triplet_matrix matrix ##
  prep_cmd_out <- .prep_cmd_INPUT(
    dtm = dtm_tm,
    cw = cw_oov,
    cv = get_centroid(anchor_solo_c, fake_word_vectors),
    wv = fake_word_vectors_oov,
    missing = "stop"
  )

  expect_s4_class(prep_cmd_out$DTM, "dgCMatrix")
  expect_identical(dim(prep_cmd_out$wem), wv_cv_cw_dims)
})


test_that("dtm_builder produces identical dtm to cast_dtm", {
  testthat::skip_on_cran()
  testthat::skip_if_not_installed("tm")
  testthat::skip_if_not_installed("quanteda")
  # example 1
  dtm_a <- jfk_corpus |>
    dtm_builder(clean_text, doc_id)

  # example 2
  dtm_b <- dtm_builder(jfk_corpus, text = clean_text, doc_id = doc_id)

  expect_identical(dim(dtm_a), dim(dtm_b))
  expect_identical(dim(dtm_a), dim(dtm_tm))

  expect_identical(sum(dtm_a), sum(dtm_b))
  expect_identical(sum(dtm_a), sum(dtm_tm))

  expect_identical(
    as.vector(colnames(dtm_a)),
    as.vector(colnames(dtm_b))
  )
  expect_identical(
    as.vector(sort(colnames(dtm_a))),
    as.vector(sort(colnames(dtm_tm)))
  )
})
