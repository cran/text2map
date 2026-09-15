test_that(".prep_cmd_INPUT works on base R DTM", {
  ## base R matrix ##
  out <- .prep_cmd_INPUT(
    dtm = dtm_bse,
    cw = cw,
    cv = NULL,
    wv = fake_word_vectors,
    missing = "stop"
  )

  expect_s4_class(out$DTM, "dgCMatrix")
  expect_identical(dim(out$wem), wv_dims)
})


test_that(".prep_cmd_INPUT works on dgCMatrix DTM", {
  ## dgCMatrix matrix ##
  out <- .prep_cmd_INPUT(
    dtm = dtm_dgc,
    cw = cw,
    cv = NULL,
    wv = fake_word_vectors,
    missing = "stop"
  )

  expect_s4_class(out$DTM, "dgCMatrix")
  expect_identical(dim(out$wem), wv_dims)
})




test_that(".prep_cmd_INPUT adds OOV words on different DTM types", {
  ## base R matrix ##
  out <- .prep_cmd_INPUT(
    dtm = dtm_bse,
    cw = cw_oov,
    cv = NULL,
    wv = fake_word_vectors_oov,
    missing = "stop"
  )

  expect_s4_class(out$DTM, "dgCMatrix")
  expect_identical(dim(out$wem), wv_oov_dims)

  ## dgCMatrix matrix ##
  out <- .prep_cmd_INPUT(
    dtm = dtm_dgc,
    cw = cw_oov,
    cv = NULL,
    wv = fake_word_vectors_oov,
    missing = "stop"
  )

  expect_s4_class(out$DTM, "dgCMatrix")
  expect_identical(dim(out$wem), wv_oov_dims)
})


test_that(".prep_cmd_INPUT adds multiple missing concept words as all-zero columns", {
  # regression coverage for the vocab-growing step, which previously added
  # one missing concept word per cbind() call in a loop; batching these into
  # a single cbind() must produce identical output
  new_words <- paste0("zzz_missing_", 1:5)
  extra_wv <- matrix(
    stats::rnorm(length(new_words) * ncol(fake_word_vectors)),
    nrow = length(new_words)
  )
  rownames(extra_wv) <- new_words
  wv_aug <- rbind(fake_word_vectors, extra_wv)

  out <- .prep_cmd_INPUT(
    dtm = dtm_dgc,
    cw = new_words,
    cv = NULL,
    wv = wv_aug,
    missing = "stop"
  )

  expect_s4_class(out$DTM, "dgCMatrix")
  expect_true(all(new_words %in% colnames(out$DTM)))
  expect_true(all(Matrix::colSums(out$DTM[, new_words, drop = FALSE]) == 0))
  expect_equal(out$n_pd, length(new_words))
  expect_equal(out$labels, new_words)
})


test_that(".prep_cmd_INPUT adds concept vectors on different DTM types", {
  ## base R matrix ##
  out <- .prep_cmd_INPUT(
    dtm = dtm_bse,
    cw = NULL,
    cv = get_centroid(anchor_solo_c, fake_word_vectors),
    wv = fake_word_vectors,
    missing = "stop"
  )

  expect_s4_class(out$DTM, "dgCMatrix")
  expect_identical(dim(out$wem), wv_cv_dims)

  ## dgCMatrix matrix ##
  out <- .prep_cmd_INPUT(
    dtm = dtm_dgc,
    cw = NULL,
    cv = get_centroid(anchor_solo_c, fake_word_vectors),
    wv = fake_word_vectors,
    missing = "stop"
  )

  expect_s4_class(out$DTM, "dgCMatrix")
  expect_identical(dim(out$wem), wv_cv_dims)
})


test_that(".prep_cmd_INPUT adds concept vectors and
            OOV words on different DTM types", {
  ## base R matrix ##
  out <- .prep_cmd_INPUT(
    dtm = dtm_bse,
    cw = cw_oov,
    cv = get_centroid(anchor_solo_c, fake_word_vectors),
    wv = fake_word_vectors_oov,
    missing = "stop"
  )

  expect_s4_class(out$DTM, "dgCMatrix")
  expect_identical(dim(out$wem), wv_cv_cw_dims)

  ## dgCMatrix matrix ##
  out <- .prep_cmd_INPUT(
    dtm = dtm_dgc,
    cw = cw_oov,
    cv = get_centroid(anchor_solo_c, fake_word_vectors),
    wv = fake_word_vectors_oov,
    missing = "stop"
  )

  expect_s4_class(out$DTM, "dgCMatrix")
  expect_identical(dim(out$wem), wv_cv_cw_dims)
})
