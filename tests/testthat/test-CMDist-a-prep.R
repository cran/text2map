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
