
test_that(".prep_cmd_INPUT works on base R DTM", {
  ## base R matrix ##
  out <- .prep_cmd_INPUT(
    dtm = dtm.bse,
    cw = cw,
    cv = NULL,
    wv = fake_word_vectors,
    missing = "stop"
  )

  expect_s4_class(out$DTM, "dgCMatrix")
  expect_identical(dim(out$wem), wv.dims)

})


test_that(".prep_cmd_INPUT works on dgCMatrix DTM", {

  ## dgCMatrix matrix ##
  out <- .prep_cmd_INPUT(
    dtm = dtm.dgc,
    cw = cw,
    cv = NULL,
    wv = fake_word_vectors,
    missing = "stop"
  )

  expect_s4_class(out$DTM, "dgCMatrix")
  expect_identical(dim(out$wem), wv.dims)

})


test_that(".prep_cmd_INPUT works on dfm DTM", {

  ## quanteda dfm//dgCMatrix matrix ##
  out <- .prep_cmd_INPUT(
    dtm = dtm.dfm,
    cw = cw,
    cv = NULL,
    wv = fake_word_vectors,
    missing = "stop"
  )

  expect_s4_class(out$DTM, "dgCMatrix")
  expect_identical(dim(out$wem), wv.dims)


})


test_that(".prep_cmd_INPUT works on tm DTM", {

  ## tm//simple_triplet_matrix matrix ##
  out <- .prep_cmd_INPUT(
    dtm = dtm.tm,
    cw = cw,
    cv = NULL,
    wv = fake_word_vectors,
    missing = "stop"
  )

  expect_s4_class(out$DTM, "dgCMatrix")
  expect_identical(dim(out$wem), wv.dims)
  
})



test_that(".prep_cmd_INPUT adds OOV words on different DTM types", {
  ## base R matrix ##
  out <- .prep_cmd_INPUT(
    dtm = dtm.bse,
    cw = cw.oov,
    cv = NULL,
    wv = fake_word_vectors_oov,
    missing = "stop"
  )

  expect_s4_class(out$DTM, "dgCMatrix")
  expect_identical(dim(out$wem), wv.oov.dims)

  ## dgCMatrix matrix ##
  out <- .prep_cmd_INPUT(
    dtm = dtm.dgc,
    cw = cw.oov,
    cv = NULL,
    wv = fake_word_vectors_oov,
    missing = "stop"
  )

  expect_s4_class(out$DTM, "dgCMatrix")
  expect_identical(dim(out$wem), wv.oov.dims)

  ## dfm//dgCMatrix matrix ##
  out <- .prep_cmd_INPUT(
    dtm = dtm.dfm,
    cw = cw.oov,
    cv = NULL,
    wv = fake_word_vectors_oov,
    missing = "stop"
  )

  expect_s4_class(out$DTM, "dgCMatrix")
  expect_identical(dim(out$wem), wv.oov.dims)

  ## tm//simple_triplet_matrix matrix ##
  out <- .prep_cmd_INPUT(
    dtm = dtm.tm,
    cw = cw.oov,
    cv = NULL,
    wv = fake_word_vectors_oov,
    missing = "stop"
  )

  expect_s4_class(out$DTM, "dgCMatrix")
  expect_identical(dim(out$wem), wv.oov.dims)
})


test_that(".prep_cmd_INPUT adds concept vectors on different DTM types", {
  ## base R matrix ##
  out <- .prep_cmd_INPUT(
    dtm = dtm.bse,
    cw = NULL,
    cv = get_centroid(anchor.solo.c, fake_word_vectors),
    wv = fake_word_vectors,
    missing = "stop"
  )

  expect_s4_class(out$DTM, "dgCMatrix")
  expect_identical(dim(out$wem), wv.cv.dims)

  ## dgCMatrix matrix ##
  out <- .prep_cmd_INPUT(
    dtm = dtm.dgc,
    cw = NULL,
    cv = get_centroid(anchor.solo.c, fake_word_vectors),
    wv = fake_word_vectors,
    missing = "stop"
  )

  expect_s4_class(out$DTM, "dgCMatrix")
  expect_identical(dim(out$wem), wv.cv.dims)

  ## dfm//dgCMatrix matrix ##
  out <- .prep_cmd_INPUT(
    dtm = dtm.dfm,
    cw = NULL,
    cv = get_centroid(anchor.solo.c, fake_word_vectors),
    wv = fake_word_vectors_oov,
    missing = "stop"
  )

  expect_s4_class(out$DTM, "dgCMatrix")
  expect_identical(dim(out$wem), wv.cv.dims)

  ## tm//simple_triplet_matrix matrix ##
  out <- .prep_cmd_INPUT(
    dtm = dtm.tm,
    cw = NULL,
    cv = get_centroid(anchor.solo.c, fake_word_vectors),
    wv = fake_word_vectors_oov,
    missing = "stop"
  )

  expect_s4_class(out$DTM, "dgCMatrix")
  expect_identical(dim(out$wem), wv.cv.dims)
})



test_that(".prep_cmd_INPUT adds concept vectors and
            OOV words on different DTM types", {
  ## base R matrix ##
  out <- .prep_cmd_INPUT(
    dtm = dtm.bse,
    cw = cw.oov,
    cv = get_centroid(anchor.solo.c, fake_word_vectors),
    wv = fake_word_vectors_oov,
    missing = "stop"
  )

  expect_s4_class(out$DTM, "dgCMatrix")
  expect_identical(dim(out$wem), wv.cv.cw.dims)

  ## dgCMatrix matrix ##
  out <- .prep_cmd_INPUT(
    dtm = dtm.dgc,
    cw = cw.oov,
    cv = get_centroid(anchor.solo.c, fake_word_vectors),
    wv = fake_word_vectors_oov,
    missing = "stop"
  )

  expect_s4_class(out$DTM, "dgCMatrix")
  expect_identical(dim(out$wem), wv.cv.cw.dims)

  ## dfm//dgCMatrix matrix ##
  out <- .prep_cmd_INPUT(
    dtm = dtm.dfm,
    cw = cw.oov,
    cv = get_centroid(anchor.solo.c, fake_word_vectors),
    wv = fake_word_vectors_oov,
    missing = "stop"
  )

  expect_s4_class(out$DTM, "dgCMatrix")
  expect_identical(dim(out$wem), wv.cv.cw.dims)

  ## tm//simple_triplet_matrix matrix ##
  out <- .prep_cmd_INPUT(
    dtm = dtm.tm,
    cw = cw.oov,
    cv = get_centroid(anchor.solo.c, fake_word_vectors),
    wv = fake_word_vectors_oov,
    missing = "stop"
  )

  expect_s4_class(out$DTM, "dgCMatrix")
  expect_identical(dim(out$wem), wv.cv.cw.dims)
})