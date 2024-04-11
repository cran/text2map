test_that("CMDist works on different DTMtypes", {
  ## base R matrix ##
  out_bse <- CMDist(
    dtm = dtm_bse,
    cw = cw,
    cv = NULL,
    wv = fake_word_vectors,
    missing = "stop"
  )

  expect_s3_class(out_bse, "data.frame")
  expect_type(out_bse[, 2], "double")
  expect_identical(out_bse$doc_id, rownames(dtm_bse))

  ## dgCMatrix matrix ##
  out_dgc <- CMDist(
    dtm = dtm_dgc,
    cw = cw,
    cv = NULL,
    wv = fake_word_vectors,
    missing = "stop"
  )

  expect_s3_class(out_dgc, "data.frame")
  expect_type(out_dgc[, 2], "double")
  expect_identical(out_dgc$doc_id, rownames(dtm_dgc))
})

test_that("CMDist, the same doc has identical outputs across
          different runs with scale=FALSE", {
  out2 <- CMDist(
    dtm = dtm_dgc[1:2, ],
    cw = cw,
    cv = NULL,
    wv = fake_word_vectors,
    scale = FALSE,
    missing = "stop"
  )

  out4 <- CMDist(
    dtm = dtm_dgc[1:4, ],
    cw = cw,
    cv = NULL,
    wv = fake_word_vectors,
    scale = FALSE,
    missing = "stop"
  )

  expect_identical(out2[1, 2], out4[1, 2])
  expect_identical(out2[2, 2], out4[2, 2])
})

test_that("CMDist, handles dtm's with zero words", {
  out <- CMDist(
    dtm = dtm_dgc[, 1:35],
    cw = cw,
    cv = sd_01,
    wv = fake_word_vectors,
    scale = FALSE,
    missing = "remove"
  )

  expect_identical(out[10, 2], 0)
  expect_identical(out[10, 3], 0)
})

test_that("CMDist works with multiple words/compound concepts", {
  ## two concept words ##
  out <- CMDist(
    dtm = dtm_dgc,
    cw = cw_2,
    cv = NULL,
    wv = fake_word_vectors,
    missing = "stop"
  )

  expect_s3_class(out, "data.frame")
  expect_type(out[, 2], "double")
  expect_type(out[, 3], "double")
  expect_identical(out$doc_id, rownames(dtm_dgc))
  expect_identical(colnames(out), c("doc_id", cw_2))

  ## compound concept ##
  out <- CMDist(
    dtm = dtm_dgc,
    cw = cw_3,
    cv = NULL,
    wv = fake_word_vectors,
    missing = "stop"
  )

  expect_s3_class(out, "data.frame")
  expect_type(out[, 2], "double")
  expect_type(out[, 3], "double")
  expect_type(out[, 4], "double")
  expect_identical(out$doc_id, rownames(dtm_dgc))
  expect_identical(colnames(out), c("doc_id", "choose", "decade", "the"))

  ## compound concept with duplicated first words ##
  out <- CMDist(
    dtm = dtm_dgc,
    cw = cw_4,
    cv = NULL,
    wv = fake_word_vectors,
    missing = "stop"
  )

  expect_s3_class(out, "data.frame")
  expect_type(out[, 2], "double")
  expect_type(out[, 3], "double")
  expect_type(out[, 4], "double")
  expect_identical(out$doc_id, rownames(dtm_dgc))
  expect_identical(colnames(out), c("doc_id", "choose", "decade", "decade_1"))

  ## compound concept with duplicated vector labels ##

  sem_dirs <- rbind(
    get_centroid(anchor_solo_c, fake_word_vectors),
    get_centroid(anchor_solo_c, fake_word_vectors)
  )

  out <- CMDist(
    dtm = dtm_dgc,
    cw = cw_4,
    cv = sem_dirs,
    wv = fake_word_vectors,
    missing = "stop"
  )

  expect_s3_class(out, "data.frame")
  expect_type(out[, 2], "double")
  expect_type(out[, 3], "double")
  expect_type(out[, 4], "double")
  expect_identical(out$doc_id, rownames(dtm_dgc))
  expect_identical(
    colnames(out),
    c(
      "doc_id", "choose", "decade", "decade_1",
      "choose_centroid", "choose_centroid_1"
    )
  )
})
