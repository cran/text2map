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
  expect_true(all(is.finite(out_bse[, 2])))

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
  expect_true(all(is.finite(out_dgc[, 2])))

  # bse and dgc produce identical results
  expect_identical(out_bse, out_dgc)
})

test_that("CMDist works on a single-document DTM", {
  # a 1-row DTM must not collapse to a plain vector when the shared
  # vocabulary is subset out of it internally; scale=FALSE isolates this
  # from the separate (and legitimate) fact that z-scoring a single value
  # is undefined (sd of one observation is NA)
  out <- CMDist(
    dtm = dtm_dgc[1, , drop = FALSE],
    cw = cw,
    cv = NULL,
    wv = fake_word_vectors,
    scale = FALSE,
    missing = "stop"
  )

  expect_s3_class(out, "data.frame")
  expect_equal(nrow(out), 1L)
  expect_true(all(is.finite(out[, 2])))
})

test_that("CMDist works when a multi-word cw is partially out-of-vocabulary", {
  # a bad word inside a multi-word cw phrase used to survive
  # `.check_term_in_embeddings(action = "remove")` (it only matched whole
  # phrases against bad words, not their constituent words), so the phrase
  # was never dropped from `cw` even though the message claimed it was;
  # the leftover bad word then reached a pDTM column that no longer existed,
  # crashing with "invalid character indexing"
  out <- CMDist(
    dtm = dtm_dgc,
    cw = c("choose", "nonexistent_word_xyz moon"),
    cv = NULL,
    wv = fake_word_vectors,
    missing = "remove"
  )

  expect_s3_class(out, "data.frame")
  expect_equal(nrow(out), nrow(dtm_dgc))
  expect_named(out, c("doc_id", "choose"))
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
  # non-zero distances should be positive
  nonzero <- out[out$doc_id != rownames(dtm_dgc)[10], ]
  expect_true(all(nonzero[, 2] != 0))
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

test_that("CMDist output has class CMDist", {
  out <- CMDist(dtm = dtm_dgc, cw = cw, cv = NULL, wv = fake_word_vectors)
  expect_s3_class(out, "CMDist")
  expect_s3_class(out, "data.frame")
})

test_that("print.CMDist reports concepts and document count", {
  out <- CMDist(dtm = dtm_dgc, cw = cw, cv = NULL, wv = fake_word_vectors)

  printed <- capture.output(print(out))
  expect_true(any(grepl("CMDist estimates", printed, fixed = TRUE)))
  expect_true(any(grepl(paste0("Concepts: ", cw), printed, fixed = TRUE)))
  expect_true(any(grepl(nrow(out), printed, fixed = TRUE)))
})

test_that("print.CMDist notes when sensitivity intervals are present", {
  out <- CMDist(
    dtm = dtm_dgc, cw = cw, cv = NULL, wv = fake_word_vectors,
    sens_interval = TRUE, n_iters = 5
  )

  printed <- capture.output(print(out))
  expect_true(any(grepl("sensitivity intervals", printed, fixed = TRUE)))
})

test_that("plot.CMDist plots a single concept without error", {
  out <- CMDist(dtm = dtm_dgc, cw = cw, cv = NULL, wv = fake_word_vectors)

  pdf(NULL)
  on.exit(dev.off())
  result <- plot(out)
  expect_s3_class(result, "CMDist")
})

test_that("plot.CMDist plots multiple concepts and respects concept=", {
  out <- CMDist(
    dtm = dtm_dgc, cw = cw_4, cv = NULL, wv = fake_word_vectors
  )

  pdf(NULL)
  on.exit(dev.off())
  expect_no_error(plot(out))
  expect_no_error(plot(out, concept = "choose"))
  expect_error(plot(out, concept = "not_a_real_concept"), "concept")
})

test_that("plot.CMDist draws sensitivity interval error bars without error", {
  out <- CMDist(
    dtm = dtm_dgc, cw = cw, cv = NULL, wv = fake_word_vectors,
    sens_interval = TRUE, n_iters = 5
  )

  pdf(NULL)
  on.exit(dev.off())
  expect_no_error(plot(out))
})
