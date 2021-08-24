test_that(".prep_cmd_INPUT works on different DTM types", {

    ## base R matrix ##
    out <- .prep_cmd_INPUT(dtm=dtm.bse,
                        cw = cw,
                        cv = NULL,
                        wv = fake_word_vectors,
                        missing = "stop")

    expect_s4_class(out$DTM, "dgCMatrix")
    expect_identical(dim(out$wem),  wv.dims)


    ## dgCMatrix matrix ##
    out <- .prep_cmd_INPUT(dtm=dtm.dgc,
                    cw = cw,
                    cv = NULL,
                    wv = fake_word_vectors,
                    missing = "stop")

    expect_s4_class(out$DTM, "dgCMatrix")
    expect_identical(dim(out$wem),  wv.dims)

    ## dfm//dgCMatrix matrix ##
    out <- .prep_cmd_INPUT(dtm=dtm.dfm,
                    cw = cw,
                    cv = NULL,
                    wv = fake_word_vectors,
                    missing = "stop")

    expect_s4_class(out$DTM, "dgCMatrix")
    expect_identical(dim(out$wem),  wv.dims)

    ## tm//simple_triplet_matrix matrix ##
    out <- .prep_cmd_INPUT(dtm=dtm.tm,
                    cw = cw,
                    cv = NULL,
                    wv = fake_word_vectors,
                    missing = "stop")

    expect_s4_class(out$DTM, "dgCMatrix")
    expect_identical(dim(out$wem),  wv.dims)


})

test_that(".prep_cmd_INPUT adds OOV words on different DTM types", {

  ## base R matrix ##
  out <- .prep_cmd_INPUT(dtm=dtm.bse,
                         cw = cw.oov,
                         cv = NULL,
                         wv = fake_word_vectors_oov,
                         missing = "stop")

  expect_s4_class(out$DTM, "dgCMatrix")
  expect_identical(dim(out$wem),  wv.oov.dims)

  ## dgCMatrix matrix ##
  out <- .prep_cmd_INPUT(dtm=dtm.dgc,
                         cw = cw.oov,
                         cv = NULL,
                         wv = fake_word_vectors_oov,
                         missing = "stop")

  expect_s4_class(out$DTM, "dgCMatrix")
  expect_identical(dim(out$wem),  wv.oov.dims)

  ## dfm//dgCMatrix matrix ##
  out <- .prep_cmd_INPUT(dtm=dtm.dfm,
                         cw = cw.oov,
                         cv = NULL,
                         wv = fake_word_vectors_oov,
                         missing = "stop")

  expect_s4_class(out$DTM, "dgCMatrix")
  expect_identical(dim(out$wem),  wv.oov.dims)

  ## tm//simple_triplet_matrix matrix ##
  out <- .prep_cmd_INPUT(dtm=dtm.tm,
                         cw = cw.oov,
                         cv = NULL,
                         wv = fake_word_vectors_oov,
                         missing = "stop")

  expect_s4_class(out$DTM, "dgCMatrix")
  expect_identical(dim(out$wem),  wv.oov.dims)

})


test_that(".prep_cmd_INPUT adds concept vectors on different DTM types", {

  ## base R matrix ##
  out <- .prep_cmd_INPUT(dtm=dtm.bse,
                         cw = NULL,
                         cv = get_centroid(anchor.solo.c, fake_word_vectors),
                         wv = fake_word_vectors,
                         missing = "stop")

  expect_s4_class(out$DTM, "dgCMatrix")
  expect_identical(dim(out$wem),  wv.cv.dims)

  ## dgCMatrix matrix ##
  out <- .prep_cmd_INPUT(dtm=dtm.dgc,
                         cw = NULL,
                         cv = get_centroid(anchor.solo.c,fake_word_vectors),
                         wv = fake_word_vectors,
                         missing = "stop")

  expect_s4_class(out$DTM, "dgCMatrix")
  expect_identical(dim(out$wem),  wv.cv.dims)

  ## dfm//dgCMatrix matrix ##
  out <- .prep_cmd_INPUT(dtm=dtm.dfm,
                         cw = NULL,
                         cv = get_centroid(anchor.solo.c,fake_word_vectors),
                         wv = fake_word_vectors_oov,
                         missing = "stop")

  expect_s4_class(out$DTM, "dgCMatrix")
  expect_identical(dim(out$wem),  wv.cv.dims)

  ## tm//simple_triplet_matrix matrix ##
  out <- .prep_cmd_INPUT(dtm=dtm.tm,
                         cw = NULL,
                         cv = get_centroid(anchor.solo.c, fake_word_vectors),
                         wv = fake_word_vectors_oov,
                         missing = "stop")

  expect_s4_class(out$DTM, "dgCMatrix")
  expect_identical(dim(out$wem),  wv.cv.dims)

})



test_that(".prep_cmd_INPUT adds concept vectors and
            OOV words on different DTM types", {

  ## base R matrix ##
  out <- .prep_cmd_INPUT(dtm=dtm.bse,
                         cw = cw.oov,
                         cv = get_centroid(anchor.solo.c, fake_word_vectors),
                         wv = fake_word_vectors_oov,
                         missing = "stop")

  expect_s4_class(out$DTM, "dgCMatrix")
  expect_identical(dim(out$wem), wv.cv.cw.dims)

  ## dgCMatrix matrix ##
  out <- .prep_cmd_INPUT(dtm=dtm.dgc,
                         cw = cw.oov,
                         cv = get_centroid(anchor.solo.c,fake_word_vectors),
                         wv = fake_word_vectors_oov,
                         missing = "stop")

  expect_s4_class(out$DTM, "dgCMatrix")
  expect_identical(dim(out$wem), wv.cv.cw.dims)

  ## dfm//dgCMatrix matrix ##
  out <- .prep_cmd_INPUT(dtm=dtm.dfm,
                         cw = cw.oov,
                         cv = get_centroid(anchor.solo.c,fake_word_vectors),
                         wv = fake_word_vectors_oov,
                         missing = "stop")

  expect_s4_class(out$DTM, "dgCMatrix")
  expect_identical(dim(out$wem), wv.cv.cw.dims)

  ## tm//simple_triplet_matrix matrix ##
  out <- .prep_cmd_INPUT(dtm=dtm.tm,
                         cw = cw.oov,
                         cv = get_centroid(anchor.solo.c, fake_word_vectors),
                         wv = fake_word_vectors_oov,
                         missing = "stop")

  expect_s4_class(out$DTM, "dgCMatrix")
  expect_identical(dim(out$wem), wv.cv.cw.dims)

})


test_that("CMDist works on different DTMtypes", {

    ## base R matrix ##
    out.bse <- CMDist(dtm=dtm.bse,
                        cw = cw,
                        cv = NULL,
                        wv = fake_word_vectors,
                        missing = "stop")

    expect_s3_class(out.bse, "data.frame")
    expect_type(out.bse[,2], "double")
    expect_identical(out.bse$doc_id, rownames(dtm.bse))

    ## dgCMatrix matrix ##
    out.dgc <- CMDist(dtm=dtm.dgc,
                        cw = cw,
                        cv = NULL,
                        wv = fake_word_vectors,
                        missing = "stop")

    expect_s3_class(out.dgc, "data.frame")
    expect_type(out.dgc[,2], "double")
    expect_identical(out.dgc$doc_id, rownames(dtm.dgc))

    ## dfm//dgCMatrix matrix ##
    out.dfm <- CMDist(dtm=dtm.dfm,
                        cw = cw,
                        cv = NULL,
                        wv = fake_word_vectors,
                        missing = "stop")

    expect_s3_class(out.dfm, "data.frame")
    expect_type(out.dfm[,2], "double")
    expect_identical(out.dfm$doc_id, rownames(dtm.dfm))

    ## tm//simple_triplet_matrix matrix ##
    out.tm <- CMDist(dtm=dtm.tm,
                        cw = cw,
                        cv = NULL,
                        wv = fake_word_vectors,
                        missing = "stop")

    expect_s3_class(out.tm, "data.frame")
    expect_type(out.tm[,2], "double")
    expect_identical(out.tm$doc_id, rownames(dtm.tm))

    expect_identical(out.bse, out.dgc, out.dfm, out.tm)

})

test_that("CMDist, the same doc has identical outputs across
          different runs with scale=FALSE", {

    out.2 <- CMDist(dtm=dtm.dgc[1:2, ],
                     cw = cw,
                     cv = NULL,
                     wv = fake_word_vectors,
                     scale = FALSE,
                     missing = "stop")

    out.4 <- CMDist(dtm=dtm.dgc[1:4, ],
                    cw = cw,
                    cv = NULL,
                    wv = fake_word_vectors,
                    scale = FALSE,
                    missing = "stop")

    expect_identical(out.2[1,2], out.4[1,2])
    expect_identical(out.2[2,2], out.4[2,2])

})

test_that("CMDist, handles dtm's with zero words", {

            out <- CMDist(dtm=dtm.dgc[, 1:35],
                            cw = cw,
                            cv = sd.01,
                            wv = fake_word_vectors,
                            scale = FALSE,
                            missing = "remove")

            expect_identical(out[10,2], 0)
            expect_identical(out[10,3], 0)

})

test_that("CMDist works with multiple words/compound concepts", {

  ## two concept words ##
  out <- CMDist(dtm=dtm.dgc,
                    cw = cw.2,
                    cv = NULL,
                    wv = fake_word_vectors,
                    missing = "stop")

  expect_s3_class(out, "data.frame")
  expect_type(out[,2], "double")
  expect_type(out[,3], "double")
  expect_identical(out$doc_id, rownames(dtm.dgc))
  expect_identical(colnames(out), c("doc_id", cw.2) )

  ## compound concept ##
  out <- CMDist(dtm=dtm.dgc,
                cw = cw.3,
                cv = NULL,
                wv = fake_word_vectors,
                missing = "stop")

  expect_s3_class(out, "data.frame")
  expect_type(out[,2], "double")
  expect_type(out[,3], "double")
  expect_type(out[,4], "double")
  expect_identical(out$doc_id, rownames(dtm.dgc))
  expect_identical(colnames(out), c("doc_id", "choose", "decade", "the") )

  ## compound concept with duplicated first words ##
  out <- CMDist(dtm=dtm.dgc,
                cw = cw.4,
                cv = NULL,
                wv = fake_word_vectors,
                missing = "stop")

  expect_s3_class(out, "data.frame")
  expect_type(out[,2], "double")
  expect_type(out[,3], "double")
  expect_type(out[,4], "double")
  expect_identical(out$doc_id, rownames(dtm.dgc))
  expect_identical(colnames(out), c("doc_id", "choose", "decade", "decade_1") )

  ## compound concept with duplicated vector labels ##

  sem.dirs <- rbind(get_centroid(anchor.solo.c, fake_word_vectors),
                 get_centroid(anchor.solo.c, fake_word_vectors))

  out <- CMDist(dtm=dtm.dgc,
                cw = cw.4,
                cv = sem.dirs,
                wv = fake_word_vectors,
                missing = "stop")

  expect_s3_class(out, "data.frame")
  expect_type(out[,2], "double")
  expect_type(out[,3], "double")
  expect_type(out[,4], "double")
  expect_identical(out$doc_id, rownames(dtm.dgc))
  expect_identical(colnames(out),
                   c("doc_id", "choose", "decade", "decade_1",
                      "choose_centroid", "choose_centroid_1") )

})

