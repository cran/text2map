
test_that("dtm_stats returns expect output", {

    # simplified, base R dtm
    out <- dtm_stats(dtm.bse,
                     richness = TRUE,
                     distribution = TRUE,
                     central = TRUE,
                     character = TRUE,
                     simplify = TRUE)
    expect_equal(dim(out), as.integer(c(1L, 26L)) )
    expect_type(out, "list")
    expect_equal(unlist(out$n_docs),  nrow(dtm.bse))
    expect_equal(unlist(out$n_types),  ncol(dtm.bse))
    expect_equal(unlist(out$n_tokens),  sum(dtm.bse))

    # not simplified, base R dtm
    out <- dtm_stats(dtm.bse)
    expect_equal(length(out), 5L)
    expect_equal(dim(out[[1]]), as.integer(c(5L, 2L)) )
    expect_equal(dim(out[[2]]), as.integer(c(4L, 2L)) )
    expect_equal(dim(out[[3]]), as.integer(c(10L, 2L)) )
    expect_equal(dim(out[[4]]), as.integer(c(4L, 2L)) )
    expect_equal(dim(out[[5]]), as.integer(c(3L, 2L)) )
    expect_type(out, "list")
    expect_equal(as.character(out[[1]][1,2]),  as.character(nrow(dtm.bse)))
    expect_equal(as.character(out[[1]][3,2]),  as.character(ncol(dtm.bse)))
    expect_equal(as.character(out[[1]][4,2]),  as.character(sum(dtm.bse)))

    # simplified, dgCMatrix R dtm
    out <- dtm_stats(dtm.dgc,
                     richness = TRUE,
                     distribution = TRUE,
                     central = TRUE,
                     character = TRUE,
                     simplify = TRUE)
    expect_equal(dim(out), as.integer(c(1L, 26L)) )
    expect_type(out, "list")
    expect_equal(unlist(out$n_docs),  nrow(dtm.bse))
    expect_equal(unlist(out$n_types),  ncol(dtm.bse))
    expect_equal(unlist(out$n_tokens),  sum(dtm.bse))

    # not simplified, dgCMatrix R dtm
    out <- dtm_stats(dtm.dgc)
    expect_equal(dim(out[[1]]), as.integer(c(5L, 2L)) )
    expect_equal(dim(out[[2]]), as.integer(c(4L, 2L)) )
    expect_equal(dim(out[[3]]), as.integer(c(10L, 2L)) )
    expect_equal(dim(out[[4]]), as.integer(c(4L, 2L)) )
    expect_equal(dim(out[[5]]), as.integer(c(3L, 2L)) )
    expect_type(out, "list")
    expect_equal(as.character(out[[1]][1,2]),  as.character(nrow(dtm.bse)))
    expect_equal(as.character(out[[1]][3,2]),  as.character(ncol(dtm.bse)))
    expect_equal(as.character(out[[1]][4,2]),  as.character(sum(dtm.bse)))

})

test_that("dtm_stats returns basic output alone", {

    out <- dtm_stats(dtm=dtm.bse,
                     richness = FALSE,
                     distribution = FALSE,
                     central = FALSE,
                     character = FALSE,
                     simplify = FALSE)
    expect_type(out, "list")
    expect_equal(length(out), as.integer(1L))
    expect_equal(dim(out[[1]]), as.integer(c(5L, 2L)) )

})


test_that("dtm_builder produces identical dtm to cast_dtm", {

    my.corpus <- data.frame(
        text = c("I hear babies crying I watch them grow",
                 "They’ll learn much more than I'll ever know",
                 "And I think to myself",
                 "What a wonderful world",
                 "Yes I think to myself",
                 "What a wonderful world"),
        line_id = paste0("line", 1:6))

    # some text preprocessing
    my.corpus$clean_text <- tolower(gsub("'|’", "", my.corpus$text) )

    # example 1
    dtm.a <- my.corpus |>
        dtm_builder(clean_text, line_id)

    # example 2
    dtm.b <-  dtm_builder(my.corpus, text=clean_text, doc_id=line_id)

    # example 3
    dtm.c <- my.corpus |>
        dplyr::mutate(clean_text = gsub("'|’", "", text),
                      clean_text = tolower(clean_text)) |>
        dtm_builder(clean_text, line_id)

    # compare to tidy
    dtm.tidy <- my.corpus |>
        tidytext::unnest_tokens(word, clean_text) |>
        dplyr::count(line_id, word, sort = TRUE) |>
        tidytext::cast_dtm(line_id, word, n)

    expect_identical(dim(dtm.a), dim(dtm.b) )
    expect_identical(dim(dtm.a), dim(dtm.c) )
    expect_identical(dim(dtm.b), dim(dtm.c) )
    expect_identical(dim(dtm.a), dim(dtm.tidy) )

    expect_identical(sum(dtm.a), sum(dtm.b) )
    expect_identical(sum(dtm.a), sum(dtm.c) )
    expect_identical(sum(dtm.b), sum(dtm.c) )
    expect_identical(sum(dtm.a), sum(dtm.tidy) )

    expect_identical(as.vector(colnames(dtm.a)),
                     as.vector(colnames(dtm.b)))
    expect_identical(as.vector(colnames(dtm.a)),
                     as.vector(colnames(dtm.c)))
    expect_identical(as.vector(colnames(dtm.b)),
                     as.vector(colnames(dtm.c)))
    expect_identical(as.vector(sort(colnames(dtm.a))),
                     as.vector(sort(colnames(dtm.tidy))))

})

test_that("dtm_builder error/message if last row is blank", {


    my.corpus <- data.frame(
        my_text = c("I hear babies crying I watch them grow",
                    "They’ll learn much more than I'll ever know",
                    "And I think to myself",
                    "What a wonderful world",
                    "Yes I think to myself",
                    "What a wonderful world"),
        line_id = paste0("line", 1:6))

    my.corpus$my_text[6] <- ""

    expect_error(
        expect_message( dtm_builder(my.corpus, my_text, line_id) ) )

})


test_that("dtm_builder chunks correctly", {

    chunk <- 3L
    dtm.e <- corpus |>
             dtm_builder(text, doc_id, chunk = chunk)
    expect_equal(sum(dtm.e[1,]), chunk)

    dtm.f <- corpus |>
        dtm_builder(text, doc_id)
    expect_equal(sum(dtm.e), sum(dtm.f))

    chunk <- 100L
    dtm.g <- corpus |>
        dtm_builder(text, doc_id, chunk = chunk)
    expect_equal(sum(dtm.g[1,]), sum(dtm.f))

})

# tests for internal functions
test_that(".convert_dtm_to_dgCMatrix convert all dtms to dgCMatrix", {

    ## base R matrix ##
    expect_s4_class(.convert_dtm_to_dgCMatrix(dtm.bse), "dgCMatrix")
    ## dgCMatrix matrix ##
    expect_s4_class(.convert_dtm_to_dgCMatrix(dtm.dgc), "dgCMatrix")
    ## dfm//dgCMatrix matrix ##
    expect_s4_class(.convert_dtm_to_dgCMatrix(dtm.dfm), "dgCMatrix")
    ## tm//simple_triplet_matrix matrix ##
    expect_s4_class(.convert_dtm_to_dgCMatrix(dtm.tm), "dgCMatrix")
    ## TermDocumentMatrix //  tm//simple_triplet_matrix matrix ##
    expect_s4_class(.convert_dtm_to_dgCMatrix(dtm.tdm), "dgCMatrix")

})

test_that("dtm resampler creates DTM of the same dimensions", {


    out <- dtm_resampler(dtm.dgc, alpha=0.2)
    expect_s4_class(out, "dgCMatrix")
    expect_identical(dim(out), dim(dtm.dgc))
    expect_equal(sum(out), sum(dtm.dgc)*0.2, tolerance=0.1)

    out <- dtm_resampler(dtm.dgc, alpha=0.5)
    expect_s4_class(out, "dgCMatrix")
    expect_identical(dim(out), dim(dtm.dgc))
    expect_equal(sum(out), sum(dtm.dgc)*0.5, tolerance=0.1)

    out <- dtm_resampler(dtm.dgc, alpha=0.7)
    expect_s4_class(out, "dgCMatrix")
    expect_identical(dim(out), dim(dtm.dgc))
    expect_equal(sum(out), sum(dtm.dgc)*0.7, tolerance=0.1)

    out <- dtm_resampler(dtm.dgc, alpha=1)
    expect_s4_class(out, "dgCMatrix")
    expect_identical(dim(out), dim(dtm.dgc))
    expect_equal(sum(out), sum(dtm.dgc)*1, tolerance=0.1)


})


test_that("dtm resampler works on output of .prep_cmd_INPUT", {

    ## dgCMatrix matrix ##
    out <- .prep_cmd_INPUT(dtm=dtm.dgc[,1:35],
                            cw = cw.oov,
                            cv = NULL,
                            wv = fake_word_vectors_oov,
                            missing = "stop")

    dtm.samp <- dtm_resampler(out$DTM, alpha=1)
    expect_s4_class(dtm.samp, "dgCMatrix")
    expect_equal(nrow(dtm.samp), nrow(out$DTM))
    expect_equal(ncol(dtm.samp), ncol(out$DTM))

})
