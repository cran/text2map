test_that("get_anchor returns data object", {

    gend <- get_anchors(relation="gender")
    expect_equal(ncol(gend), 2L)

})

test_that("get_centroid works on different data types", {

    ## single c() list of terms ##
    out.c <- get_centroid(anchors=anchor.solo.c,
                          wv=fake_word_vectors)

    expect_type(out.c, "double")
    expect_identical(dim(out.c),  ce.dims)
    expect_identical(rownames(out.c), ce.name)

    ## order of single c() list of terms doesn't matter ##
    out.d <- get_centroid(anchors=anchor.solo.d,
                          wv=fake_word_vectors)

    expect_identical(out.c[1], out.d[1])

    # actual list list
    out <- get_centroid(anchors=anchor.solo.list,
                        wv=fake_word_vectors)

    expect_type(out, "double")
    expect_identical(dim(out),  ce.dims)
    expect_identical(rownames(out), ce.name)
    expect_identical(out.c[1], out[1])

    # data.frame
    out <- get_centroid(anchors=anchor.solo.df,
                        wv=fake_word_vectors)
    expect_type(out, "double")
    expect_identical(dim(out),  ce.dims)
    expect_identical(rownames(out), ce.name)
    expect_identical(out.c[1], out[1])

    # tibble
    out <- get_centroid(anchors=anchor.solo.tbl,
                        wv=fake_word_vectors)

    expect_type(out, "double")
    expect_identical(dim(out),  ce.dims)
    expect_identical(rownames(out), ce.name)
    expect_identical(out.c[1], out[1])

    ## paired list of terms ##
    ## get_centroid should only use first lsit

    # actual list list
    out <- get_centroid(anchors=anchor.pair.list,
                             wv=fake_word_vectors)
    expect_type(out, "double")
    expect_identical(dim(out),  ce.dims)
    expect_identical(rownames(out), ce.name)
    expect_identical(out.c[1], out[1])

    # data.frame
    out <- get_centroid(anchors=anchor.pair.df,
                             wv=fake_word_vectors)
    expect_type(out, "double")
    expect_identical(dim(out),  ce.dims)
    expect_identical(rownames(out), ce.name)
    expect_identical(out.c[1], out[1])

    # tibble
    out <- get_centroid(anchors=anchor.pair.tbl,
                             wv=fake_word_vectors)
    expect_type(out, "double")
    expect_identical(dim(out),  ce.dims)
    expect_identical(rownames(out), ce.name)
    expect_identical(out.c[1], out[1])

})


test_that("get_direction works on different data types", {

    ## paired list of terms ##
    # actual list list
    out <- get_direction(anchors=anchor.pair.list,
                        wv=fake_word_vectors)
    expect_type(out, "double")
    expect_identical(dim(out),  di.dims)
    expect_identical(rownames(out), di.name)

    # data.frame
    out <- get_direction(anchors=anchor.pair.df,
                        wv=fake_word_vectors)
    expect_type(out, "double")
    expect_identical(dim(out),  di.dims)
    expect_identical(rownames(out), di.name)

    # tibble
    out <- get_direction(anchors=anchor.pair.tbl,
                         wv=fake_word_vectors)
    expect_type(out, "double")
    expect_identical(dim(out),  di.dims)
    expect_identical(rownames(out), di.name)

})

test_that("get_direction errors if only one set of terms", {

    out <- get_direction(anchors=anchor.pair.list,
                            wv=fake_word_vectors,
                            method="paired")
    expect_type(out, "double")
    expect_identical(dim(out),  di.dims)
    expect_identical(rownames(out), di.name)

    out <- get_direction(anchors=anchor.pair.list,
                            wv=fake_word_vectors,
                            method="pooled")
    expect_type(out, "double")
    expect_identical(dim(out),  di.dims)
    expect_identical(rownames(out), di.name)

    out <- get_direction(anchors=anchor.pair.list,
                            wv=fake_word_vectors,
                            method="L2")
    expect_type(out, "double")
    expect_identical(dim(out),  di.dims)
    expect_identical(rownames(out), di.name)

    out <- get_direction(anchors=anchor.pair.list,
                         wv=fake_word_vectors,
                         method="PCA")
    expect_type(out, "double")
    expect_identical(dim(out),  di.dims)
    expect_identical(rownames(out), di.name)

    expect_error(get_direction(anchors=anchor.pair.list,
                            wv=fake_word_vectors,
                            method="not_a_method"),
                 "method must be 'pooled', 'paired', 'L2' or 'PCA'")

})

test_that("check if different get_direction methods work", {

    expect_error( get_direction(anchors=anchor.solo.list,
                                wv=fake_word_vectors),
                  "get_direction requires two sets of juxtaposing terms")

    expect_error( get_direction(anchors=anchor.solo.df,
                                wv=fake_word_vectors),
                  "get_direction requires two sets of juxtaposing terms")

    expect_error( get_direction(anchors=anchor.solo.tbl,
                                wv=fake_word_vectors),
                  "get_direction requires two sets of juxtaposing terms")

})

test_that(".check_term_in_embeddings works on different data types", {

    ## single list of terms ##
    # character list
    terms <- c("choose", "moon")
    out <- .check_term_in_embeddings(terms=terms,
                                     wv=fake_word_vectors,
                                     action="remove")
    expect_identical(out, terms)
    # actual list list
    terms <- list(c("choose", "moon"))
    out <- .check_term_in_embeddings(terms=terms,
                                     wv=fake_word_vectors,
                                     action="remove")
    expect_identical(out, terms)
    # data.frame
    terms <- data.frame(add = c("choose", "moon"))
    out <- .check_term_in_embeddings(terms=terms,
                                     wv=fake_word_vectors,
                                     action="remove")
    expect_identical(out, terms)
    # tibble
    terms <- tibble::tibble(add = c("choose", "moon"))
    out <- .check_term_in_embeddings(terms=terms,
                                     wv=fake_word_vectors,
                                     action="remove")
    expect_identical(out, terms)

    ## paired list of terms ##
    # actual list list
    terms <- list(add=c("choose", "moon"),
                  sub=c("decade", "this"))
    out <- .check_term_in_embeddings(terms=terms,
                                     wv=fake_word_vectors,
                                     action="remove")
    expect_identical(out, terms)
    # data.frame
    terms <- data.frame(add=c("choose", "moon"),
                        sub=c("decade", "this"))
    out <- .check_term_in_embeddings(terms=terms,
                                     wv=fake_word_vectors,
                                     action="remove")
    expect_identical(out, terms)
    # tibble
    terms <- tibble::tibble(add=c("choose", "moon"),
                            sub=c("decade", "this"))
    out <- .check_term_in_embeddings(terms=terms,
                                     wv=fake_word_vectors,
                                     action="remove")
    expect_identical(out, terms)

})

test_that(".check_term_in_embeddings removes words on different data types", {

    ## single list of terms ##
    # character list
    terms <- c("choose", "moon", "picklespit")
    expect_message(out <- .check_term_in_embeddings(terms=terms,
                                     wv=fake_word_vectors,
                                     action="remove"))

    "The following (and any associated terms) removed
    because there are no matching word vectors: picklespit"

    expect_identical(out,
                     c("choose", "moon"))

    # actual list list
    terms <- list(c("choose", "moon", "picklespit"))
    expect_message(out <- .check_term_in_embeddings(terms=terms,
                                     wv=fake_word_vectors,
                                     action="remove"))
    expect_identical(out,
                     list(c("choose", "moon")))

    # data.frame
    terms <- data.frame(add = c("choose", "moon", "picklespit"))
    expect_message(out <- .check_term_in_embeddings(terms=terms,
                                     wv=fake_word_vectors,
                                     action="remove"))
    expect_identical(out,
                     data.frame(add = c("choose", "moon")))

    # tibble
    terms <- tibble::tibble(add = c("choose", "moon", "picklespit"))
    expect_message(out <- .check_term_in_embeddings(terms=terms,
                                     wv=fake_word_vectors,
                                     action="remove"))
    expect_identical(out,
                     tibble::tibble(add = c("choose", "moon")))

    ## paired list of terms ##
    # actual list list
    terms <- list(add=c("choose", "moon","picklespit", "decade"),
                  sub=c("decade", "this", "choose", "picklespit"))
    expect_message(out <- .check_term_in_embeddings(terms=terms,
                                     wv=fake_word_vectors,
                                     action="remove"))
    expect_identical(out,
                     list(add=c("choose", "moon", "decade"),
                          sub=c("decade", "this", "choose")))
    # data.frame
    terms <- data.frame(add=c("choose", "moon","picklespit", "decade"),
                        sub=c("decade", "this", "choose", "picklespit"))
    expect_message(out <- .check_term_in_embeddings(terms=terms,
                                     wv=fake_word_vectors,
                                     action="remove"))
    expect_identical(out,
                     data.frame(add=c("choose", "moon"),
                                sub=c("decade", "this")) )

    # tibble
    terms <- tibble::tibble(add=c("choose", "moon","picklespit", "decade"),
                            sub=c("decade", "this", "choose", "picklespit"))
    expect_message(out <- .check_term_in_embeddings(terms=terms,
                                     wv=fake_word_vectors,
                                     action="remove"))
    expect_identical(out,
                     tibble::tibble(add=c("choose", "moon"),
                                    sub=c("decade", "this")) )


})


test_that(".check_term_in_embeddings removing all words will stop", {

    ## single list of terms ##
    # character list
    terms <- c("picklespit")
    expect_error(
        .check_term_in_embeddings(terms=terms,
                                  wv=fake_word_vectors,
                                  action="remove"),
        "The following have no matching word vectors: picklespit")

    # actual list list
    terms <- list(c("picklespit"))

    expect_error(
        .check_term_in_embeddings(terms=terms,
                                  wv=fake_word_vectors,
                                  action="remove"),
        "The following have no matching word vectors: picklespit")

    # data.frame
    terms <- data.frame(add = c("picklespit"))
    expect_error(
        .check_term_in_embeddings(terms=terms,
                                  wv=fake_word_vectors,
                                  action="remove"),
        "The following have no matching word vectors: picklespit")

    # tibble
    terms <- tibble::tibble(add = c("picklespit"))
    expect_error(
        .check_term_in_embeddings(terms=terms,
                                  wv=fake_word_vectors,
                                  action="remove"),
        "The following have no matching word vectors: picklespit")

    ## paired list of terms ##

    # actual list list
    terms <- list(add=c("picklespit", "mulepants"),
                  sub=c("choose", "picklespit"))
    expect_error(
        .check_term_in_embeddings(terms=terms,
                                  wv=fake_word_vectors,
                                  action="remove"),
        "The following have no matching word vectors: picklespit")

    # data.frame
    terms <- data.frame(add=c("picklespit", "decade"),
                        sub=c("choose", "picklespit"))
    expect_error(
        out <- .check_term_in_embeddings(terms=terms,
                                         wv=fake_word_vectors,
                                         action="remove"),
        "The following have no matching word vectors: picklespit")

    # tibble
    terms <- tibble::tibble(add=c("picklespit", "decade"),
                            sub=c("choose", "picklespit"))
    expect_error(
        out <- .check_term_in_embeddings(terms=terms,
                                         wv=fake_word_vectors,
                                         action="remove"),
        "The following have no matching word vectors: picklespit")


    # actual list list -- works on the opposite side too...
    terms <- list(add=c("choose", "picklespit"),
                  sub=c("mulepants", "picklespit"))
    expect_error(
        .check_term_in_embeddings(terms=terms,
                                  wv=fake_word_vectors,
                                  action="remove"),
        "The following have no matching word vectors: picklespit")

})


test_that(".check_term_in_embeddings only prints 10 bad words", {

    ## single list of terms ##
    # character list
    terms <- c("choose", "picklespit", "mulepants", "pandaboots",
               "rhinojumps", "penguinland", "tigersoda", "wildrumpus",
               "weirdal", "boul", "jawn", "handcrank", "quink")
    expect_message(
    out <- .check_term_in_embeddings(terms=terms,
                                  wv=fake_word_vectors,
                                  action="remove"))
    expect_identical(out, "choose")

})


test_that(".check_term_in_embeddings stops when words
            missing on different data types", {

    er.msg <- "The following have no matching word vectors: picklespit"

    ## single list of terms ##
    # character list
    terms <- c("choose", "moon", "picklespit")
    expect_error(.check_term_in_embeddings(terms=terms,
                                     wv=fake_word_vectors,
                                     action="stop"),  er.msg)

    # actual list list
    terms <- list(c("choose", "moon", "picklespit"))
    expect_error(.check_term_in_embeddings(terms=terms,
                                     wv=fake_word_vectors,
                                     action="stop"),  er.msg)

    # data.frame
    terms <- data.frame(add = c("choose", "moon", "picklespit"))
    expect_error(.check_term_in_embeddings(terms=terms,
                                     wv=fake_word_vectors,
                                     action="stop"), er.msg)

    # tibble
    terms <- tibble::tibble(add = c("choose", "moon", "picklespit"))
    expect_error(.check_term_in_embeddings(terms=terms,
                                     wv=fake_word_vectors,
                                     action="stop"), er.msg)

    ## paired list of terms ##
    # actual list list
    terms <- list(add=c("choose", "moon","picklespit", "decade"),
                  sub=c("decade", "this", "choose", "picklespit"))
    expect_error(.check_term_in_embeddings(terms=terms,
                                     wv=fake_word_vectors,
                                     action="stop"),  er.msg)

    # data.frame
    terms <- data.frame(add=c("choose", "moon","picklespit", "decade"),
                        sub=c("decade", "this", "choose", "picklespit"))
    expect_error(.check_term_in_embeddings(terms=terms,
                                     wv=fake_word_vectors,
                                     action="stop"),  er.msg)

    # tibble
    terms <- tibble::tibble(add=c("choose", "moon","picklespit", "decade"),
                            sub=c("decade", "this", "choose", "picklespit"))

    expect_error(.check_term_in_embeddings(terms=terms,
                                     wv=fake_word_vectors,
                                     action="stop"), er.msg)

})

test_that("get_regions seed works", {

    my.regions <- get_regions(
        wv = fake_word_vectors,
        k_regions = 20L,
        max_iter = 10L,
        algorithm = "hamerly",
        seed = 442
    )

    my.regions2 <- get_regions(
        wv = fake_word_vectors,
        k_regions = 20L,
        max_iter = 10L,
        algorithm = "hamerly",
        seed = 442
    )

    expect_identical(my.regions,  my.regions2)

})

test_that("find_projection will stop if lengths differ", {

    vector <- fake_word_vectors[2,1:5] #choose

    expect_error(find_projection(fake_word_vectors,
                                 vector))

})

test_that("find_rejection will stop if lengths differ", {

    vector <- fake_word_vectors[2,1:5] #choose

    expect_error(find_rejection(fake_word_vectors,
                                 vector))

})

test_that("find_projection produces matrix with correct dimensions", {

    vector <- fake_word_vectors[2,]

    project <- find_projection(fake_word_vectors, vector)

    expect_identical(length(vector), ncol(fake_word_vectors), ncol(project))

    expect_identical(nrow(fake_word_vectors), nrow(project))

})

test_that("find_rejection produces matrix with correct dimensions", {

    vector <- fake_word_vectors[2,]

    reject <- find_rejection(fake_word_vectors, vector)

    expect_identical(length(vector), ncol(fake_word_vectors), ncol(reject))

    expect_identical(nrow(fake_word_vectors), nrow(reject))

})

test_that("find_tranformation, dimensions and names", {

    norm <- find_transformation(wv=fake_word_vectors,
                                 method = "norm")
    center <- find_transformation(wv=fake_word_vectors,
                                 method = "center" )
    align <- find_transformation(wv=fake_word_vectors,
                                 ref=fake_word_vectors,
                                 method = "align" )

    fake_vectors_dgc <- methods::as(fake_word_vectors,
                                    "dgCMatrix")

    norm.dgc <- find_transformation(wv=fake_vectors_dgc,
                                method = "norm")
    center.dgc  <- find_transformation(wv=fake_vectors_dgc,
                                  method = "center" )
    align.dgc  <- find_transformation(wv=fake_vectors_dgc,
                                 ref=fake_vectors_dgc,
                                 method = "align" )

    expect_identical(dim(norm), dim(fake_word_vectors))
    expect_identical(dim(center), dim(fake_word_vectors))
    expect_identical(dim(align), dim(fake_word_vectors))

    expect_identical(rownames(norm), rownames(fake_word_vectors))
    expect_identical(rownames(center), rownames(fake_word_vectors))
    expect_identical(rownames(align), rownames(fake_word_vectors))

    expect_identical(dim(norm), dim(norm.dgc))
    expect_identical(dim(center), dim(center.dgc))
    expect_identical(dim(align), dim(align.dgc))

    expect_identical(rownames(norm), rownames(norm.dgc))
    expect_identical(rownames(center), rownames(center.dgc))
    expect_identical(rownames(align), rownames(align.dgc))

})

