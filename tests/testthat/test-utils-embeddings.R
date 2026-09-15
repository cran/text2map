
test_that(".check_term_in_embeddings works on different data types", {
    ## single list of terms ##
    # character list
    terms <- c("choose", "moon")
    out <- .check_term_in_embeddings(
        terms = terms,
        wv = fake_word_vectors,
        action = "remove"
    )
    expect_identical(out, terms)
    # actual list list
    terms <- list(c("choose", "moon"))
    out <- .check_term_in_embeddings(
        terms = terms,
        wv = fake_word_vectors,
        action = "remove"
    )
    expect_identical(out, terms)
    # data.frame
    terms <- data.frame(add = c("choose", "moon"))
    out <- .check_term_in_embeddings(
        terms = terms,
        wv = fake_word_vectors,
        action = "remove"
    )
    expect_identical(out, terms)
    # tibble
    terms <- tibble::tibble(add = c("choose", "moon"))
    out <- .check_term_in_embeddings(
        terms = terms,
        wv = fake_word_vectors,
        action = "remove"
    )
    expect_identical(out, terms)

    ## paired list of terms ##
    # actual list list
    terms <- list(
        add = c("choose", "moon"),
        sub = c("decade", "this")
    )
    out <- .check_term_in_embeddings(
        terms = terms,
        wv = fake_word_vectors,
        action = "remove"
    )
    expect_identical(out, terms)
    # data.frame
    terms <- data.frame(
        add = c("choose", "moon"),
        sub = c("decade", "this")
    )
    out <- .check_term_in_embeddings(
        terms = terms,
        wv = fake_word_vectors,
        action = "remove"
    )
    expect_identical(out, terms)
    # tibble
    terms <- tibble::tibble(
        add = c("choose", "moon"),
        sub = c("decade", "this")
    )
    out <- .check_term_in_embeddings(
        terms = terms,
        wv = fake_word_vectors,
        action = "remove"
    )
    expect_identical(out, terms)
})

test_that(".check_term_in_embeddings removes words on different data types", {
    ## single list of terms ##
    # character list
    terms <- c("choose", "moon", "picklespit")
    expect_message(out <- .check_term_in_embeddings(
        terms = terms,
        wv = fake_word_vectors,
        action = "remove"
    ))

    "The following (and any associated terms) removed
    because there are no matching word vectors: picklespit"

    expect_identical(
        out,
        c("choose", "moon")
    )

    # actual list list
    terms <- list(c("choose", "moon", "picklespit"))
    expect_message(out <- .check_term_in_embeddings(
        terms = terms,
        wv = fake_word_vectors,
        action = "remove"
    ))
    expect_identical(
        out,
        list(c("choose", "moon"))
    )

    # data.frame
    terms <- data.frame(add = c("choose", "moon", "picklespit"))
    expect_message(out <- .check_term_in_embeddings(
        terms = terms,
        wv = fake_word_vectors,
        action = "remove"
    ))
    expect_identical(
        out,
        data.frame(add = c("choose", "moon"))
    )

    # tibble
    terms <- tibble::tibble(add = c("choose", "moon", "picklespit"))
    expect_message(out <- .check_term_in_embeddings(
        terms = terms,
        wv = fake_word_vectors,
        action = "remove"
    ))
    expect_identical(
        out,
        tibble::tibble(add = c("choose", "moon"))
    )

    ## paired list of terms ##
    # actual list list
    terms <- list(
        add = c("choose", "moon", "picklespit", "decade"),
        sub = c("decade", "this", "choose", "picklespit")
    )
    expect_message(out <- .check_term_in_embeddings(
        terms = terms,
        wv = fake_word_vectors,
        action = "remove"
    ))
    expect_identical(
        out,
        list(
            add = c("choose", "moon", "decade"),
            sub = c("decade", "this", "choose")
        )
    )
    # data.frame
    terms <- data.frame(
        add = c("choose", "moon", "picklespit", "decade"),
        sub = c("decade", "this", "choose", "picklespit")
    )
    expect_message(out <- .check_term_in_embeddings(
        terms = terms,
        wv = fake_word_vectors,
        action = "remove"
    ))
    expect_identical(
        out,
        data.frame(
            add = c("choose", "moon"),
            sub = c("decade", "this")
        )
    )

    # tibble
    terms <- tibble::tibble(
        add = c("choose", "moon", "picklespit", "decade"),
        sub = c("decade", "this", "choose", "picklespit")
    )
    expect_message(out <- .check_term_in_embeddings(
        terms = terms,
        wv = fake_word_vectors,
        action = "remove"
    ))
    expect_identical(
        out,
        tibble::tibble(
            add = c("choose", "moon"),
            sub = c("decade", "this")
        )
    )
})


test_that(".check_term_in_embeddings removing all words will stop", {
    ## single list of terms ##
    # character list
    terms <- c("picklespit")
    expect_error(
        .check_term_in_embeddings(
            terms = terms,
            wv = fake_word_vectors,
            action = "remove"
        ),
        "The following have no matching word vectors: picklespit"
    )

    # actual list list
    terms <- list(c("picklespit"))

    expect_error(
        .check_term_in_embeddings(
            terms = terms,
            wv = fake_word_vectors,
            action = "remove"
        ),
        "The following have no matching word vectors: picklespit"
    )

    # data.frame
    terms <- data.frame(add = c("picklespit"))
    expect_error(
        .check_term_in_embeddings(
            terms = terms,
            wv = fake_word_vectors,
            action = "remove"
        ),
        "The following have no matching word vectors: picklespit"
    )

    # tibble
    terms <- tibble::tibble(add = c("picklespit"))
    expect_error(
        .check_term_in_embeddings(
            terms = terms,
            wv = fake_word_vectors,
            action = "remove"
        ),
        "The following have no matching word vectors: picklespit"
    )

    ## paired list of terms ##

    # actual list list
    terms <- list(
        add = c("picklespit", "mulepants"),
        sub = c("choose", "picklespit")
    )
    expect_error(
        .check_term_in_embeddings(
            terms = terms,
            wv = fake_word_vectors,
            action = "remove"
        ),
        "The following have no matching word vectors: picklespit"
    )

    # data.frame
    terms <- data.frame(
        add = c("picklespit", "decade"),
        sub = c("choose", "picklespit")
    )
    expect_error(
        out <- .check_term_in_embeddings(
            terms = terms,
            wv = fake_word_vectors,
            action = "remove"
        ),
        "The following have no matching word vectors: picklespit"
    )

    # tibble
    terms <- tibble::tibble(
        add = c("picklespit", "decade"),
        sub = c("choose", "picklespit")
    )
    expect_error(
        out <- .check_term_in_embeddings(
            terms = terms,
            wv = fake_word_vectors,
            action = "remove"
        ),
        "The following have no matching word vectors: picklespit"
    )


    # actual list list -- works on the opposite side too...
    terms <- list(
        add = c("choose", "picklespit"),
        sub = c("mulepants", "picklespit")
    )
    expect_error(
        .check_term_in_embeddings(
            terms = terms,
            wv = fake_word_vectors,
            action = "remove"
        ),
        "The following have no matching word vectors: picklespit"
    )
})


test_that(".check_term_in_embeddings only prints 10 bad words", {
    terms <- c(
        "choose", "picklespit", "mulepants", "pandaboots",
        "rhinojumps", "penguinland", "tigersoda", "wildrumpus",
        "weirdal", "boul", "jawn", "handcrank", "quink"
    )
    expect_message(
        .check_term_in_embeddings(
            terms = terms,
            wv = fake_word_vectors,
            action = "remove"
        ),
        "2 more term"
    )
})

test_that(".check_term_in_embeddings rejects invalid action", {
    expect_error(
        .check_term_in_embeddings(
            terms = c("moon", "picklespit"),
            wv = fake_word_vectors,
            action = "invalid"
        ),
        class = "simpleError"
    )
})

test_that(".check_term_in_embeddings truncates stop message at 10 words", {
    terms <- c(
        "choose", "picklespit", "mulepants", "pandaboots",
        "rhinojumps", "penguinland", "tigersoda", "wildrumpus",
        "weirdal", "boul", "jawn", "handcrank", "quink"
    )
    expect_error(
        .check_term_in_embeddings(
            terms = terms,
            wv = fake_word_vectors,
            action = "stop"
        ),
        "2 more term"
    )
})

test_that(".check_term_in_embeddings stops when words
            missing on different data types", {
    er.msg <- "The following have no matching word vectors: picklespit"

    ## single list of terms ##
    # character list
    terms <- c("choose", "moon", "picklespit")
    expect_error(.check_term_in_embeddings(
        terms = terms,
        wv = fake_word_vectors,
        action = "stop"
    ), er.msg)

    # actual list list
    terms <- list(c("choose", "moon", "picklespit"))
    expect_error(.check_term_in_embeddings(
        terms = terms,
        wv = fake_word_vectors,
        action = "stop"
    ), er.msg)

    # data.frame
    terms <- data.frame(add = c("choose", "moon", "picklespit"))
    expect_error(.check_term_in_embeddings(
        terms = terms,
        wv = fake_word_vectors,
        action = "stop"
    ), er.msg)

    # tibble
    terms <- tibble::tibble(add = c("choose", "moon", "picklespit"))
    expect_error(.check_term_in_embeddings(
        terms = terms,
        wv = fake_word_vectors,
        action = "stop"
    ), er.msg)

    ## paired list of terms ##
    # actual list list
    terms <- list(
        add = c("choose", "moon", "picklespit", "decade"),
        sub = c("decade", "this", "choose", "picklespit")
    )
    expect_error(.check_term_in_embeddings(
        terms = terms,
        wv = fake_word_vectors,
        action = "stop"
    ), er.msg)

    # data.frame
    terms <- data.frame(
        add = c("choose", "moon", "picklespit", "decade"),
        sub = c("decade", "this", "choose", "picklespit")
    )
    expect_error(.check_term_in_embeddings(
        terms = terms,
        wv = fake_word_vectors,
        action = "stop"
    ), er.msg)

    # tibble
    terms <- tibble::tibble(
        add = c("choose", "moon", "picklespit", "decade"),
        sub = c("decade", "this", "choose", "picklespit")
    )

    expect_error(.check_term_in_embeddings(
        terms = terms,
        wv = fake_word_vectors,
        action = "stop"
    ), er.msg)
})


test_that("find_projection will stop if lengths differ", {
    vector <- fake_word_vectors[2, 1:5] # choose

    expect_error(find_projection(
        fake_word_vectors,
        vector
    ))
})

test_that("find_rejection will stop if lengths differ", {
    vector <- fake_word_vectors[2, 1:5] # choose

    expect_error(find_rejection(
        fake_word_vectors,
        vector
    ))
})

test_that("find_projection errors with a clear message on an all-zero vector", {
    vector <- rep(0, ncol(fake_word_vectors))

    expect_error(
        find_projection(fake_word_vectors, vector),
        "zero"
    )
})

test_that("find_rejection errors with a clear message on an all-zero vector", {
    vector <- rep(0, ncol(fake_word_vectors))

    expect_error(
        find_rejection(fake_word_vectors, vector),
        "zero"
    )
})

test_that("find_projection produces matrix with correct dimensions", {
    vector <- fake_word_vectors[2, ]

    project <- find_projection(fake_word_vectors, vector)

    expect_identical(length(vector), ncol(fake_word_vectors))
    expect_identical(ncol(fake_word_vectors), ncol(project))

    expect_identical(nrow(fake_word_vectors), nrow(project))
})

test_that("find_rejection produces matrix with correct dimensions", {
    vector <- fake_word_vectors[2, ]

    reject <- find_rejection(fake_word_vectors, vector)

    expect_identical(length(vector), ncol(fake_word_vectors))
    expect_identical(ncol(fake_word_vectors), ncol(reject))

    expect_identical(nrow(fake_word_vectors), nrow(reject))
})

test_that("find_tranformation, dimensions and names", {
    norm <- find_transformation(
        wv = fake_word_vectors,
        method = "norm"
    )
    center <- find_transformation(
        wv = fake_word_vectors,
        method = "center"
    )
    align <- find_transformation(
        wv = fake_word_vectors,
        ref = fake_word_vectors,
        method = "align"
    )

    fake_vectors_dgc <- methods::as(
        fake_word_vectors,
        "dgCMatrix"
    )

    norm.dgc <- find_transformation(
        wv = fake_vectors_dgc,
        method = "norm"
    )
    center.dgc <- find_transformation(
        wv = fake_vectors_dgc,
        method = "center"
    )
    align.dgc <- find_transformation(
        wv = fake_vectors_dgc,
        ref = fake_vectors_dgc,
        method = "align"
    )

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

test_that("find_tranformation, dimensions and names", {
    
    tcm.approx <- text2vec::sim2(fake_word_vectors, method = "cosine")
    vocab <- base::intersect(rownames(tcm_dgc), rownames(tcm.approx))
    tcm_dgc.b <- tcm_dgc[vocab, vocab]
    tcm.approx <- tcm.approx[vocab, vocab]
    base <- mean((tcm.approx - tcm_dgc.b)^2)

    retro <- find_transformation(
        wv = fake_word_vectors,
        ref = tcm_dgc,
        method = "retrofit"
    )

    tcm.approx <- text2vec::sim2(retro, method = "cosine")
    tcm_dgc.b <- tcm_dgc[vocab, vocab]
    tcm.approx <- tcm.approx[vocab, vocab]
    post <- mean((tcm.approx - tcm_dgc.b)^2)

    retro <- find_transformation(
        wv = retro,
        ref = tcm_dgc,
        method = "retrofit"
    )

    tcm.approx <- text2vec::sim2(retro, method = "cosine")
    tcm_dgc.b <- tcm_dgc[vocab, vocab]
    tcm.approx <- tcm.approx[vocab, vocab]
    postb <- mean((tcm.approx - tcm_dgc.b)^2)

    expect_true(base > post)
    expect_true(post > postb)

    expect_identical(dim(retro), dim(fake_word_vectors))
})

test_that("find_projection + find_rejection sum equals original", {
    vec <- fake_word_vectors[2, ]
    proj <- find_projection(fake_word_vectors, vec)
    rej <- find_rejection(fake_word_vectors, vec)

    # proj + rej should approximate the original
    reconstructed <- proj + rej
    expect_true(max(abs(as.matrix(reconstructed) - fake_word_vectors)) < 1e-10)
})

test_that("find_projection produces parallel vectors", {
    vec <- fake_word_vectors[2, ]
    proj <- find_projection(fake_word_vectors, vec)

    # each row of proj should be proportional to vec
    for (i in seq_len(nrow(proj))) {
        ratio <- as.numeric(proj[i, ]) / as.numeric(vec)
        expect_true(sd(ratio) < 1e-10)
    }
})

test_that("find_rejection produces orthogonal vectors", {
    vec <- fake_word_vectors[2, ]
    rej <- find_rejection(fake_word_vectors, vec)

    dots <- as.matrix(rej) %*% as.numeric(vec)
    expect_true(max(abs(dots)) < 1e-10)
})

test_that("find_transformation norm produces unit-length rows", {
    normed <- find_transformation(fake_word_vectors, method = "norm")
    norms <- sqrt(rowSums(normed^2))
    expect_true(all(abs(norms - 1) < 1e-10))
})

test_that("find_transformation center produces zero-mean columns", {
    centered <- find_transformation(fake_word_vectors, method = "center")
    col_means <- colMeans(as.matrix(centered))
    expect_true(all(abs(col_means) < 1e-10))
})

test_that("get_centroid with missing=remove drops OOV terms", {
    anchors_oov <- c("choose", "picklespit", "moon")
    result <- suppressWarnings(suppressMessages(get_centroid(
        anchors = anchors_oov, wv = fake_word_vectors, missing = "remove"
    )))
    expect_type(result, "double")
    expect_true(nrow(result) == 1)
})

test_that("get_centroid with compound terms splits and averages", {
    anchors <- c("choose moon")
    result <- get_centroid(anchors = anchors, wv = fake_word_vectors)
    manual <- (fake_word_vectors["choose", ] + fake_word_vectors["moon", ]) / 2
    expect_equal(as.numeric(result), as.numeric(manual), tolerance = 1e-10)
})

test_that("get_regions returns correct dimensions and names", {
    skip_if_not_installed("ClusterR")
    regions <- get_regions(wv = fake_word_vectors, k_regions = 3L, seed = 123)

    expect_identical(dim(regions), c(3L, ncol(fake_word_vectors)))
    expect_true(grepl("^region_", rownames(regions)[1]))
    expect_s4_class(regions, "dgCMatrix")
})

test_that("get_regions gives a clear error when k_regions exceeds nrow(wv)", {
    skip_if_not_installed("ClusterR")
    # ClusterR::KMeans_arma doesn't raise an R condition when clusters >
    # nrow(data); it prints a console message and returns a character
    # string in place of a matrix, which get_regions() didn't validate,
    # so it fell through to an unrelated-looking `invalid 'data'` error
    # several lines later
    expect_error(
        get_regions(wv = fake_word_vectors, k_regions = nrow(fake_word_vectors) + 1L),
        "k_regions"
    )
})


## weight_ppmi tests ##

test_that("weight_ppmi computes correct PPMI on a known small TCM", {
  tcm <- matrix(c(2, 1, 0,
                  1, 3, 1,
                  0, 1, 2), nrow = 3, byrow = TRUE,
                dimnames = list(c("a", "b", "c"), c("a", "b", "c")))

  N <- sum(tcm)
  rsum <- rowSums(tcm)
  csum <- colSums(tcm)
  expected <- matrix(0, nrow = 3, ncol = 3,
                     dimnames = list(c("a", "b", "c"), c("a", "b", "c")))
  for (i in 1:3) for (j in 1:3) {
    if (tcm[i, j] > 0) {
      pmi <- log((tcm[i, j] / N) / ((rsum[i] / N) * (csum[j] / N)))
      expected[i, j] <- max(pmi, 0)
    }
  }

  res <- weight_ppmi(tcm)
  expect_s4_class(res, "dgCMatrix")
  expect_identical(dim(res), c(3L, 3L))
  expect_equal(as.matrix(res), expected, tolerance = 1e-12)
})

test_that("weight_ppmi gives equivalent results for dense and sparse inputs", {
  tcm_dense <- matrix(c(2, 1, 0, 1, 3, 1, 0, 1, 2), nrow = 3,
                      dimnames = list(c("a", "b", "c"), c("a", "b", "c")))
  tcm_sparse <- methods::as(tcm_dense, "dgCMatrix")

  res_d <- weight_ppmi(tcm_dense)
  res_s <- weight_ppmi(tcm_sparse)

  expect_equal(as.matrix(res_d), as.matrix(res_s), tolerance = 1e-12)
})

test_that("weight_ppmi preserves dimnames", {
  tcm <- matrix(c(2, 1, 0, 1, 3, 1, 0, 1, 2), nrow = 3,
                dimnames = list(c("a", "b", "c"), c("a", "b", "c")))
  res <- weight_ppmi(tcm)
  expect_identical(rownames(res), c("a", "b", "c"))
  expect_identical(colnames(res), c("a", "b", "c"))
})

test_that("weight_ppmi returns a dgCMatrix", {
  tcm <- matrix(c(2, 1, 0, 1, 3, 1, 0, 1, 2), nrow = 3)
  res <- weight_ppmi(tcm)
  expect_s4_class(res, "dgCMatrix")
})

test_that("weight_ppmi handles all-zero rows without NaN or -Inf", {
  tcm <- matrix(c(0, 0, 0,
                  1, 2, 1,
                  0, 1, 1), nrow = 3, byrow = TRUE,
                dimnames = list(c("z", "b", "c"), c("z", "b", "c")))
  res <- weight_ppmi(tcm)
  vals <- as.vector(as.matrix(res))
  expect_true(all(is.finite(vals)))
  expect_true(all(vals >= 0))
  expect_true(all(as.vector(as.matrix(res)[1, ]) == 0))
})

test_that("weight_ppmi smooth argument shifts the distribution", {
  tcm <- matrix(c(2, 1, 0, 1, 3, 1, 0, 1, 2), nrow = 3,
                dimnames = list(c("a", "b", "c"), c("a", "b", "c")))
  res0 <- weight_ppmi(tcm, smooth = 0)
  res1 <- weight_ppmi(tcm, smooth = 1)
  # both finite, non-negative, same shape
  expect_true(all(is.finite(as.vector(as.matrix(res0)))))
  expect_true(all(is.finite(as.vector(as.matrix(res1)))))
  # smoothing meaningfully changes the values
  expect_false(isTRUE(all.equal(as.vector(as.matrix(res0)),
                                as.vector(as.matrix(res1)))))
  # smooth=1e-10 runs without error (negligible shift)
  expect_s4_class(weight_ppmi(tcm, smooth = 1e-10), "dgCMatrix")
})

test_that("weight_ppmi errors on non-matrix input", {
  expect_error(weight_ppmi(1:5), class = "rlang_error")
  expect_error(weight_ppmi("foo"), class = "rlang_error")
})

test_that("weight_ppmi errors on negative or non-scalar smooth", {
  tcm <- matrix(c(2, 1, 0, 1, 3, 1, 0, 1, 2), nrow = 3)
  expect_error(weight_ppmi(tcm, smooth = -1), class = "rlang_error")
  expect_error(weight_ppmi(tcm, smooth = c(0, 1)), class = "rlang_error")
})

test_that("weight_ppmi round-trip on tcm_dgc fixture", {
  skip_if_not(exists("tcm_dgc"), "tcm_dgc fixture not available")
  res <- weight_ppmi(tcm_dgc)
  expect_s4_class(res, "dgCMatrix")
  expect_identical(dim(res), dim(tcm_dgc))
  expect_identical(rownames(res), rownames(tcm_dgc))
  expect_identical(colnames(res), colnames(tcm_dgc))
  vals <- as.vector(as.matrix(res))
  expect_true(all(is.finite(vals)))
  expect_true(all(vals >= 0))
})
