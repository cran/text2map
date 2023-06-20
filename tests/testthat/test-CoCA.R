test_that("CoCA works on different data types", {
  testthat::skip_on_cran()

  ## base R matrix ##
  classes <- CoCA(
    dtm = dtm.bse,
    wv = fake_word_vectors_coca,
    directions = sem.dirs,
    filter_sig = TRUE,
    filter_value = 0.05,
    zero_action = "drop"
  )

  expect_named(classes, c("membership", "modules", "cormat"))
  expect_equal(length(classes$membership), nrow(dtm.bse))
  expect_named(classes$modules[[1]], c("cmds", "cormat", "degenerate"))
  expect_equal(
    ncol(classes$modules[[1]]$cmds),
    nrow(sem.dirs)
  )
  expect_equal(
    ncol(classes$modules[[1]]$cormat),
    nrow(sem.dirs)
  )
  expect_equal(dim(classes$cormat), cor.dims)

  ## dgCMatrix matrix ##
  classes <- CoCA(
    dtm = dtm.dgc,
    wv = fake_word_vectors_coca,
    directions = sem.dirs,
    filter_sig = TRUE,
    filter_value = 0.05,
    zero_action = "drop"
  )

  expect_named(classes, c("membership", "modules", "cormat"))
  expect_equal(length(classes$membership), nrow(dtm.bse))
  expect_named(classes$modules[[1]], c("cmds", "cormat", "degenerate"))
  expect_equal(
    ncol(classes$modules[[1]]$cmds),
    nrow(sem.dirs)
  )
  expect_equal(
    ncol(classes$modules[[1]]$cormat),
    nrow(sem.dirs)
  )
  expect_equal(dim(classes$cormat), cor.dims)

  ## dfm//dgCMatrix matrix ##
  classes <- CoCA(
    dtm = dtm.dfm,
    wv = fake_word_vectors_coca,
    directions = sem.dirs,
    filter_sig = TRUE,
    filter_value = 0.05,
    zero_action = "drop"
  )

  expect_named(classes, c("membership", "modules", "cormat"))
  expect_equal(length(classes$membership), nrow(dtm.bse))
  expect_named(classes$modules[[1]], c("cmds", "cormat", "degenerate"))
  expect_equal(
    ncol(classes$modules[[1]]$cmds),
    nrow(sem.dirs)
  )
  expect_equal(
    ncol(classes$modules[[1]]$cormat),
    nrow(sem.dirs)
  )
  expect_equal(dim(classes$cormat), cor.dims)

  ## tm//simple_triplet_matrix matrix ##
  classes <- CoCA(
    dtm = dtm.tm,
    wv = fake_word_vectors_coca,
    directions = sem.dirs,
    filter_sig = TRUE,
    filter_value = 0.05,
    zero_action = "drop"
  )

  expect_named(classes, c("membership", "modules", "cormat"))
  expect_equal(length(classes$membership), nrow(dtm.bse))
  expect_named(classes$modules[[1]], c("cmds", "cormat", "degenerate"))
  expect_equal(
    ncol(classes$modules[[1]]$cmds),
    nrow(sem.dirs)
  )
  expect_equal(
    ncol(classes$modules[[1]]$cormat),
    nrow(sem.dirs)
  )
  expect_equal(dim(classes$cormat), cor.dims)
})

test_that("CoCA prints a solution", {

  testthat::skip_on_cran()


  ## base R matrix ##
  classes <- CoCA(
    dtm = dtm.bse,
    wv = fake_word_vectors_coca,
    directions = sem.dirs,
    filter_sig = TRUE,
    filter_value = 0.05,
    zero_action = "drop"
  )

  expect_output(print(classes))
  # expect_output(print(classes), coca.msg)
})

test_that("CoCA plot", {

  testthat::skip_on_cran()

  classes <- CoCA(
    dtm = dtm.bse,
    wv = fake_word_vectors_coca,
    directions = sem.dirs,
    filter_sig = TRUE,
    filter_value = 0.05,
    zero_action = "drop"
  )

  p <- plot(classes, module = 1)

  expect_identical(
    p$Arguments$labels,
    rownames(sem.dirs)
  )
  expect_identical(
    p$Arguments$esize,
    8
  )
  expect_identical(
    p$Arguments$alpha,
    0.05
  )
})
