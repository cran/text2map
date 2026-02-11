
test_that("CoCA prepare", {
  testthat::skip_on_cran()

    # classes <- CoCA(dtm=dtm_bse,
    #                 wv=fake_word_vectors_coca,
    #                 directions = sem_dirs,
    #                 filter_sig = TRUE,
    #                 filter_value = 0.05,
    #                 zero_action = 'drop')
    # coca.msg <- "CoCA found 2 schematic classes in the corpus. Sizes: 5 5"
    # coca.msg.d <- "CoCA found 2 schematic classes in the corpus. Sizes: 6 4"

    # Make a degenerate class
    expect_warning(classes.d <- CoCA(
          dtm = dtm_bse,
          wv = fake_word_vectors_coca,
          directions = sem_dirs,
          filter_sig = TRUE,
          filter_value = 0.05,
          zero_action = 'drop'
          ))

    expect_s3_class(classes.d, "CoCA")

    
})


test_that("CoCA works on different data types", {

  testthat::skip_on_cran()

  # Get for CoCA print message
  cor_dims <- c(
      as.integer(nrow(dtm_bse)),
      as.integer(nrow(dtm_bse))
  )

  ## base R matrix ##
  expect_warning(classes <- CoCA(
    dtm = dtm_bse,
    wv = fake_word_vectors_coca,
    directions = sem_dirs,
    filter_sig = TRUE,
    filter_value = 0.05,
    zero_action = "drop"
  ))

  expect_named(classes, c("membership", "modules", "cormat"))
  expect_equal(length(classes$membership), nrow(dtm_bse))
  expect_named(classes$modules[[1]], c("cmds", "cormat", "degenerate"))
  expect_equal(
    ncol(classes$modules[[1]]$cmds),
    nrow(sem_dirs)
  )
  expect_equal(
    ncol(classes$modules[[1]]$cormat),
    nrow(sem_dirs)
  )
  expect_equal(dim(classes$cormat), cor_dims)

  ## dgCMatrix matrix ##
  expect_warning(classes <- CoCA(
    dtm = dtm_dgc,
    wv = fake_word_vectors_coca,
    directions = sem_dirs,
    filter_sig = TRUE,
    filter_value = 0.05,
    zero_action = "drop"
  ))

  expect_named(classes, c("membership", "modules", "cormat"))
  expect_equal(length(classes$membership), nrow(dtm_bse))
  expect_named(classes$modules[[1]], c("cmds", "cormat", "degenerate"))
  expect_equal(
    ncol(classes$modules[[1]]$cmds),
    nrow(sem_dirs)
  )
  expect_equal(
    ncol(classes$modules[[1]]$cormat),
    nrow(sem_dirs)
  )
  expect_equal(dim(classes$cormat), cor_dims)

})


test_that("CoCA works on tm/quanteda data types", {

  testthat::skip_on_cran()
  testthat::skip_if_not_installed("tm")
  testthat::skip_if_not_installed("quanteda")
  require(quanteda)
  require(tm)

  # Get for CoCA print message
  cor_dims <- c(
      as.integer(nrow(dtm_bse)),
      as.integer(nrow(dtm_bse))
  )

  ## dfm//dgCMatrix matrix ##
  expect_warning(classes <- CoCA(
    dtm = dtm_dfm,
    wv = fake_word_vectors_coca,
    directions = sem_dirs,
    filter_sig = TRUE,
    filter_value = 0.05,
    zero_action = "drop"
  ))

  expect_named(classes, c("membership", "modules", "cormat"))
  expect_equal(length(classes$membership), nrow(dtm_bse))
  expect_named(classes$modules[[1]], c("cmds", "cormat", "degenerate"))
  expect_equal(
    ncol(classes$modules[[1]]$cmds),
    nrow(sem_dirs)
  )
  expect_equal(
    ncol(classes$modules[[1]]$cormat),
    nrow(sem_dirs)
  )
  expect_equal(dim(classes$cormat), cor_dims)

  ## tm//simple_triplet_matrix matrix ##
  expect_warning(classes <- CoCA(
    dtm = dtm_tm,
    wv = fake_word_vectors_coca,
    directions = sem_dirs,
    filter_sig = TRUE,
    filter_value = 0.05,
    zero_action = "drop"
  ))

  expect_named(classes, c("membership", "modules", "cormat"))
  expect_equal(length(classes$membership), nrow(dtm_bse))
  expect_named(classes$modules[[1]], c("cmds", "cormat", "degenerate"))
  expect_equal(
    ncol(classes$modules[[1]]$cmds),
    nrow(sem_dirs)
  )
  expect_equal(
    ncol(classes$modules[[1]]$cormat),
    nrow(sem_dirs)
  )
  expect_equal(dim(classes$cormat), cor_dims)

})


test_that("CoCA prints a solution", {

  testthat::skip_on_cran()


  ## base R matrix ##
  expect_warning(classes <- CoCA(
    dtm = dtm_bse,
    wv = fake_word_vectors_coca,
    directions = sem_dirs,
    filter_sig = TRUE,
    filter_value = 0.05,
    zero_action = "drop"
  ))

  expect_output(print(classes))
  # expect_output(print(classes), coca.msg)
})

test_that("CoCA plot method", {

  testthat::skip_on_cran()

  classes_p <- CoCA(
    dtm = dtm_bse,
    wv = fake_word_vectors_coca,
    directions = sem_dirs,
    filter_sig = FALSE,
    filter_value = 0.05,
    zero_action = "drop"
  )

  p <- plot(classes_p, module = 1)

  expect_identical(
    p$Arguments$labels,
    rownames(sem_dirs)
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
