test_that("check perm_tester errors", {

    testthat::skip_on_cran()

    data(meta_shakespeare)
    data <- meta_shakespeare
    null_model <- lm(body_count ~ 1, data = data)

    expect_error(out1 <- perm_tester(
        data = data,
        model = null_model,
        perm_var = "body_count",
        statistic = "coefficients",
        perm_n = 500,
        alternative = "two.sided",
        alpha = .01,
        seed = 8675309
    ))

})


test_that("does perm_tester worky", {

  testthat::skip_on_cran()

  data(meta_shakespeare)
  data <- meta_shakespeare
  model <- lm(body_count ~ boas_problem_plays + year + genre, data = data)

  # without stratified permutations, two-sided test
  out1 <- perm_tester(
    data = data,
    model = model,
    perm_var = "body_count",
    statistic = "coefficients",
    perm_n = 500,
    alternative = "two.sided",
    alpha = .01,
    seed = 8675309
  )

  # with stratified permutations, two-sided test
  out2 <- perm_tester(
    data = data,
    model = model,
    perm_var = "body_count",
    strat_var = "boas_problem_plays",
    statistic = "coefficients",
    perm_n = 500,
    alternative = "two.sided",
    alpha = .01,
    seed = 8675309
  )

  # left-tailed test
  out3 <- perm_tester(
      data = data,
      model = model,
      perm_var = "body_count",
      statistic = "coefficients",
      perm_n = 500,
      alternative = "left",
      alpha = .01,
      seed = 8675309
    )

  # right-tailed test
  out4 <- perm_tester(
      data = data,
      model = model,
      perm_var = "body_count",
      statistic = "coefficients",
      perm_n = 500,
      alternative = "right",
      alpha = .01,
      seed = 8675309
    )

  # all
  out5 <- perm_tester(
      data = data,
      model = model,
      perm_var = "body_count",
      statistic = "coefficients",
      perm_n = 500,
      alternative = "all",
      alpha = .01,
      seed = 8675309
    )

  # without stratified permutations, two-sided test, different seed
  out6 <- perm_tester(
      data = data,
      model = model,
      perm_var = "body_count",
      statistic = "coefficients",
      perm_n = 500,
      alternative = "all",
      alpha = .01,
      seed = 59801
  )

  # without stratified permutations, two-sided test,no perm_var
  out7 <- perm_tester(
      data = data,
      model = model,
      statistic = "coefficients",
      perm_n = 500,
      alternative = "two.sided",
      alpha = .01,
      seed = 8675309
  )

  expect_equal(dim(out1), as.integer(c(length(coef(model)), 6L)) )
  expect_equal(dim(out2), as.integer(c(length(coef(model)), 6L)) )
  expect_equal(dim(out3), as.integer(c(length(coef(model)), 6L)) )
  expect_equal(dim(out4), as.integer(c(length(coef(model)), 6L)) )
  expect_equal(dim(out5), as.integer(c(length(coef(model)), 14L)) )
  expect_type(out1, "list")
  expect_type(out2, "list")
  expect_type(out3, "list")
  expect_type(out4, "list")
  expect_type(out5, "list")
  expect_equal(out1$CI_two_lo, out5$CI_two_lo)
  expect_equal(out1$CI_two_up, out5$CI_two_up)
  expect_equal(out3$CI_left_lo, out5$CI_left_lo)
  expect_false(all(out6 == out5))
  expect_equal(out1, out7)


})

test_that("does rancor_builder worky", {

  testthat::skip_on_cran()

  dtm <- dtm_builder(
    data = my_corpus,
    text = clean_text,
    doc_id = line_id
  )
  df <- data.frame(
    vocab = colnames(dtm),
    freqs = colSums(dtm)
  )
  # convert to probabilities
  df$probs <- df$freqs / sum(df$freqs)
  # create random DTM
  rDTM1 <- df |> rancor_builder(vocab, probs)
  rDTM2 <- df |> rancor_builder(vocab, probs)
  rDTM3 <- df |> rancor_builder(vocab, probs, seed = 59801)
  rDTM4 <- df |> rancor_builder(vocab, probs, seed = 59801)

  expect_equal(dim(rDTM1), c(100L, 24L))
  expect_equal(dim(rDTM2), c(100L, 24L))
  expect_equal(dim(rDTM3), c(100L, 24L))
  expect_equal(dim(rDTM4), c(100L, 24L))

  expect_false(all(rDTM1 == rDTM2))
  expect_true(all(rDTM3 == rDTM4))


  rDTM5 <- df |> rancor_builder(vocab, probs, 
                            n_docs = 1000L,
                            len_mean = 700,
                            len_var = 10L,
                            len_min = 20L,
                            len_max = 1000L)

  rDTM6 <- df |> rancor_builder(vocab, probs, 
                            n_docs = 1050L,
                            len_mean = 700,
                            len_var = 10L,
                            len_min = 20L,
                            len_max = 1000L)

  expect_equal(dim(rDTM5), c(1000L, 24L))
  expect_equal(dim(rDTM6), c(1050L, 24L))



 })

test_that("rancor_builder returns sparse matrix with seed reproducibility", {

  dtm <- dtm_builder(
    data = my_corpus,
    text = clean_text,
    doc_id = line_id
  )
  df <- data.frame(
    vocab = colnames(dtm),
    freqs = colSums(dtm)
  )
  df$probs <- df$freqs / sum(df$freqs)

  r1 <- df |> rancor_builder(vocab, probs, n_docs = 20L, seed = 42)
  r2 <- df |> rancor_builder(vocab, probs, n_docs = 20L, seed = 42)

  expect_s4_class(r1, "dgCMatrix")
  expect_true(all(r1 == r2))
  expect_equal(nrow(r1), 20L)
  expect_equal(ncol(r1), ncol(dtm))
})

test_that("rancor_builder stores seed attribute", {

  dtm <- dtm_builder(
    data = my_corpus,
    text = clean_text,
    doc_id = line_id
  )
  df <- data.frame(
    vocab = colnames(dtm),
    freqs = colSums(dtm)
  )
  df$probs <- df$freqs / sum(df$freqs)

  r <- df |> rancor_builder(vocab, probs, n_docs = 10L, seed = 99)

  expect_true("seed" %in% names(attributes(r)))
  expect_equal(attr(r, "seed"), 99)
})



test_that("does rancors_builder worky", {

  testthat::skip_on_cran()

  dtm <- dtm_builder(
    data = my_corpus,
    text = clean_text,
    doc_id = line_id
  )
  df <- data.frame(
    vocab = colnames(dtm),
    freqs = colSums(dtm)
  )
  # convert to probabilities
  df$probs <- df$freqs / sum(df$freqs)

  # create random DTM
  ls_dtms <- df |> 
  rancors_builder(vocab,
      probs,
    n_cors = 20,
      n_docs = 100,
      len_mean = c(50, 200),
      len_var = 5,
      len_min = 20,
      len_max = 1000,
      seed = 59801
  )
  expect_identical(length(ls_dtms), 20L)
  expect_true(all(lengths(ls_dtms) == (length(df$vocab) * 100)))

  ls_dtms2 <- df |> 
  rancors_builder(vocab,
      probs,
    n_cors = 20,
      n_docs = 100,
      len_mean = c(50, 200),
      len_var = 5,
      len_min = 20,
      len_max = 1000,
      seed = 59801
  )

  ls_dtms3 <- df |> 
  rancors_builder(vocab,
      probs,
    n_cors = 20,
      n_docs = 100,
      len_mean = c(50, 200),
      len_var = 5,
      len_min = 20,
      len_max = 1000,
      seed = 59802
  )

  expect_identical(ls_dtms, ls_dtms2)
  expect_false(all(ls_dtms[[1]] == ls_dtms3[[1]]))

  ls_dtms4 <- df |> 
  rancors_builder(vocab,
      probs,
    n_cors = 20,
      n_docs = 100,
      len_mean = c(50, 200),
      len_var = c(5, 20),
      len_min = 20,
      len_max = 1000,
      seed = 59802
  )

  expect_equal(length(ls_dtms4), 20)
  expect_type(ls_dtms4, "list")
  expect_type(ls_dtms4[[1]], "S4")
  expect_s4_class(ls_dtms4[[1]], "dgCMatrix")

  expect_true(min(rowSums(ls_dtms4[[1]])) > 20)
  expect_true(max(rowSums(ls_dtms4[[1]])) < 1000)
  expect_true(min(rowSums(ls_dtms4[[10]])) > 20)
  expect_true(max(rowSums(ls_dtms4[[10]])) < 1000)


})

test_that("perm_tester errors on intercept-only model", {

  data(meta_shakespeare)
  null_model <- lm(body_count ~ 1, data = meta_shakespeare)

  expect_error(
    perm_tester(
      data = meta_shakespeare,
      model = null_model,
      perm_var = "body_count",
      statistic = "coefficients",
      perm_n = 10,
      alternative = "two.sided",
      alpha = .01,
      seed = 8675309
    ),
    "intercept-only"
  )
})

test_that("perm_tester works with a model with exactly 2 coefficients", {
    # data with only 2 columns (perm_var + 1 predictor) used to collapse
    # `data[, !names(data) %in% perm_var]` to an unnamed vector (drop=TRUE
    # default), losing the predictor's column name and crashing with
    # `object 'x' not found` when the permuted model was refit
    set.seed(1)
    df <- data.frame(y = rnorm(30), x = rnorm(30))
    model <- lm(y ~ x, data = df)

    out <- perm_tester(
        data = df,
        model = model,
        statistic = "coefficients",
        perm_n = 20,
        seed = 8675309
    )

    expect_s3_class(out, "data.frame")
    expect_equal(nrow(out), length(coef(model)))
})

test_that("perm_tester gives a clear error when statistic is not a model component", {
  data(meta_shakespeare)
  model <- lm(body_count ~ boas_problem_plays + year, data = meta_shakespeare)

  # an invalid `statistic` must not fall through to an opaque rowSums()
  # failure deep inside the tail-probability computation
  expect_error(
    perm_tester(
      data = meta_shakespeare,
      model = model,
      statistic = "not_a_real_component",
      perm_n = 10,
      seed = 8675309
    ),
    "`statistic`",
    fixed = TRUE
  )
})

test_that("perm_tester two.sided returns expected structure", {

  data(meta_shakespeare)
  model <- lm(body_count ~ boas_problem_plays + year, data = meta_shakespeare)

  out <- perm_tester(
    data = meta_shakespeare,
    model = model,
    statistic = "coefficients",
    perm_n = 20,
    alternative = "two.sided",
    alpha = .05,
    seed = 8675309
  )

  expect_s3_class(out, "data.frame")
  expect_true("stat" %in% colnames(out))
  expect_true("P_two" %in% colnames(out))
  expect_equal(nrow(out), length(coef(model)))
})

test_that("perm_tester alternative=all returns all p-value columns", {

  data(meta_shakespeare)
  model <- lm(body_count ~ boas_problem_plays + year, data = meta_shakespeare)

  out <- perm_tester(
    data = meta_shakespeare,
    model = model,
    statistic = "coefficients",
    perm_n = 20,
    alternative = "all",
    alpha = .05,
    seed = 8675309
  )

  expect_true("P_two" %in% colnames(out))
  expect_true("P_left" %in% colnames(out))
  expect_true("P_right" %in% colnames(out))
})

test_that("rancors_builder returns list of sparse matrices with seed reproducibility", {

  dtm <- dtm_builder(
    data = my_corpus,
    text = clean_text,
    doc_id = line_id
  )
  df <- data.frame(
    vocab = colnames(dtm),
    freqs = colSums(dtm)
  )
  df$probs <- df$freqs / sum(df$freqs)

  ls1 <- df |> rancors_builder(vocab, probs,
    n_cors = 3, n_docs = 50, len_mean = 100,
    len_var = 5, len_min = 20, len_max = 500,
    seed = 42
  )
  ls2 <- df |> rancors_builder(vocab, probs,
    n_cors = 3, n_docs = 50, len_mean = 100,
    len_var = 5, len_min = 20, len_max = 500,
    seed = 42
  )

  expect_type(ls1, "list")
  expect_equal(length(ls1), 3L)
  expect_s4_class(ls1[[1]], "dgCMatrix")
  expect_true(all(ls1[[1]] == ls2[[1]]))
})

test_that("rancor_builder restores .Random.seed after exit", {
  dtm <- dtm_builder(
    data = my_corpus,
    text = clean_text,
    doc_id = line_id
  )
  df <- data.frame(
    vocab = colnames(dtm),
    freqs = colSums(dtm)
  )
  df$probs <- df$freqs / sum(df$freqs)

  set.seed(42)
  pre <- .Random.seed
  df |> rancor_builder(vocab, probs, n_docs = 10L, seed = 99)
  post <- .Random.seed
  expect_identical(pre, post)
})

test_that("rancor_builder does not leak RNG state when seed = NULL", {
  dtm <- dtm_builder(
    data = my_corpus,
    text = clean_text,
    doc_id = line_id
  )
  df <- data.frame(
    vocab = colnames(dtm),
    freqs = colSums(dtm)
  )
  df$probs <- df$freqs / sum(df$freqs)

  set.seed(42)
  pre <- .Random.seed
  df |> rancor_builder(vocab, probs, n_docs = 10L)
  post <- .Random.seed
  expect_identical(pre, post)
})

test_that("perm_tester does not modify .Random.seed when seed=NULL", {
  set.seed(42)
  pre <- .Random.seed
  perm_tester(
    data = meta_shakespeare,
    model = lm(body_count ~ year, data = meta_shakespeare),
    statistic = "coefficients",
    perm_n = 5,
    seed = NULL
  )
  post <- .Random.seed
  expect_identical(pre, post)
})

test_that("perm_tester restores .Random.seed when seed is set", {
  set.seed(42)
  pre <- .Random.seed
  perm_tester(
    data = meta_shakespeare,
    model = lm(body_count ~ year, data = meta_shakespeare),
    statistic = "coefficients",
    perm_n = 5,
    seed = 12345
  )
  post <- .Random.seed
  expect_identical(pre, post)
})

test_that("perm_tester works when .Random.seed does not exist yet", {
  had_seed <- exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)
  old_seed <- if (had_seed) get(".Random.seed", envir = .GlobalEnv) else NULL
  on.exit({
    if (had_seed) {
      assign(".Random.seed", old_seed, envir = .GlobalEnv)
    } else if (exists(".Random.seed", envir = .GlobalEnv, inherits = FALSE)) {
      rm(".Random.seed", envir = .GlobalEnv)
    }
  }, add = TRUE)

  if (had_seed) rm(".Random.seed", envir = .GlobalEnv)

  expect_no_error(
    perm_tester(
      data = meta_shakespeare,
      model = lm(body_count ~ year, data = meta_shakespeare),
      statistic = "coefficients",
      perm_n = 5,
      seed = 42
    )
  )
})

test_that("rancors_builder restores .Random.seed after exit", {
  dtm <- dtm_builder(
    data = my_corpus,
    text = clean_text,
    doc_id = line_id
  )
  df <- data.frame(
    vocab = colnames(dtm),
    freqs = colSums(dtm)
  )
  df$probs <- df$freqs / sum(df$freqs)

  set.seed(42)
  pre <- .Random.seed
  df |>
    rancors_builder(
      vocab, probs, n_cors = 3, n_docs = 20,
      len_mean = 100, len_var = 5, len_min = 20,
      len_max = 500, seed = 77
    )
  post <- .Random.seed
  expect_identical(pre, post)
})

test_that("rancors_builder does not leak RNG state when seed = NULL", {
  dtm <- dtm_builder(
    data = my_corpus,
    text = clean_text,
    doc_id = line_id
  )
  df <- data.frame(
    vocab = colnames(dtm),
    freqs = colSums(dtm)
  )
  df$probs <- df$freqs / sum(df$freqs)

  set.seed(42)
  pre <- .Random.seed
  df |> rancors_builder(
    vocab, probs, n_cors = 3, n_docs = 20,
    len_mean = 100, len_var = 5, len_min = 20, len_max = 500
  )
  post <- .Random.seed
  expect_identical(pre, post)
})

