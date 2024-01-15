test_that("check perm_tester errors", {

    testthat::skip_on_cran()

    data <- text2map::meta_shakespeare
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

  data <- text2map::meta_shakespeare
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

