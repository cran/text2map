#' Monte Carlo Permutation Tests for Model P-Values
#'
#' `perm_tester()` carries out Monte Carlo permutation tests for model
#' p-values from two-tailed, left-tailed, and/or right-tailed hypothesis
#' testing.
#'
#' @details
#'
#' `perm_tester()` can be used to derive p-values under the randomization
#' model of inference. There are various reasons one might want to do this---
#' with text data, and observational data more generally, this might be
#' because the corpus/sample is not a random sample from a target population.
#' In such cases, population model p-values might not make much sense since
#' the asymptotically-derived standard errors from which they are constructed
#' themselves do not make sense. We might therefore want to make inferences
#' on the basis of whether or not randomness, as a data-generating mechanism,
#' might reasonably account for a statistic at least as extreme as the one
#' we observed. `perm_tester()` works from this idea.
#'
#' `perm_tester()` works like this. First, the model (supplied the `model`
#' parameter) is run on the observed data. Second, we take some statistic of
#' interest, which we indicate with the `statistic` parameter, and set it to
#' the side. Third, a variable, `perm_var`, is permuted---meaning the observed
#' values for the rows of `data` on `perm_var` are randomly reshuffled. Fourth,
#' we estimate the model again, this time with the permuted `perm_var`. Fifth,
#' we get grab that same `statistic`. We repeat steps two through
#' five a total of `perm_n` times, each time tallying the number of times the
#' `statistic` from the permutation-derived model is greater than or equal to
#' (for a right-tailed test), less-than or equal to (for a left-tailed test),
#' and/or has an absolute value greater than or equal to (for a two-tailed test)
#' the `statistic` from the "real" model.
#'
#' If we divide those tallies by the total number of permutations, then we
#' get randomization-based p-values. This is what `perm_tester()` does. The
#' null hypothesis is that randomness could likely generate the statistic
#' that we observe. The alternative hypothesis is that randomness alone likely
#' can't account for the observed statistic.
#'
#' We then reject the null hypothesis if the p-value is below a threshold indicated
#' with `alpha`, which, as in population-based inference, is the probability
#' below which we are willing to reject the null hypothesis when it is actually
#' true. So if the p-value is below, say, `alpha` = 0.05 and we're performing,
#' a right-tailed test, then fewer than 5% of the statistics derived from the
#' permutation-based models are greater than or equal to our observed
#' statistic. We would then reject the null, as it is unlikely (based on our `alpha`
#' threshold), that randomness as a data-generating mechanism can account
#' for a test statistic at least as large the one we observed.
#'
#' In most cases, analysts probably cannot expect to perform "exact" permutation
#' tests where every possible permutation is accounted for---i.e., where
#' `perm_n` equals the total number of possible permutations. Instead, we
#' can take random samples of the "population" of permutations. `perm_tester()`
#' does this, and reports the standard errors and (1 - `alpha`) confidence
#' intervals for the p-values.
#'
#' `perm_tester()` can also perform stratified permutation tests, where the observed
#' `perm_var` variables within groups. This can be done by setting the `strat_var`
#' variable to be he grouping variable.
#'
#' @references
#' Taylor, Marshall A. (2020)
#' 'Visualization Strategies for Regression Estimates with Randomization
#' Inference' \emph{Stata Journal} 20(2):309-335.
#' \doi{10.1177/1536867X20930999}.\cr
#'
#' #' Darlington, Richard B. and Andrew F. Hayes (2016)
#' \emph{Regression analysis and linear models: Concepts, applications, and implementation}.
#' Guilford Publications.\cr
#'
#' Ernst, Michael D. (2004)
#' 'permutation methods: a basis for exact inference' \emph{Statistical Scicence}
#' 19(4):676-685.
#' \doi{10.1214/088342304000000396}.\cr
#'
#' Manly, Bryan F. J. (2007)
#' \emph{Randomization, Bootstrap and Monte Carlo Methods in Biology}.
#' Chapman and Hall/CRC.
#' \doi{10.1201/9781315273075}.\cr
#'
#' @name perm_tester
#' @author Marshall Taylor and Dustin Stoltz
#'
#' @param data The dataframe from which the model is estimated.
#'
#' @param model The model which will be estimated and re-estimated.
#'
#' @param perm_var The variable in the model that will be permuted.
#'                  Default is `NULL` which takes the first `Y`n term
#'                  in the formula of the model
#'
#' @param strat_var Categorical variable for within-stratum permutations.
#'                  Defaults to `NULL`.
#'
#' @param statistic The name of the model statistic you want to "grab" after
#'                  re-running the model with each permutation to compare to
#'                  the original model statistic.
#'
#' @param perm_n The total number of permutations. Defaults to 1000.
#'
#' @param alternative The alternative hypothesis. One of `"two.sided"` 
#'                     (default),
#'                    `"left"`, `"right"`, and `"all"`. Defaults to `"all"`,
#'                    which reports the p-value statistics for all three
#'                    alternative hypotheses.
#'
#' @param alpha Alpha level for the hypothesis test. Defaults to 0.05.
#'
#' @param seed Optional seed for reproducibility of the p-value statistics.
#'             Defaults to null.
#'
#' @returns Returns a data frame with the observed statistic (`stat`), the
#'          p-values (`P_left`, for left-tailed, `P_right` for right-tailed, 
#'          and/or
#'          `P_two` for two-tailed), and the standard errors and confidence
#'          intervals for those p-values, respectively.
#'
#' @importFrom permute how
#' @importFrom permute Plots
#' @importFrom permute shuffle
#' @importFrom tibble as_tibble
#' @importFrom tibble rownames_to_column
#' @importFrom Matrix update
#' @importFrom stats qnorm
#'
#' @seealso [CMDist], [CoCA], [get_direction]
#'
#' @examples
#' \donttest{
#' data <- text2map::meta_shakespeare
#'
#' model <- lm(body_count ~ boas_problem_plays + year + genre, data = data)
#'
#' # without stratified permutations, two-sided test
#' out1 <- perm_tester(
#'   data = data,
#'   model = model,
#'   statistic = "coefficients",
#'   perm_n = 40,
#'   alternative = "two.sided",
#'   alpha = .01,
#'   seed = 8675309
#' )
#'
#' # with stratified permutations, two-sided test
#' out2 <- perm_tester(
#'   data = data,
#'   model = model,
#'   strat_var = "boas_problem_plays",
#'   statistic = "coefficients",
#'   perm_n = 40,
#'   alternative = "two.sided",
#'   alpha = .01,
#'   seed = 8675309
#' )
#' }
#'
#' @export
perm_tester <- function(data,
                        model,
                        perm_var = NULL,
                        strat_var = NULL,
                        statistic,
                        perm_n = 1000,
                        alternative = "all",
                        alpha = 0.05,
                        seed = NULL) {
  if (length(stats::coef(model)) == 1L) {
    stop("The intercept for an intercept-only model is
    the univariate mean of the outcome variable, so
    permutations will not lead to different and
    the p-values will intercepts always equal 1.")
  }

  if (is.null(perm_var)) {
    # extract Y if no terms provided
    perm_var <- as.character(attributes(model$terms)$variables[[2]])
  }

  set.seed(seed)
  perms <- data.frame(matrix(ncol = perm_n, nrow = nrow(data)))
  colnames(perms) <- rep(perm_var, perm_n)
  perm_list <- list()

  if (is.null(strat_var) == FALSE) {
    control <- permute::how(plots = permute::Plots(strata = data[[strat_var]]))

    for (i in seq_len(perm_n)) {
      perms[, i] <- data[[perm_var]][permute::shuffle(length(data[[perm_var]]),
        control = control
      )]

      perm_list[[i]] <- cbind(perms[i], data[, !names(data) %in% perm_var])
    }
  }

  if (is.null(strat_var) == TRUE) {
    for (i in seq_len(perm_n)) {
      perms[, i] <- data[[perm_var]][permute::shuffle(length(data[[perm_var]]))]

      perm_list[[i]] <- cbind(perms[i], data[, !names(data) %in% perm_var])
    }
  }

  models <- list()

  for (i in seq_along(perm_list)) {
    models[[i]] <- Matrix::update(model, . ~ ., data = perm_list[[i]])
  }

  perm_mat <- do.call(cbind, lapply(models, FUN = `[[`, statistic))

  mat <- data.frame(matrix(ncol = 1, nrow = length(model[[statistic]])))
  colnames(mat) <- "stat"
  rownames(mat) <- names(model[[statistic]])
  mat$stat <- model[[statistic]]

  if (alternative %in% c("all", "two.sided")) {
    mat$P_two <- rowSums(abs(perm_mat) >= abs(model[[statistic]])) / perm_n
    mat$SE_two <- sqrt((mat$P_two * (1 - mat$P_two)) / perm_n)
    mat$CI_two_lo <- mat$P_two - (stats::qnorm(alpha / 2,
      mean = 0,
      sd = 1,
      lower.tail = FALSE
    ) * mat$SE_two)
    mat$CI_two_up <- mat$P_two + (stats::qnorm(alpha / 2,
      mean = 0,
      sd = 1,
      lower.tail = FALSE
    ) * mat$SE_two)
  }

  if (alternative %in% c("all", "left")) {
    mat$P_left <- rowSums(perm_mat <= model[[statistic]]) / perm_n
    mat$SE_left <- sqrt((mat$P_left * (1 - mat$P_left)) / perm_n)
    mat$CI_left_lo <- mat$P_left - (stats::qnorm(alpha / 2,
      mean = 0,
      sd = 1,
      lower.tail = FALSE
    ) * mat$SE_left)
    mat$CI_left_up <- mat$P_left + (stats::qnorm(alpha / 2,
      mean = 0,
      sd = 1,
      lower.tail = FALSE
    ) * mat$SE_left)
  }

  if (alternative %in% c("all", "right")) {
    mat$P_right <- rowSums(perm_mat >= model[[statistic]]) / perm_n
    mat$SE_right <- sqrt((mat$P_right * (1 - mat$P_right)) / perm_n)
    mat$CI_right_lo <- mat$P_right - (stats::qnorm(alpha / 2,
      mean = 0,
      sd = 1,
      lower.tail = FALSE
    ) * mat$SE_right)
    mat$CI_right_up <- mat$P_right + (stats::qnorm(alpha / 2,
      mean = 0,
      sd = 1,
      lower.tail = FALSE
    ) * mat$SE_right)
  }

  mat <- mat |>
    tibble::as_tibble(rownames = NA) |>
    tibble::rownames_to_column()

  return(mat)
}


#' Build a Random Corpus
#'
#' `rancor_builder()` generates a random corpus (rancor) based on a user
#' defined term probabilities and vocabulary. Users can set the number of
#' documents, as well as the mean, standard deviation, minimum, and maximum
#' document lengths (i.e., number of tokens) of the parent normal distribution
#' from which the document lengths are randomly sampled. The output is a single
#' document-term matrix. To produce multiple random corpora, use
#' `rancors_builder()` (note the plural). Term probabilities/vocabulary can
#' come from a users own corpus, or a pre-compiled frequency list, such
#' as the one derived from the Google Book N-grams corpus
#'
#'
#' @name rancor_builder
#' @author Dustin Stoltz and Marshall Taylor
#'
#' @importFrom Matrix sparseMatrix
#' @importFrom fastmatch fmatch
#' @importFrom dplyr pull
#'
#' @param data Data.frame containing vocabulary and probabilities
#' @param vocab Name of the column containing vocabulary
#' @param probs Name of the column containing probabilities
#' @param n_docs Integer indicating the number of documents to be returned
#' @param len_mean Integer indicating the mean of the document lengths in the
#'                parent normal sampling distribution
#' @param len_var Integer indicating the standard deviation of the 
#'                document lengths in the parent normal sampling distribution
#' @param len_min Integer indicating the minimum of the document lengths
#'                 in the parent normal sampling distribution
#' @param len_max Integer indicating the maximum of the document lengths
#'                 in the parent normal sampling distribution
#' @param seed Optional seed for reproducibility
#'
#' @examples
#' # create corpus and DTM
#' my_corpus <- data.frame(
#'   text = c(
#'     "I hear babies crying I watch them grow",
#'     "They'll learn much more than I'll ever know",
#'     "And I think to myself",
#'     "What a wonderful world",
#'     "Yes I think to myself",
#'     "What a wonderful world"
#'   ),
#'   line_id = paste0("line", seq_len(6))
#' )
#' ## some text preprocessing
#' my_corpus$clean_text <- tolower(gsub("'", "", my_corpus$text))
#'
#' dtm <- dtm_builder(
#'   data = my_corpus,
#'   text = clean_text,
#'   doc_id = line_id
#' )
#'
#' # use colSums to get term frequencies
#' df <- data.frame(
#'   terms = colnames(dtm),
#'   freqs = colSums(dtm)
#' )
#' # convert to probabilities
#' df$probs <- df$freqs / sum(df$freqs)
#'
#' # create random DTM
#' rDTM <- df |>
#'   rancor_builder(terms, probs)
#'
#' @export
rancor_builder <- function(data,
                           vocab,
                           probs,
                           n_docs = 100L,
                           len_mean = 500,
                           len_var = 10L,
                           len_min = 20L,
                           len_max = 1000L,
                           seed = NULL) {
  if (is.null(seed)) {
    seed <- as.integer(runif(1, 1, 1000000))
  }

  vocab <- dplyr::pull(data, {{ vocab }})
  probs <- dplyr::pull(data, {{ probs }})

  # set the seed for the doc length distribution
  set.seed(seed)
  # get a distribution of document lengths
  doc_lens <- .rnorm_trunc(
    n = n_docs,
    mean = len_mean,
    sd = len_var,
    min = len_min,
    max = len_max
  )

  # create token lists from a random sample from each row
  samp_tokns <- lapply(
    seq_len(n_docs),
    FUN = function(i, seed) {
      # set new seed for each row
      set.seed(seed + i)
      samp <- base::sample(
        vocab,
        size = doc_lens[i],
        replace = TRUE,
        prob = as.numeric(probs / sum(probs))
      )
      # use different seed to randomize draws from each row
      return(samp)
    },
    seed
  )

  samp_vects <- unlist(samp_tokns,
    recursive = FALSE,
    use.names = FALSE
  )

  # create the "sampled" DTM
  sdtm <- Matrix::sparseMatrix(
    i = rep(
      seq_along(samp_tokns),
      lengths(samp_tokns, use.names = TRUE)
    ),
    j = fastmatch::fmatch(samp_vects, vocab, nomatch = 0),
    dims = c(n_docs, length(vocab)),
    x = 1L,
    dimnames = list(paste0("doc_", seq_len(n_docs)), vocab)
  )

  # explicitly declare class for all DTMs
  sdtm <- methods::as(sdtm, "CsparseMatrix")

  attr(sdtm, "seed") <- seed

  return(sdtm)
}



#' Build Multiple Random Corpora
#'
#' `rancors_builder()` generates multiple random corpus (rancor) based on a user
#' defined term probabilities and vocabulary. sers can set the number of
#' documents, as well as the mean, standard deviation, minimum, and maximum
#' document lengths (i.e., number of tokens) of the parent normal distribution
#' from which the document lengths are randomly sampled. The output is a list of
#' document-term matrices. To produce a *single* random corpus, use
#' `rancor_builder()` (note the singular).
#'
#'
#' @name rancors_builder
#' @author Dustin Stoltz and Marshall Taylor
#'
#'
#' @param data Data.frame containing vocabulary and probabilities
#' @param vocab Name of the column containing vocabulary
#' @param probs Name of the column containing probabilities
#' @param n_cors Integer indicating the number of corpora to build
#' @param n_docs Integer(s) indicating the number of documents to be returned
#'               If two numbers are provide, number will be randomly sampled
#'               within the range for each corpora.
#' @param len_mean Integer(s) indicating the mean of the document lengths
#'                in the parent normal sampling distribution. If two 
#'                numbers are provided, number will be randomly sampled
#'                within the range for each corpora.
#' @param len_var Integer(s) indicating the standard deviation of the 
#'                document lengths in the parent normal sampling distribution.
#'                If two numbers are provided, number will be randomly sampled 
#'                within the range for each corpora.
#' @param len_min Integer(s) indicating the minimum of the document lengths
#'                in the parent normal sampling distribution. If two numbers
#'                are provided, number will be randomly sampled within the 
#'                range for each corpora.
#' @param len_max Integer(s) indicating the maximum of the document lengths
#'                in the parent normal sampling distribution. If two numbers
#'                are provided, number will be randomly sampled within the
#'                range for each corpora.
#' @param seed Optional seed for reproducibility
#'
#' @examples
#' 
#' \donttest{
#' # create corpus and DTM
#' my_corpus <- data.frame(
#'   text = c(
#'     "I hear babies crying I watch them grow",
#'     "They'll learn much more than I'll ever know",
#'     "And I think to myself",
#'     "What a wonderful world",
#'     "Yes I think to myself",
#'     "What a wonderful world"
#'   ),
#'   line_id = paste0("line", seq_len(6))
#' )
#' ## some text preprocessing
#' my_corpus$clean_text <- tolower(gsub("'", "", my_corpus$text))
#'
#' dtm <- dtm_builder(
#'   data = my_corpus,
#'   text = clean_text,
#'   doc_id = line_id
#' )
#'
#' # use colSums to get term frequencies
#' df <- data.frame(
#'   vocab = colnames(dtm),
#'   freqs = colSums(dtm)
#' )
#' # convert to probabilities
#' df$probs <- df$freqs / sum(df$freqs)
#'
#' # create random DTM
#' ls_dtms <- df |>
#'   rancors_builder(vocab,
#'     probs,
#'     n_cors = 20,
#'     n_docs = 100,
#'     len_mean = c(50, 200),
#'     len_var = 5,
#'     len_min = 20,
#'     len_max = 1000,
#'     seed = 59801
#'   )
#' length(ls_dtms)
#' }
#' @export
rancors_builder <- function(data,
                            vocab,
                            probs,
                            n_cors,
                            n_docs,
                            len_mean,
                            len_var,
                            len_min,
                            len_max,
                            seed = NULL) {
  if (is.null(seed)) {
    seed <- as.integer(runif(1, 1, 1000000))
  }

  set.seed(seed)
  vec_ndocs <- .rc_seq(n_docs, n_cors)
  vec_len_mean <- .rc_seq(len_mean, n_cors)
  vec_len_var <- .rc_seq(len_var, n_cors)
  vec_len_min <- .rc_seq(len_min, n_cors)
  vec_len_max <- .rc_seq(len_max, n_cors)

  ls_rdtms <- list()
  length(ls_rdtms) <- n_cors
  attr(ls_rdtms, "seed") <- seed

  for (i in seq_len(n_cors)) {
    ls_rdtms[[i]] <- data |>
      rancor_builder(
        vocab,
        probs,
        n_docs = vec_ndocs[i],
        len_mean = vec_len_mean[i],
        len_var = vec_len_var[i],
        len_min = vec_len_min[i],
        len_max = vec_len_max[i],
        seed
      )

    # assure new seed must be greater
    # than the number of docs
    seed <- as.integer(seed + (max(n_docs) * i))
  }

  return(ls_rdtms)
}

#' @keywords internal
#' @noRd
.rc_seq <- function(r, n) {
  if (length(r) > 1) {
    sample(seq(from = r[[1]], to = r[[2]]), size = n, replace = TRUE)
  } else {
    rep(r, n)
  }
}
