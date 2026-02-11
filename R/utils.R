#' Gets stoplist from precompiled lists
#'
#' Provides access to 8 precompiled stoplists, including the most commonly used
#' stoplist from the Snowball stemming package ("snowball2014"), `text2map`'s
#' tiny stoplist ("tiny2020"), a few historically important stop lists. This
#' aims to be a transparent and well-document collection of stoplists. Only
#' includes English language stoplists at the moment.
#'
#' @details
#'
#' There is no such thing as a *stopword*! But, there are **tons** of
#' precompiled lists of words that someone thinks we should remove from
#' our texts. (See for example: https://github.com/igorbrigadir/stopwords)
#' One of the first stoplists is from C.J. van Rijsbergen's "Information
#' retrieval: theory and practice" (1979) and includes 250 words.
#' `text2map`'s very own stoplist `tiny2020` is a lean 34 words.
#'
#' Below are stoplists available with [get_stoplist]:
#' - "tiny2020": Tiny (2020) list of 33 words (Default)
#' - "snowball2001": Snowball stemming package's (2001) list of 127 words
#' - "snowball2014": Updated Snowball (2014) list of 175 words
#' - "van1979": C. J. van Rijsbergen's (1979) list of 250 words
#' - "fox1990": Christopher Fox's (1990) list of 421 words
#' - "smart1993": Original SMART (1993) list of 570 words
#' - "onix2000": ONIX (2000) list of 196 words
#' - "nltk2001": Python's NLTK (2009) list of 179 words
#'
#' The Snowball (2014) stoplist is likely the most commonly, it is the default
#' in the `stopwords` package, which is used by `quanteda`, `tidytext` and
#' `tokenizers` packages, followed closely by the Smart (1993) stoplist,
#' the default in the `tm` package. The word counts for SMART (1993) and
#' ONIX (2000) are slightly different than in other places because of
#' duplicate words.
#'
#'
#' @name get_stoplist
#' @author Dustin Stoltz
#'
#' @importFrom tibble tibble
#'
#' @param source Character indicating source, default = `"tiny2020"`
#' @param language Character (default = "en") indicating language of stopwords
#'                 by ISO 639-1 code, currently only English is supported.
#' @param tidy logical (default = `FALSE`), returns a tibble
#' @return Character vector of words to be stopped,
#'         if tidy = TRUE, a tibble is returned
#'
#' @export
#'
#'
#'
get_stoplist <- function(source = "tiny2020", language = "en", tidy = FALSE) {
  if (source %in% c(
    "van1979", "fox1990",
    "smart1993", "onix2000",
    "snowball2001", "snowball2014",
    "tiny2020", "nltk2009"
  )) {
    if (language != "en") {
      stop(paste0(source, " stoplist is currently only available for 'en'"))
    }

    # this allows the data to be accessed without attaching the package
    slists <- eval(parse(text = "text2map::stoplists"))
    stop_list <- slists[slists[, source] == TRUE, "word", drop = TRUE]
  } else {
    stop(paste0(source, " stoplist is not currently available"))
  }

  if (tidy == TRUE) {
    stop_list <- tibble(
      word = stop_list,
      lexicon = source
    )
  }


  return(stop_list)
}


#' A very tiny "gender" tagger
#'
#' Provides a small dictionary which matches common English pronouns
#' and nouns to conventional gender categories ("masculine" or
#' "feminine"). There are 20 words in each category.
#'
#' @importFrom tibble tibble
#'
#' @name tiny_gender_tagger
#' @author Dustin Stoltz
#'
#' @return returns a tibble with two columns
#' @export
tiny_gender_tagger <- function() {
  tb1 <- tibble::tibble(
    word = c(
      "she", "her", "hers", "herself",
      "woman", "girl", "lady", "gal",
      "women", "girls", "ladies", "gals",
      "mom", "mother", "wife", "girlfriend",
      "daughter", "sister", "mrs", "female"
    )
  )
  tb1$gender <- "feminine"

  tb2 <- tibble::tibble(
    word = c(
      "he", "him", "his", "himself",
      "man", "boy", "gentleman", "guy",
      "men", "boys", "gentlemen", "guys",
      "dad", "father", "husband", "boyfriend",
      "son", "brother", "mr", "male"
    )
  )
  tb2$gender <- "masculine"

  gender_tagger <- rbind(tb1, tb2)

  return(gender_tagger)
}


## ------ INTERNAL GENERIC FUNCTIONS ----------------------------------------- #


## ----------------------------------------------------------------------------
# given list, gives confidence intervals
## ----------------------------------------------------------------------------

.conf_int <- function(x, ci = .95) {
  a <- mean(x)
  s <- sd(x)
  n <- length(x)
  error <- qt(ci+(1-ci)/2, df = n-1)*s/sqrt(n)
  return(c(mean = a, lower_ci = a-error, upper_ci = a+error))
}


#' Fast-match `%fin%` operator masking base R `%in%`
#'
#' Returns the elements of `x` that are "in" `y`, uses `fastmatch`'s
#' matching backend.
#'
#' @import fastmatch
#' @importFrom fastmatch %fin%
#' @importFrom fastmatch fmatch
#'
#' @usage x \%fin\% y
#'
#' @param x vector of all items
#' @param y vector of set of items to be not-matched
#'
#' @return logical vector of items in x not in y
#'
#' @author Dustin Stoltz
#'
#' @keywords internal
#' @noRd
`%in%` <- fastmatch::`%fin%`


#' Fast-not-match `%fnin%` operator
#'
#' Complement of the operator \code{\%in\%}. Returns the elements of `x` that
#' are "out of" or "not in" `y`, but uses `fastmatch`'s matching backend.
#'
#'
#' @importFrom fastmatch %fin%
#' @importFrom fastmatch fmatch
#'
#' @usage x \%fin\% y
#'
#' @param x vector of all items
#' @param y vector of set of items to be not-matched
#'
#' @return logical vector of items in x not in y
#'
#' @author Dustin Stoltz
#'
#' @noRd
`%fnin%` <- Negate(fastmatch::`%fin%`)

# #' Fast-not-match `%fnin%` operator
# #'
# #' Complement of the operator \code{\%in\%}. Returns the elements of `x` that
# #'
# #'
# #'
# #' @usage x \%fin\% y
# #'
# #' @param x vector of all items
# #' @param y vector of set of items to be not-matched
# #'
# #' @return logical vector of items in x not in y
# #'
# #' @author Dustin Stoltz
# #'
# #' @noRd
# `%fnin%` <- Negate(base::`%in%`)


#' Measure .kurtosis
#'
#' adapted from e1071 package type=3 in the kurtosis function:
#' b_2 = m_4/s^4 - 3 = (g_2 + 3)(1 - 1/n)^2 - 3
#'
#' @param x a vector of numbers
#'
#' @keywords internal
#' @noRd
.kurtosis <- function(x) {
  n <- length(x)
  x <- x - base::mean(x)
  r <- n * base::sum(x^4) / (base::sum(x^2)^2)
  out <- r * (1 - 1 / n)^2 - 3
  return(out)
}

#' Measure .skewness
#'
#' adapted from e1071 package type=3 in the skewness function:
#' b_1 = m_3/s^3 = g_((n - 1)/n)^3/2
#'
#' @param x a vector of numbers
#'
#' @keywords internal
#' @noRd
.skewness <- function(x) {
  n <- length(x)
  x <- x - base::mean(x)
  y <- sqrt(n) * base::sum(x^3) / (base::sum(x^2)^(3 / 2))
  out <- y * ((1 - 1 / n))^(3 / 2)
  return(out)
}

#' Measure .n_decimal_places
#'
#' Counts the number of decimal places. Code from
#' https://stackoverflow.com/a/5173906/15855390
#'
#' @param x a vector of numbers
#'
#' @keywords internal
#' @noRd
.n_decimal_places <- function(x) {
  if (abs(x - round(x)) > .Machine$double.eps^0.5) {
    nchar(strsplit(
      sub(
        "0+$", "",
        as.character(format(x, scientific = FALSE))
      ), ".",
      fixed = TRUE
    )[[1]][[2]])
  } else {
    return(as.integer(0))
  }
}

#' .check_whole_num
#'
#' Checks if numbers are integers/whole numbers
#'
#' @param x a vector of numbers
#' @param any logical (default FALSE), return any TRUE
#'
#' @keywords internal
#' @noRd
.check_whole_num <- function(x) {
  out <- is.integer(x)

  return(isTRUE(out))
}


#' .rnorm_trunc
#'
#' Generating random numbers from a truncated distribution
#' https://www.r-bloggers.com/2020/08/generating-data-from-a-truncated-distribution/
#' 
#' @importFrom stats runif
#' @importFrom stats pnorm
#' @importFrom stats qnorm
#'
#' @param n number of random values to return
#' @param mean average of the returned values
#' @param sd standard deviation of the returned values
#' @param min minimum value of the returned values
#' @param max maximum value of the returned values
#'
#' @keywords internal
#' @noRd
.rnorm_trunc <- function(n, mean, sd, min, max) {
  u <- stats::runif(n,
    min = stats::pnorm(min, mean = mean, sd = sd),
    max = stats::pnorm(max, mean = mean, sd = sd)
  )

  return(stats::qnorm(u, mean = mean, sd = sd))
}

#' quiets concerns of R CMD check re: i in loops and lapply
#'
#' @keywords internal
#' @noRd
utils::globalVariables(c("i"))


