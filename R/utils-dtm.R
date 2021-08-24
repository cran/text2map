#' A fast unigram DTM builder
#'
#' A streamlined function to take raw texts from a column of a data.frame and
#' produce a sparse Document-Term Matrix (of generic class "dgCMatrix").
#'
#' @details
#'
#' The function is fast because it has few bells and whistles:
#' * No weighting schemes other than raw counts
#' * Tokenizes by the fixed, single whitespace
#' * Only tokenizes unigrams, no bigrams, trigrams, etc..
#' * Columns are in the order unique words are discovered
#' * No preprocessing during building
#' * Outputs a basic sparse matrix
#'
#' Weighting or stopping words can be done efficiently after the fact with
#' simple matrix operations, rather than achieved implicitly within the
#' function itself. Prior to creating the DTM, texts should have whitespace
#' trimmed, punctuation removed, and, if desired, words should be lowercased.
#'
#' Like `tidytext`'s DTM functions, `dtm_builder()` is optimized for use in a
#' pipeline, but unlike `tidytext`, it does not build an intermediary
#' tripletlist, so `dtm_builder()` is faster and far more memory
#' efficient.
#'
#' The function can also `chunk` the corpus into documents of a given length
#' (default is `NULL`). If the integer provided is `200L`, this will divide the
#' corpus into new documents with 200 words (with the final document likely
#' including slightly less than 200). If the total words in the corpus
#' were less than or equal to `chunk` integer, this would produce a
#' DTM with one document (most will probably not want this).
#'
#' @importFrom stringi stri_split
#' @importFrom dplyr pull
#' @importFrom kit funique
#' @importFrom fastmatch fmatch
#' @importFrom dplyr pull
#' @importFrom Matrix sparseMatrix
#' @importFrom methods as
#' @importFrom rlang quo_name enquo
#'
#' @name dtm_builder
#' @author Dustin Stoltz
#'
#' @param df Data.frame with one column of texts and one column of document ids
#' @param text Name of the column with documents' text
#' @param doc_id Name of the column with documents' unique ids
#' @param vocab Default is `NULL`, if a list of terms is provided, the function
#'              will return a DTM with terms restricted to this vocabulary.
#'              Columns will also be in the same order as the list of terms.
#' @param chunk Default is `NULL`, if an integer is provided, the function will
#'              "re-chunk" the corpus into new documents of a particular length.
#'              For example, `100L` will divide the corpus into new documents
#'              with 100 words (with the final document likely including
#'              slightly less than 100).
#'
#' @return returns a document-term matrix of class "dgCMatrix"
#'
#' @examples
#'
#' library(dplyr)
#'
#' my.corpus <- data.frame(
#'   text = c(
#'     "I hear babies crying I watch them grow",
#'     "They’ll learn much more than I'll ever know",
#'     "And I think to myself",
#'     "What a wonderful world",
#'     "Yes I think to myself",
#'     "What a wonderful world"
#'   ),
#'   line_id = paste0("line", 1:6)
#' )
#'
#' ## some text preprocessing
#' my.corpus$clean_text <- tolower(gsub("'|’", "", my.corpus$text))
#'
#' # example 1
#' dtm <- my.corpus |>
#'   dtm_builder(clean_text, line_id)
#'
#' # example 2
#' dtm <- dtm_builder(my.corpus, text = clean_text, doc_id = line_id)
#'
#' # example 3
#' dtm <- my.corpus |>
#'   mutate(
#'     clean_text = gsub("'|’", "", text),
#'     clean_text = tolower(clean_text)
#'   ) |>
#'   dtm_builder(clean_text, line_id)
#'
#' # example 4
#' dtm <- my.corpus |>
#'   dtm_builder(clean_text, line_id, chunk = 3L)
#'
#' @export
dtm_builder <- function(df, text, doc_id, vocab = NULL, chunk = NULL) {

  # this will tokenize by the fixed space pattern
  # outputs a nested list of tokens
  tokns <- stringi::stri_split(
    dplyr::pull(df, {{ text }}),
    fixed = " ", omit_empty = TRUE
  )

  # vectorize our token-list into one
  # vector of tokens for the entire corpus
  vects <- unlist(tokns,
    recursive = FALSE,
    use.names = FALSE
  )

  if (is.null(vocab)) {

    # get all the unique words in the corpus
    # they will be in the order they first appear
    vocab <- kit::funique(vects)

    if (!is.null(chunk)) {
      dtm <- Matrix::sparseMatrix(
        i = ceiling(seq_along(vects) / as.integer(chunk)),
        j = fastmatch::fmatch(vects, vocab, nomatch = 0L),
        x = 1L,
        dimnames = list(NULL, vocab)
      )
    } else {
      dtm <- tryCatch(
        {
          Matrix::sparseMatrix(
            i = rep(
              seq_along(tokns),
              lengths(tokns, use.names = TRUE)
            ),
            j = fastmatch::fmatch(vects, vocab, nomatch = 0L),
            x = 1L,
            dimnames = list(dplyr::pull(df, {{ doc_id }}), vocab)
          )
        },
        error = function(e) {
          err <- conditionMessage(e)

          if (stringi::stri_detect_fixed(
            err,
            "length(Dimnames[1]) differs from Dim[1]"
          )) {
            message(
              "`", rlang::quo_name(rlang::enquo(doc_id)), "`",
              " is longer than the number of rows in the DTM.\n",
              "This is likely because the last row of `",
              rlang::quo_name(rlang::enquo(text)), "`",
              " is empty.\n",
              "The original error message: "
            )
            stop(e)
          } else {
            stop(e)
          }
        }
      )
    }

    return(dtm)
  }
  # if a vocabulary is provided
  if (!is.null(vocab)) {
    vocab <- c(vocab, "INT_VC_MISS")
    vects <- c(vects, "INT_VC_MISS")
    tokns <- append(tokns, "INT_VC_MISS")
    docs <- c(dplyr::pull(df, {{ doc_id }}), "INT_DOC_EMPTY")

    if (!is.null(chunk)) {
      dtm <- Matrix::sparseMatrix(
        i = ceiling(seq_along(vects) / as.integer(chunk)),
        j = fastmatch::fmatch(vects, vocab, nomatch = 0L),
        x = 1L,
        dimnames = list(NULL, vocab)
      )
    } else {
      dtm <- tryCatch(
        {
          Matrix::sparseMatrix(
            i = rep(
              seq_along(tokns),
              lengths(tokns, use.names = TRUE)
            ),
            j = fastmatch::fmatch(vects, vocab, nomatch = length(vocab)),
            x = 1L,
            dimnames = list(docs, vocab)
          )
        },
        error = function(e) {
          err <- conditionMessage(e)

          if (stringi::stri_detect_fixed(
            err,
            "length(Dimnames[1]) differs from Dim[1]"
          )) {
            message(
              "`", rlang::quo_name(rlang::enquo(doc_id)), "`",
              " is longer than the number of rows in the DTM.\n",
              "This is likely because the last row of `",
              rlang::quo_name(rlang::enquo(text)), "`",
              " is empty.\n",
              "The original error message: "
            )
            stop(e)
          } else {
            stop(e)
          }
        }
      )
    }

    return(dtm[
      rownames(dtm) != "INT_DOC_EMPTY",
      colnames(dtm) != "INT_VC_MISS"
    ])
  }
}


#' Gets DTM summary statistics
#'
#' `dtm_stats()` provides a summary, corpus-level statistics using
#' any document-term matrix
#'
#' @name dtm_stats
#' @author Dustin Stoltz
#'
#' @importFrom Matrix rowSums
#' @importFrom utils object.size
#'
#' @param dtm Document-term matrix with words as columns. Works with DTMs
#'            produced by any popular text analysis package, or you can use the
#'            `dtm_builder()` function.
#' @param richness Logical (default = TRUE), whether to include statistics
#'                 about lexical richness, i.e. words that occur once,
#'                 twice, and three times (hapax, dis, tris), and the total
#'                 type-token ratio.
#' @param distribution Logical (default = TRUE), whether to include statistics
#'                     about the distribution, i.e. min, max st. dev, skewness,
#'                     kurtosis.
#' @param central Logical (default = TRUE), whether to include statistics
#'                about the central tendencies i.e. mean and median for
#'                types and tokens.
#' @param character Logical (default = TRUE), whether to include statistics
#'                  about the character lengths of terms, i.e. min, max, mean
#' @param simplify Logical (default = FALSE), whether to return statistics as a
#'                 data frame where each statistic is a column. Default returns
#'                 a list of small data frames.
#'
#' @return An list of 1 to five data frames with summary statistics (if
#'         `simplify==FALSE`), otherwise a single data frame where each
#'         statistics is a column.
#'
#' @export
dtm_stats <- function(dtm,
                      richness = TRUE,
                      distribution = TRUE,
                      central = TRUE,
                      character = TRUE,
                      simplify = FALSE) {

  # convert all DTMs to dgCMatrix class
  if (class(dtm)[[1]] != "dgCMatrix") {
    dtm <- .convert_dtm_to_dgCMatrix(dtm)
  }

  # basic stats
  sparsity <- sum(dtm[] == 0) / (ncol(dtm) * nrow(dtm))
  doc_count <- Matrix::rowSums(dtm)
  doc_bins <- Matrix::rowSums(dtm[] > 0)

  tab1 <- data.frame(
    n_docs = nrow(dtm),
    sparsity = round(sparsity, 3),
    n_types = ncol(dtm),
    n_tokens = sum(dtm),
    size = format(utils::object.size(dtm), units = "auto"),
    row.names = NULL
  )

  tab <- tab1

  # lexical richness
  if (richness == TRUE) {
    hapax <- sum(dtm[] == 1) / sum(dtm)
    dis <- sum(dtm[] == 2) / sum(dtm)
    tris <- sum(dtm[] == 3) / sum(dtm)

    tab2 <- data.frame(
      hapax = round(hapax, 3),
      dis = round(dis, 3),
      tris = round(tris, 3),
      ttr = round(ncol(dtm) / sum(dtm), 3),
      row.names = NULL
    )

    tab <- cbind(tab, tab2)
  }

  # term distributions
  if (distribution == TRUE) {
    tab3 <- data.frame(
      min_types = as.integer(base::min(doc_bins)),
      min_tokens = as.integer(base::min(doc_count)),
      max_types = as.integer(base::max(doc_bins)),
      max_tokens = as.integer(base::max(doc_count)),
      sd_types = round(stats::sd(doc_bins), 3),
      sd_tokens = round(stats::sd(doc_count), 3),
      kr_types = round(.kurtosis(doc_bins), 3),
      kr_tokens = round(.kurtosis(doc_count), 3),
      sk_types = round(.skewness(doc_bins), 3),
      sk_tokens = round(.skewness(doc_count), 3),
      row.names = NULL
    )

    tab <- cbind(tab, tab3)
  }

  # term central tendencies
  if (central == TRUE) {
    tab4 <- data.frame(
      mu_types = base::mean(doc_bins),
      mu_tokens = base::mean(doc_count),
      md_types = stats::median(doc_bins),
      md_tokens = stats::median(doc_count),
      row.names = NULL
    )

    tab <- cbind(tab, tab4)
  }

  # character-level
  if (character == TRUE) {
    char.len <- nchar(colnames(dtm))

    tab5 <- data.frame(
      min_length = as.integer(base::min(char.len)),
      max_length = as.integer(base::max(char.len)),
      mu_length = round(base::mean(char.len), 3),
      row.names = NULL
    )

    tab <- cbind(tab, tab5)
  }

  if (simplify == TRUE) {
    return(tab)
  } else {
    tab1$sparsity <- paste0(formatC(100 * tab1$sparsity,
      format = "f",
      digits = 2
    ), "%")

    tab1 <- data.frame(
      Measure = c(
        "Total Docs",
        "Percent Sparse",
        "Total Types",
        "Total Tokens",
        "Object Size"
      ),
      Value = t(tab1)[, 1],
      row.names = NULL
    )

    if (richness == TRUE) {
      tab2$hapax <- paste0(formatC(100 * tab2$hapax,
        format = "f",
        digits = 3
      ), "%")
      tab2$dis <- paste0(formatC(100 * tab2$dis,
        format = "f",
        digits = 3
      ), "%")
      tab2$tris <- paste0(formatC(100 * tab2$tris,
        format = "f",
        digits = 3
      ), "%")

      tab2 <- data.frame(
        Measure = c(
          "Percent Hapax",
          "Percent Dis",
          "Percent Tris",
          "Type-Token Ratio"
        ),
        Value = as.character(t(tab2)[, 1]),
        row.names = NULL
      )
    } else {
      tab2 <- NULL
    }

    # term distributions
    if (distribution == TRUE) {
      tab3 <- data.frame(
        Measure = c(
          "Min Types",
          "Min Tokens",
          "Max Types",
          "Max Tokens",
          "St. Dev. Types",
          "St. Dev. Tokens",
          "Kurtosis Types",
          "Kurtosis Tokens",
          "Skew Types",
          "Skew Tokens"
        ),
        Value = as.character(t(tab3)[, 1]),
        row.names = NULL
      )
    } else {
      tab3 <- NULL
    }

    # term central tendencies
    if (central == TRUE) {
      tab4 <- data.frame(
        Measure = c(
          "Mean Types",
          "Mean Tokens",
          "Median Types",
          "Median Tokens"
        ),
        Value = as.character(t(tab4)[, 1]),
        row.names = NULL
      )
    } else {
      tab4 <- NULL
    }

    # character-level
    if (character == TRUE) {
      tab5 <- data.frame(
        Measure = c(
          "Min Characters",
          "Max Characters",
          "Mean Characters"
        ),
        Value = as.character(t(tab5)[, 1]),
        row.names = NULL
      )
    } else {
      tab5 <- NULL
    }

    RESULT <- list(
      `Basic Information` = tab1,
      `Lexical Richness` = tab2,
      `Term Distribution` = tab3,
      `Central Tendency` = tab4,
      `Term Lengths` = tab5
    )

    RESULT[vapply(RESULT, is.null, FUN.VALUE = logical(1))] <- NULL

    return(RESULT)
  }
}

#' Removes terms from a DTM based on rules
#'
#' `dtm_stopper()` will "stop" terms from the analysis by removing columns
#' that match terms to be stopped. Rules include using precompiled or custom
#' list of terms, or terms meeting an upper or lower document frequency
#' threshold.
#'
#' Stopping terms by removing their respective columns in the DTM is
#' significantly more efficient than searching raw text with string matching
#' and deletion rules. The `stop_freq` and `stop_list` arguments are akin to
#' the `tm` package's `removeSparseTerms` and `removeWords` functions,
#' respectively. Behind the scenes, the function relies on the `fastmatch`
#' package to quickly match/not-match terms.
#'
#' @name dtm_stopper
#' @author Dustin Stoltz
#'
#' @importFrom Matrix colSums
#'
#' @param dtm Document-term matrix with words as columns. Works with DTMs
#'            produced by any popular text analysis package, or you can use the
#'            `dtm_builder()` function.
#' @param stop_list Vector of terms, from either a precompiled stoplist or
#'                  custom list such as `c("never", "gonna", "give")`, or a
#'                  combination of the two.
#' @param stop_freq Vector of two numbers indicating the lower and upper
#'                  threshold for inclusion. If real numbers less than 1,
#'                  for example `c(0.01, 0.99)`, then words will be removed by
#'                  proportion, here words in more than 99% of all documents or
#'                  words that are in less than 1% of all documents. If
#'                  integers are provided, for example `c(100, 9000)`, then
#'                  words will be removed by count, here words occurring less
#'                  than 100 or more than 9000 times  in the corpus will
#'                  be removed.
#' @param ignore_case Logical (default = TRUE) indicating whether to ignore
#'                    capitalization when matching.
#'
#' @return returns a document-term matrix of class "dgCMatrix"
#'
#' @export
dtm_stopper <- function(dtm,
                        stop_list = NULL,
                        stop_freq = NULL,
                        ignore_case = TRUE) {

  # convert all dtms to dgCMatrix class
  if (class(dtm)[[1]] != "dgCMatrix") {
    dtm <- .convert_dtm_to_dgCMatrix(dtm)
  }

  # stop if both stop rules are NULL
  if (is.null(stop_list) && is.null(stop_freq)) {
    stop("No stop rules were provided.")
  }


  # if only stop_list is NULL
  # initialize the object
  if (is.null(stop_list)) {
    stop_list <- character(0)
  }

  if (is.numeric(stop_freq)) {

    ## check if whole numbers
    if (all(stop_freq %% 1 == 0) == TRUE) {
      term_freqs <- Matrix::colSums(dtm != 0)
    } else {

      ## check if real numbers between 1 and 0
      if (all(stop_freq >= 1 &&
        stop_freq <= 0) == TRUE) {
        n_decs <- max(
          c(
            .n_decimal_places(max(stop_freq)),
            .n_decimal_places(min(stop_freq))
          )
        )

        term_freqs <- round(Matrix::colSums(dtm != 0) / nrow(dtm), n_decs)
      } else {
        stop("`stop_freq` argument must be either two real numbers bound\n
               between 1 and 0, or two integers greater than 0.\n
               For example: c(0.01, 0.99) or c(100L, 9000L)")
      }
      ## find terms greater than or equal to the upper threshold
      ## or less than or equal to the lower threshold
      term_freqs <- names(
        term_freqs[term_freqs >= max(stop_freq) |
          term_freqs <= min(stop_freq)]
      )

      # merge with words in stop_list if provided
      stop_list <- kit::funique(c(term_freqs, stop_list))
    }
  }

  ## use fast-not-match to "stop" terms
  if (ignore_case == TRUE) {
    indx <- tolower(colnames(dtm)) %fnin% tolower(stop_list)
  } else {
    indx <- colnames(dtm) %fnin% stop_list
  }

  return(dtm[, indx])
}


#' Resamples an input DTM to generate new DTMs
#'
#' Takes any DTM and randomly resamples from each row, creating a new DTM
#'
#' Using the row counts as probabilities, each document's tokens are resampled
#' with replacement up to a certain proportion of the row count (set by alpha).
#' This function can be used with iteration to "bootstrap" a DTM without
#' returning to the raw text. It does not iterate, however, so operations can
#' be performed on one DTM at a time without storing multiple DTMs in memory.
#'
#' @importFrom Matrix sparseMatrix
#' @importFrom fastmatch fmatch
#'
#' @param dtm Document-term matrix with words as columns. Works with DTMs
#'            produced by any popular text analysis package, or you can use the
#'            `dtm_builder()` function.
#' @param alpha a proportion of document tokens. Default = 1, or the full
#'              document lengths.
#'
#' @return returns a document-term matrix of class "dgCMatrix"
#'
#' @export
dtm_resampler <- function(dtm, alpha = 1) {

  # explicitly declare class for input
  dtm <- .convert_dtm_to_dgCMatrix(dtm)

  # create token lists from a random sample from each row
  samp_tokns <- lapply(
    seq_len(nrow(dtm)),
    FUN = function(i) {
      i.row <- dtm[i, ]

      if (sum(i.row) == 0) {
        return(samp = NULL)
      } else {
        samp <- base::sample(
          colnames(dtm),
          size = round(sum(i.row) * alpha),
          replace = TRUE,
          prob = as.numeric(i.row / sum(i.row))
        )
        return(samp)
      }
    }
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
    j = fastmatch::fmatch(samp_vects, colnames(dtm), nomatch = 0),
    dims = dim(dtm),
    x = 1L,
    dimnames = dimnames(dtm)
  )


  # explicitly declare class for all DTMs
  sdtm <- methods::as(sdtm, "dgCMatrix")

  return(sdtm)
}

#' A fast unigram vocabulary builder
#'
#' A streamlined function to take raw texts from a column of a data.frame and
#' produce a list of all the unique tokens. Tokenizes by the fixed,
#' single whitespace, and then extract the unique tokens. This can be used as
#' input to `dtm_builder()` to standardize the vocabulary (i.e. the columns)
#' across multiple DTMs. Prior to building the vocabulary, texts should have
#' whitespace trimmed, punctuation removed, and, if desired,
#' words should be lowercased.
#'
#'
#' @importFrom stringi stri_split
#' @importFrom dplyr pull
#' @importFrom kit funique
#' @importFrom dplyr pull
#'
#' @name vocab_builder
#' @author Dustin Stoltz
#'
#' @param df Data.frame with one column of texts
#' @param text Name of the column with documents' text
#'
#' @return returns a list of unique terms in a corpus
#'
#' @export
vocab_builder <- function(df, text) {

  # this will tokenize by the fixed space pattern
  # outputs a nested list of tokens
  tokns <- stringi::stri_split(
    dplyr::pull(df, {{ text }}),
    fixed = " ", omit_empty = TRUE
  )

  # vectorize our token-list into one
  # vector of tokens for the entire corpus
  vects <- unlist(tokns,
    recursive = FALSE,
    use.names = FALSE
  )

  # get all the unique words in the corpus
  # they will be in the order they first appear
  vocab <- kit::funique(vects)

  return(vocab)
}


## ---- INTERNAL DTM SPECIFIC FUNCTIONS -------------------------------------- #

#' .convert_dtm_to_dgCMatrix
#'
#' Takes any DTM class and converts it to dgCMatrix.
#'
#' Works with tm package classes (TermDocumentMatrix, DocumentTermMatrix,
#' simple_triplet_matrix), dfm package class (dfm), and base R dense matrix.
#' It will also convert any other Matrix package class into a dgCMatrix
#'
#' @param dtm Document-term matrix with words as columns. Works with DTMs
#'            produced by any popular text analysis package, or you can use the
#'            `dtm_builder()` function.
#'
#' @noRd
.convert_dtm_to_dgCMatrix <- function(dtm) {

  ## Make DTM dgCMatrix sparse ##
  # if simple_triplet_matrix like tm package
  if (any(class(dtm) == "simple_triplet_matrix")) {

    # transpose TDM
    if (any(class(dtm) == "TermDocumentMatrix")) {
      dtm <- t(dtm)
    }

    # convert to sparse matrix
    dtm <- Matrix::sparseMatrix(
      i = dtm$i,
      j = dtm$j,
      x = dtm$v,
      dims = c(dtm$nrow, dtm$ncol),
      dimnames = dimnames(dtm)
    )
  }

  # convert base dense to spare matrix
  if (any(class(dtm) == "matrix")) {
    dtm <- Matrix::Matrix(dtm, sparse = TRUE)
    # explicitly declare class
  }

  # explicitly declare class for all DTMs
  dtm <- methods::as(dtm, "dgCMatrix")

  return(dtm)
}

#' .remove_empty_rows_in_dtm
#'
#' This function will find empty rows in a DTM, remove them, and also
#' tell the user which rows were removed (for matching purposes later)
#' DTMs can wind up with empty rows in a variety of ways, in particular
#' through preprocessing steps that involve deleting columns. When this
#' happens the row is essentially an "empty document" and that's not
#' very useful. It also can lead to minor technical problems, such as
#' the Matrix package being fussy.
#'
#'
#' @param dtm Document-term matrix with words as columns. Works with DTMs
#'            produced by any popular text analysis package, or you can use the
#'            `dtm_builder()` function.
#'
#' @noRd
.remove_empty_rows <- function(dtm) {
  empty_docs <- Matrix::rowSums(dtm) == 0
  if (any(empty_docs) != TRUE) {
    return(dtm)
  } else {
    which_empty <- paste(rownames(dtm)[empty_docs], collapse = ", ")
    dtm <- dtm[!empty_docs, ]

    message(
      "The following rows were empty and removed from the DTM: ",
      which_empty
    )

    return(dtm)
  }
}
