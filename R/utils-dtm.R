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
#' * Columns are in the order unique terms are discovered
#' * No preprocessing during building
#' * Outputs a basic sparse matrix
#'
#' Weighting or stopping terms can be done efficiently after the fact with
#' simple matrix operations, rather than achieved implicitly within the
#' function itself. For example, using the `dtm_stopper()` function.
#' Prior to creating the DTM, texts should have whitespace trimmed, if
#' desired, punctuation removed and terms lowercased.
#'
#' Like `tidytext`'s DTM functions, `dtm_builder()` is optimized for use in a
#' pipeline, but unlike `tidytext`, it does not build an intermediary
#' tripletlist, so `dtm_builder()` is faster and far more memory
#' efficient.
#'
#' The function can also `chunk` the corpus into documents of a given length
#' (default is `NULL`). If the integer provided is `200L`, this will divide the
#' corpus into new documents with 200 terms (with the final document likely
#' including slightly less than 200). If the total terms in the corpus
#' were less than or equal to `chunk` integer, this would produce a
#' DTM with one document (most will probably not want this).
#'
#' If the vocabulary is already known, or standardizing vocabulary across
#' several DTMs is desired, a list of terms can be provided to the `vocab`
#' argument. Columns of the DTM will be in the order of the list of terms.
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
#' @param data Data.frame with column of texts and column of document ids
#' @param text Name of the column with documents' text
#' @param doc_id Name of the column with documents' unique ids.
#' @param vocab Default is `NULL`, if a list of terms is provided, the function
#'              will return a DTM with terms restricted to this vocabulary.
#'              Columns will also be in the same order as the list of terms.
#' @param chunk Default is `NULL`, if an integer is provided, the function will
#'              "re-chunk" the corpus into new documents of a particular length.
#'              For example, `100L` will divide the corpus into new documents
#'              with 100 terms (with the final document likely including
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
#'     "They'll learn much more than I'll ever know",
#'     "And I think to myself",
#'     "What a wonderful world",
#'     "Yes I think to myself",
#'     "What a wonderful world"
#'   ),
#'   line_id = paste0("line", 1:6)
#' )
#' ## some text preprocessing
#' my.corpus$clean_text <- tolower(gsub("'", "", my.corpus$text))
#'
#' # example 1 with R 4.1 pipe
#' \donttest{
#' dtm <- my.corpus |>
#'   dtm_builder(clean_text, line_id)
#' }
#'
#' # example 2 without pipe
#' dtm <- dtm_builder(
#'   data = my.corpus,
#'   text = clean_text,
#'   doc_id = line_id
#' )
#'
#' # example 3 with dplyr pipe and mutate
#' dtm <- my.corpus %>%
#'   mutate(
#'     clean_text = gsub("'", "", text),
#'     clean_text = tolower(clean_text)
#'   ) %>%
#'   dtm_builder(clean_text, line_id)
#'
#' # example 4 with dplyr and chunk of 3 terms
#' dtm <- my.corpus %>%
#'   dtm_builder(clean_text,
#'     line_id,
#'     chunk = 3L
#'   )
#'
#' # example 5 with user defined vocabulary
#' my.vocab <- c("wonderful", "world", "haiku", "think")
#'
#' dtm <- dtm_builder(
#'   data = my.corpus,
#'   text = clean_text,
#'   doc_id = line_id,
#'   vocab = my.vocab
#' )
#' @export
dtm_builder <- function(data,
                        text,
                        doc_id,
                        vocab = NULL,
                        chunk = NULL) {

  # this tokenizes by the fixed space pattern
  # outputs a nested list of tokens
  tokns <- stringi::stri_split(
    dplyr::pull(data, {{ text }}),
    fixed = " ", omit_empty = TRUE
  )

  # vectorize our token-list into one
  # vector of tokens for the entire corpus
  vects <- unlist(tokns,
    recursive = FALSE,
    use.names = FALSE
  )

  if (is.null(vocab)) {

    # get all the unique terms in the corpus
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
            dimnames = list(dplyr::pull(data, {{ doc_id }}), vocab)
          )
        },
        error = function(e) {
          .dtm_error_handler(e)
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
    docs <- c(dplyr::pull(data, {{ doc_id }}), "INT_DOC_EMPTY")

    if (!is.null(chunk)) {
      vects <- vects[vects %fin% vocab]

      dtm <- Matrix::sparseMatrix(
        i = ceiling(seq_along(vects) / as.integer(chunk)),
        j = fastmatch::fmatch(vects, vocab, nomatch = 0L),
        x = 1L
      )
      dimnames(dtm) <- list(paste0("chunk_", 1:nrow(dtm)), vocab)
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
          .dtm_error_handler(e)
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
#' @param dtm Document-term matrix with terms as columns. Works with DTMs
#'            produced by any popular text analysis package, or you can use the
#'            `dtm_builder()` function.
#' @param richness Logical (default = TRUE), whether to include statistics
#'                 about lexical richness, i.e. terms that occur once,
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
#' @return A list of one to five data frames with summary statistics (if
#'         `simplify=FALSE`), otherwise a single data frame where each
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
  sparsity <- sum(dtm[] == 0) / ncol(dtm) / nrow(dtm)
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
    hapax <- sum(dtm[, colSums(dtm) == 1]) / sum(dtm)
    dis <- sum(dtm[, colSums(dtm) == 2]) / sum(dtm)
    tris <- sum(dtm[, colSums(dtm) == 3]) / sum(dtm)

    tab2 <- data.frame(
      hapax = round(hapax, 2),
      dis = round(dis, 2),
      tris = round(tris, 2),
      ttr = round(ncol(dtm) / sum(dtm), 2),
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
      sd_types = round(stats::sd(doc_bins), 2),
      sd_tokens = round(stats::sd(doc_count), 2),
      kr_types = round(.kurtosis(doc_bins), 2),
      kr_tokens = round(.kurtosis(doc_count), 2),
      sk_types = round(.skewness(doc_bins), 2),
      sk_tokens = round(.skewness(doc_count), 2),
      row.names = NULL
    )

    tab <- cbind(tab, tab3)
  }

  # term central tendencies
  if (central == TRUE) {
    tab4 <- data.frame(
      mu_types = round(base::mean(doc_bins), 2),
      mu_tokens = round(base::mean(doc_count), 2),
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
      mu_length = round(base::mean(char.len), 2),
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
        digits = 2
      ), "%")
      tab2$dis <- paste0(formatC(100 * tab2$dis,
        format = "f",
        digits = 2
      ), "%")
      tab2$tris <- paste0(formatC(100 * tab2$tris,
        format = "f",
        digits = 2
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
#' `dtm_stopper()` will "stop" terms from the analysis by removing columns in a
#' DTM based on stop rules. Rules include matching terms in a precompiled or
#' custom list, terms meeting an upper or lower document frequency threshold,
#' or terms meeting an upper or lower term frequency threshold.
#'
#' Stopping terms by removing their respective columns in the DTM is
#' significantly more efficient than searching raw text with string matching
#' and deletion rules. Behind the scenes, the function relies on
#' the `fastmatch` package to quickly match/not-match terms.
#'
#' The `stop_list` arguments takes a list of terms which are matched and
#' removed from the DTM. If `ignore_case = TRUE` (the default) then word
#' case will be ignored.
#'
#' The `stop_termfreq` argument provides rules based on a term's occurrences
#' in the DTM as a whole -- regardless of its within document frequency. If
#' real numbers between 0 and 1 are provided then terms will be removed by
#' corpus proportion. For example `c(0.01, 0.99)`, terms that are either below
#' 1% of the total tokens or above 99% of the total tokens will be removed. If
#' integers are provided then terms will be removed by total count. For example
#' `c(100, 9000)`, occurring less than 100 or more than 9000 times in the
#' corpus will be removed. This also means that if `c(0, 1)` is provided, then
#' the will only *keep* terms occurring once.
#'
#' The `stop_docfreq` argument provides rules based on a term's document
#' frequency -- i.e. the number of documents within which it occurs, regardless
#' of how many times it occurs. If real numbers between 0 and 1 are provided
#' then terms will be removed by corpus proportion. For example `c(0.01, 0.99)`,
#' terms in more than 99% of all documents or terms that are in less than 1% of
#' all documents. For example `c(100, 9000)`, then words occurring in less than
#' 100 documents or more than 9000 documents will be removed. This means that if
#' `c(0, 1)` is provided, then the function will only *keep* terms occurring in
#' exactly one document, and remove terms in more than one.
#'
#' The `stop_hapax` argument is a shortcut for removing terms occurring just one
#' time in the corpus -- called hapax legomena. Typically, a size-able portion of
#' the corpus tends to be hapax terms, and removing them is a quick solution to
#' reducing the dimensions of a DTM. The `stop_null` argument removes terms that
#' do not occur at all. In other words, there is a column for the term, but the
#' entire column is zero. This can occur for a variety of reasons, such as
#' starting with a predefined vocabulary (e.g., using [dtm_builder]'s `vocab`
#' argument) or through some cleaning processes.
#'
#' @name dtm_stopper
#' @author Dustin Stoltz
#'
#' @importFrom Matrix colSums
#'
#' @param dtm Document-term matrix with terms as columns. Works with DTMs
#'            produced by any popular text analysis package, or you can use the
#'            `dtm_builder()` function.
#' @param stop_list Vector of terms, from either a precompiled stoplist or
#'                  custom list such as `c("never", "gonna", "give")`, or a
#'                  combination of the two.
#' @param stop_termfreq Vector of two numbers indicating the lower and upper
#'                  term threshold for exclusion (see details).
#' @param stop_docfreq Vector of two numbers indicating the lower and upper
#'                  document threshold for exclusion (see details).
#' @param stop_hapax Logical (default = FALSE) indicating whether to remove terms
#'                   occurring one time (or zero times), a.k.a. hapax legomena
#' @param stop_null Logical (default = FALSE) indicating whether to remove terms
#'                  that occur zero times in the DTM.
#' @param ignore_case Logical (default = TRUE) indicating whether to ignore
#'                   capitalization when matching terms provided to `stop_list`.
#'
#' @return returns a document-term matrix of class "dgCMatrix"
#'
#' @export
dtm_stopper <- function(dtm,
                        stop_list = NULL,
                        stop_termfreq = NULL,
                        stop_docfreq = NULL,
                        stop_hapax = FALSE,
                        stop_null = FALSE,
                        ignore_case = TRUE) {

  # convert all dtms to dgCMatrix class
  if (class(dtm)[[1]] != "dgCMatrix") {
    dtm <- .convert_dtm_to_dgCMatrix(dtm)
  }
  # stop if all stop rules are NULL
  if (is.null(stop_list) &&
    is.null(stop_termfreq) &&
    is.null(stop_docfreq) &&
    stop_hapax == FALSE &&
    stop_null == FALSE) {
    stop("No stop rules were provided.")
  }

  ## remove hapax (terms occurring once)
  if (stop_hapax) {
    term_freqs <- Matrix::colSums(dtm)
    stop_haps <- names(term_freqs[term_freqs <= 1])
  }

  if (!is.null(stop_termfreq)) {
    if (!(is.numeric(stop_termfreq))) {
      stop("`stop_termfreq` argument must be either two real numbers bound\n
               between 1 and 0, or two integers greater than 0.\n
               For example: c(0.01, 0.99) or c(100L, 9000L)")
    } else {

      ## check if whole numbers
      if (isTRUE(all.equal(stop_termfreq, as.integer(stop_termfreq)))) {
        term_freqs <- Matrix::colSums(dtm)
        stop_terms <- names(
          term_freqs[term_freqs > max(stop_termfreq) |
            term_freqs < min(stop_termfreq)]
        )
      } else {
        ## check if real numbers between 1 and 0
        if (isTRUE(max(stop_termfreq) <= 1 &&
          min(stop_termfreq) >= 0)) {
          n_decs <- max(
            c(
              .n_decimal_places(max(stop_termfreq)),
              .n_decimal_places(min(stop_termfreq))
            )
          )
          term_freqs <- round(Matrix::colSums(dtm) / sum(dtm), n_decs)
          ## find terms greater than the upper threshold
          ## OR less than the lower threshold
          stop_terms <- names(
            term_freqs[term_freqs > max(stop_termfreq) |
              term_freqs < min(stop_termfreq)]
          )
        }
      }
    }
  }

  if (!is.null(stop_docfreq)) {
    if (!(is.numeric(stop_docfreq))) {
      stop("`stop_termfreq` argument must be either two real numbers bound\n
               between 1 and 0, or two integers greater than 0.\n
               For example: c(0.01, 0.99) or c(100L, 9000L)")
    } else {

      ## check if whole numbers
      if (isTRUE(all.equal(stop_docfreq, as.integer(stop_docfreq)))) {
        doc_freqs <- Matrix::colSums(dtm != 0)
        stop_docterms <- names(
          doc_freqs[doc_freqs > max(stop_docfreq) |
            doc_freqs < min(stop_docfreq)]
        )
      } else {
        ## check if real numbers between 1 and 0
        if (isTRUE(max(stop_docfreq) <= 1 &&
          min(stop_docfreq) >= 0)) {
          n_decs <- max(
            c(
              .n_decimal_places(max(stop_docfreq)),
              .n_decimal_places(min(stop_docfreq))
            )
          )
          doc_freqs <- round(Matrix::colSums(dtm != 0) / nrow(dtm), n_decs)
          ## find terms greater than or equal to the upper threshold
          ## or less than or equal to the lower threshold
          stop_docterms <- names(
            doc_freqs[doc_freqs > max(stop_docfreq) |
              doc_freqs < min(stop_docfreq)]
          )
        }
      }
    }
  }

  # if only stop_list is NULL, initialize the object
  if (is.null(stop_list)) {
    stop_list <- character(0)
  }
  if (is.null(stop_termfreq)) {
    stop_terms <- character(0)
  }
  if (is.null(stop_docfreq)) {
    stop_docterms <- character(0)
  }
  if (stop_hapax == FALSE) {
    stop_haps <- character(0)
  }
  # merge with terms in stop_list, if provided
  stop_list <- kit::funique(c(stop_list, stop_terms, stop_docterms, stop_haps))
  ## use fast-not-match to only keep words not in the final stop_list
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
#' @details
#' Using the row counts as probabilities, each document's tokens are resampled
#' with replacement up to a certain proportion of the row count (set by alpha).
#' This function can be used with iteration to "bootstrap" a DTM without
#' returning to the raw text. It does not iterate, however, so operations can
#' be performed on one DTM at a time without storing multiple DTMs in memory.
#'
#' If `alpha` is less than 1, then a proportion of each documents' lengths is
#' returned. For example, `alpha = 0.50` will return a resampled DTM where each
#' row has half the tokens of the original DTM. If `alpha = 2`, than each row in
#' the resampled DTM twice the number of tokens of the original DTM.
#' If an integer is provided to `n` then all documents will be resampled to that
#' length. For example, `n = 2000L` will resample each document until they are
#' 2000 tokens long -- meaning those shorter than 2000 will be increased in
#' length, while those longer than 2000 will be decreased in length. `alpha`
#' and `n` should not be specified at the same time.
#'
#' @importFrom Matrix sparseMatrix
#' @importFrom fastmatch fmatch
#'
#' @param dtm Document-term matrix with terms as columns. Works with DTMs
#'            produced by any popular text analysis package, or you can use the
#'            `dtm_builder()` function.
#' @param alpha Number indicating proportion of document lengths, e.g.,
#'              `alpha = 1` returns resampled rows that are the same lengths
#'              as the original DTM.
#' @param n Integer indicating the length of documents to be returned, e.g.,
#'          `n = 100L` will bring documents shorter than 100 tokens up to 100,
#'          while bringing documents longer than 100 tokens down to 100.
#'
#' @return returns a document-term matrix of class "dgCMatrix"
#'
#' @export
dtm_resampler <- function(dtm, alpha = NULL, n = NULL) {

  # explicitly declare class for input
  dtm <- .convert_dtm_to_dgCMatrix(dtm)

  if (!is.null(n) && !is.null(alpha)) {
    warning(paste0(
      "`alpha` and `n` should not be both specified.",
      " Only `alpha` will be used."
    ))
    n <- NULL
  }

  # create token lists from a random sample from each row
  samp_tokns <- lapply(
    seq_len(nrow(dtm)),
    FUN = function(i) {
      i.row <- dtm[i, ]

      if (!is.null(alpha)) {
        size <- round(sum(i.row) * alpha)
      } else {
        if (!is.null(n)) {
          size <- n
        } else {
          size <- sum(i.row)
        }
      }

      if (sum(i.row) == 0) {
        return(samp = NULL)
      } else {
        samp <- base::sample(
          colnames(dtm),
          size = size,
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
#' single whitespace, and then extracts the unique tokens. This can be used as
#' input to `dtm_builder()` to standardize the vocabulary (i.e. the columns)
#' across multiple DTMs. Prior to building the vocabulary, texts should have
#' whitespace trimmed, if desired, punctuation removed and terms lowercased.
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
#' @param data Data.frame with one column of texts
#' @param text Name of the column with documents' text
#'
#' @return returns a list of unique terms in a corpus
#'
#' @export
vocab_builder <- function(data, text) {

  # this will tokenize by the fixed space pattern
  # outputs a nested list of tokens
  tokns <- stringi::stri_split(
    dplyr::pull(data, {{ text }}),
    fixed = " ", omit_empty = TRUE
  )

  # vectorize our token-list into one
  # vector of tokens for the entire corpus
  vects <- unlist(tokns,
    recursive = FALSE,
    use.names = FALSE
  )

  # get all the unique terms in the corpus
  # they will be in the order they first appear
  vocab <- kit::funique(vects)

  return(vocab)
}

#' Melt a DTM into a triplet data frame
#'
#' Takes a DTM converts into a data frame with three columns:
#' documents, terms, counts. Each row is a unique document by term
#' count This is akin to `reshape2` packages `melt` function,
#' but works on a sparse matrix.The resulting data frame is
#' equivalent to the `tidytext` triplet tibble.
#'
#'
#' @importFrom Matrix summary
#'
#' @name dtm_melter
#' @author Dustin Stoltz
#'
#' @param dtm Document-term matrix with terms as columns. Works with DTMs
#'            produced by any popular text analysis package, or using the
#'            `dtm_builder()` function.
#'
#' @return returns data frame with three columns
#'
#' @export
dtm_melter <- function(dtm) {

  # explicitly declare class for input
  dtm <- .convert_dtm_to_dgCMatrix(dtm)

  df_trpl <- Matrix::summary(dtm)
  df_trpl[, 1] <- rownames(dtm)[df_trpl$i]
  df_trpl[, 2] <- colnames(dtm)[df_trpl$j]
  colnames(df_trpl) <- c("doc_id", "term", "count")

  return(df_trpl)
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
#' @param dtm Document-term matrix with terms as columns. Works with DTMs
#'            produced by any popular text analysis package, or you can use the
#'            `dtm_builder()` function.
#'
#' @keywords internal
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
#' @param dtm Document-term matrix with terms as columns. Works with DTMs
#'            produced by any popular text analysis package, or you can use the
#'            `dtm_builder()` function.
#'
#' @keywords internal
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

#' .dtm_error_handler
#'
#' @param e The caught error message
#' @param doc_id The document ids
#' @param text The text column
#'
#' @keywords internal
#' @noRd
.dtm_error_handler <- function(e, doc_id, text) {
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
      "\n",
      "The original error message: "
    )
    stop(e)
  } else {
    if (stringi::stri_detect_fixed(
      err,
      "not found"
    )) {
      message(
        "`", rlang::quo_name(rlang::enquo(doc_id)), "`",
        " is not in the data frame. You may have a typo.\n ",
        "If you do not have a `doc_id` defined, use:\n ",
        "data$doc_id <- paste0('doc_', 1:nrow(data))\n ",
        "\n",
        "The original error message: "
      )
      stop(e)
    } else {
      stop(e)
    }
  }
}
