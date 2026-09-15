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
#' * Only tokenizes unigrams. No bigrams, trigrams, etc...
#' * Columns are in the order unique terms are discovered
#' * No preprocessing during building
#' * Outputs a basic sparse Matrix or dense matrix
#'
#' Weighting or stopping terms can be done efficiently after the fact with
#' simple matrix operations, rather than achieved implicitly within the
#' function itself. For example, using the `dtm_stopper()` function.
#' Prior to creating the DTM, texts should have whitespace trimmed, if
#' desired, punctuation removed and terms lowercased.
#'
#' Like `tidytext`'s DTM functions, `dtm_builder()` is optimized for use
#' in a pipeline, but unlike `tidytext`, it does not build an intermediary
#' tripletlist, so `dtm_builder()` is faster and far more memory
#' efficient.
#'
#' The function can also `chunk` the corpus into documents of a given length
#' (default is `NULL`). If the integer provided is `200L`, this will divide
#' the corpus into new documents with 200 terms (with the final document
#' likely including slightly less than 200). If the total terms in the
#' corpus were less than or equal to `chunk` integer, this would produce
#' a DTM with one document (most will probably not want this).
#'
#' If the vocabulary is already known, or standardizing vocabulary across
#' several DTMs is desired, a list of terms can be provided to the `vocab`
#' argument. Columns of the DTM will be in the order of the list of terms.
#'
#' @importFrom stringi stri_split
#' @importFrom dplyr pull
#' @importFrom kit funique
#' @importFrom Matrix sparseMatrix
#' @importFrom methods as
#' @importFrom rlang enquo
#' @importClassesFrom Matrix dgCMatrix
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
#' @param omit_empty Logical (default = `FALSE`) indicating whether to omit rows
#'                   that have zero tokens.
#' @param dense The default (`FALSE`) is to return a matrix of class
#'              "dgCMatrix" as DTMs typically have
#'              mostly zero cells. This is much more memory efficient.
#'              Setting dense to `TRUE` will return a normal base `R` matrix.
#'
#' @return returns a document-term matrix of class "dgCMatrix"
#'         or class "matrix"
#'
#' @examples
#'
#' library(dplyr)
#'
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
#' # example 1 with R 4.1 pipe
#' \donttest{
#' dtm <- my_corpus |>
#'   dtm_builder(clean_text, line_id)
#' }
#'
#' # example 2 without pipe
#' dtm <- dtm_builder(
#'   data = my_corpus,
#'   text = clean_text,
#'   doc_id = line_id
#' )
#'
#' # example 3 with dplyr pipe and mutate
#' \donttest{
#' dtm <- my_corpus %>%
#'   mutate(
#'     clean_text = gsub("'", "", text),
#'     clean_text = tolower(clean_text)
#'   ) %>%
#'   dtm_builder(clean_text, line_id)
#'
#' # example 4 with dplyr and chunk of 3 terms
#' dtm <- my_corpus %>%
#'   dtm_builder(clean_text,
#'     line_id,
#'     chunk = 3L
#'   )
#' }
#'
#' # example 5 with user defined vocabulary
#' my.vocab <- c("wonderful", "world", "haiku", "think")
#'
#' dtm <- dtm_builder(
#'   data = my_corpus,
#'   text = clean_text,
#'   doc_id = line_id,
#'   vocab = my.vocab
#' )
#' @export
dtm_builder <- function(data,
                        text,
                        doc_id = NULL,
                        vocab = NULL,
                        chunk = NULL,
                        dense = FALSE,
                        omit_empty = FALSE) {
  # checks/creates doc_ids
  doc_id <- rlang::enquo(doc_id)
  if (!rlang::quo_is_null(doc_id)) {
    docs <- tryCatch(
      {
        dplyr::pull(data, {{ doc_id }})
      },
      error = function(e) {
        .dtm_error_handler(e)
      }
    )
    if (anyNA(docs)) {
      cli::cli_abort("{.arg doc_id} contains NA; every document must have an ID.")
    }
  } else {
    # add padding to numbers so it sorts in the order of data
    docs <- paste0(
      "doc_",
      stringi::stri_pad_left(
        seq_len(nrow(data)),
        nchar(nrow(data)),
        pad = "0"
      )
    )
  }

  text_col <- dplyr::pull(data, {{ text }})
  if (anyNA(text_col)) {
    cli::cli_abort("{.arg text} contains NA; remove or replace missing text before building a DTM.")
  }

  # tokenizes by the fixed space pattern
  # outputs a nested list of tokens
  tokns <- stringi::stri_split(
    text_col,
    fixed = " ", omit_empty = TRUE
  )

  # vectorize token-list into one
  # vector of tokens for entire corpus
  vects <- unlist(tokns,
    recursive = FALSE,
    use.names = FALSE
  )

  if (is.null(vocab)) {
    # get all unique terms in the corpus
    # they will be in the order they first appear
    vocab <- kit::funique(vects)

    if (!is.null(chunk)) {
      dtm <- Matrix::sparseMatrix(
        i = ceiling(seq_along(vects) / as.integer(chunk)),
        j = fastmatch::fmatch(vects, vocab, nomatch = 0L),
        x = 1L
      )
      # create chunk IDs
      ch_ids <- paste0(
        "chunk_",
        stringi::stri_pad_left(
          seq_len(nrow(dtm)),
          nchar(nrow(dtm)),
          pad = "0"
        )
      )

      dimnames(dtm) <- list(ch_ids, vocab)
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
            dims = c(length(docs), length(vocab)),
            dimnames = list(docs, vocab)
          )
        },
        error = function(e) {
          .dtm_error_handler(e)
        }
      )
    }

    ## omit empty rows
    if (omit_empty) {
      dtm <- .remove_empty_rows(dtm)
    }

    if (dense) {
      return(as.matrix(dtm))
    } else {
      return(dtm)
    }
  }
  # if a vocabulary is provided
  if (!is.null(vocab)) {
    if (length(vocab) == 0) {
      cli::cli_abort("{.arg vocab} is empty; provide at least one term or leave it {.code NULL}.")
    }
    if (anyDuplicated(vocab) > 0) {
      cli::cli_abort("{.arg vocab} contains duplicate terms: {.val {unique(vocab[duplicated(vocab)])}}.")
    }
    vocab <- c(vocab, "INT_VC_MISS")
    vects <- c(vects, "INT_VC_MISS")
    tokns <- append(tokns, "INT_VC_MISS")
    docs <- c(docs, "INT_DOC_EMPTY")

    if (!is.null(chunk)) {
      # drop only the "INT_VC_MISS" sentinel appended above before computing
      # chunk membership: chunk position is purely positional
      # (ceiling(pos/chunk)), unlike the document-index grouping used below,
      # so including the sentinel here silently shifts/adds a chunk row
      # instead of landing in a row that gets dropped by name.
      # Out-of-vocabulary tokens are kept in the position sequence and
      # bucketed into "INT_VC_MISS" (nomatch = length(vocab)), matching the
      # non-chunk branch below -- otherwise `chunk` would silently mean
      # "N in-vocabulary tokens" instead of "N tokens of the text", and
      # chunk contents would shift depending on how many OOV words appear.
      real_vects <- vects[-length(vects)]
      n_chunks <- ceiling(length(real_vects) / as.integer(chunk))

      dtm <- Matrix::sparseMatrix(
        i = ceiling(seq_along(real_vects) / as.integer(chunk)),
        j = fastmatch::fmatch(real_vects, vocab, nomatch = length(vocab)),
        dims = c(n_chunks, length(vocab)),
        x = 1L,
        dimnames = list(paste0("chunk_", seq_len(n_chunks)), vocab)
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
          .dtm_error_handler(e)
        }
      )
    }

    ## omit empty rows
    if (omit_empty) {
      dtm <- .remove_empty_rows(dtm)
    }

    if (dense) {
      return(as.matrix(dtm[
        rownames(dtm) != "INT_DOC_EMPTY",
        colnames(dtm) != "INT_VC_MISS",
        drop = FALSE
      ]))
    } else {
      return(dtm[
        rownames(dtm) != "INT_DOC_EMPTY",
        colnames(dtm) != "INT_VC_MISS",
        drop = FALSE
      ])
    }
  }
}


#' Gets DTM summary statistics
#'
#' `dtm_stats()` provides a summary of corpus-level statistics
#' using any document-term matrix. These include (1) basic information
#' on size (total documents, total unique terms, total tokens),
#' (2) lexical richness, (3) distribution information,
#' (4) central tendency, and (5) character-level information.
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
#'         statistic is a column.
#'
#' @examples
#' \donttest{
#' data(jfk_speech)
#' jfk_speech$sentence <- tolower(jfk_speech$sentence)
#' jfk_speech$sentence <- gsub("[[:punct:]]+", " ", jfk_speech$sentence)
#' dtm <- dtm_builder(jfk_speech, sentence, sentence_id)
#'
#' dtm_stats(dtm)
#' dtm_stats(dtm, simplify = TRUE)
#' }
#'
#' @export
dtm_stats <- function(dtm,
                      richness = TRUE,
                      distribution = TRUE,
                      central = TRUE,
                      character = TRUE,
                      simplify = FALSE) {
  # convert all DTMs to dgCMatrix class
  dtm <- .convert_mat_to_dgCMatrix(dtm)

  if (nrow(dtm) == 0 || ncol(dtm) == 0) {
    cli::cli_warn("{.arg dtm} has {nrow(dtm)} row(s) and {ncol(dtm)} column(s); most statistics below will be {.val NaN}/{.val NA}.")
  }

  # basic stats
  sparsity <- 1 - Matrix::nnzero(dtm) / (nrow(dtm) * ncol(dtm))
  doc_count <- Matrix::rowSums(dtm)
  doc_bins <- Matrix::rowSums(dtm != 0)

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
  if (isTRUE(richness)) {
    term_freqs <- Matrix::colSums(dtm)
    hapax <- sum(term_freqs == 1) / ncol(dtm)
    dis <- sum(term_freqs == 2) / ncol(dtm)
    tris <- sum(term_freqs == 3) / ncol(dtm)

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
  if (isTRUE(distribution)) {
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
  if (isTRUE(central)) {
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
  if (isTRUE(character)) {
    char.len <- nchar(colnames(dtm))

    tab5 <- data.frame(
      min_length = as.integer(base::min(char.len)),
      max_length = as.integer(base::max(char.len)),
      mu_length = round(base::mean(char.len), 2),
      row.names = NULL
    )

    tab <- cbind(tab, tab5)
  }

  if (isTRUE(simplify)) {
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

    if (isTRUE(richness)) {
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
    if (isTRUE(distribution)) {
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
    if (isTRUE(central)) {
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
    if (isTRUE(character)) {
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
#' `dtm_stopper` will "stop" terms from the analysis by removing columns in a
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
#' The `stop_termrank` argument provides the upper threshold for a terms' rank
#' in the corpus. For example, `5L` will remove the five most frequent terms.
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
#' time in the corpus -- called hapax legomena. Typically, a size-able portion
#' of the corpus tends to be hapax terms, and removing them is a quick solution
#' to reducing the dimensions of a DTM. The DTM must be frequency counts (not
#' relative frequencies).
#'
#' The `stop_null` argument removes terms that do not occur at all.
#' In other words, there is a column for the term, but the entire column
#' is zero. This can occur for a variety of reasons, such as starting with
#' a predefined vocabulary (e.g., using [dtm_builder]'s `vocab` argument) or
#' through some cleaning processes.
#'
#' The `omit_empty` argument will remove documents that are empty
#'
#' @name dtm_stopper
#' @author Dustin Stoltz
#'
#' @importFrom Matrix colSums
#'
#' @param dtm Document-term matrix with terms as columns. Works with DTMs
#'            produced by any popular text analysis package, or you can use the
#'            `dtm_builder` function.
#' @param stop_list Vector of terms, from a precompiled stoplist or
#'                  custom list such as `c("never", "gonna", "give")`.
#' @param stop_termfreq Vector of two numbers indicating the lower and upper
#'                  threshold for exclusion (see details). Use `Inf`
#'                  for max or min, respectively.
#' @param stop_termprop Vector of two numbers indicating the lower and upper
#'                  threshold for exclusion (see details). Use `Inf`
#'                  for max or min, respectively.
#' @param stop_termrank Single integer indicating upper term rank threshold
#'                      for exclusion (see details).
#' @param stop_docfreq Vector of two numbers indicating the lower and upper
#'                     threshold for exclusion (see details). Use `Inf`
#'                      for max or min, respectively.
#' @param stop_docprop Vector of two numbers indicating the lower and upper
#'                      threshold for exclusion (see details). Use `Inf`
#'                      for max or min, respectively.
#' @param stop_hapax Logical (default = FALSE) indicating whether to remove
#'                   terms occurring one time (or zero times), a.k.a.
#'                   hapax legomena
#' @param stop_null Logical (default = FALSE) indicating whether to remove terms
#'                  that occur zero times in the DTM.
#' @param omit_empty Logical (default = FALSE) indicating whether to omit rows
#'                   that are empty after stopping any terms.
#' @param dense The default (`FALSE`) is to return a matrix of class
#'              "dgCMatrix". Setting dense to `TRUE` will return a
#'              normal base `R` dense matrix.
#' @param ignore_case Logical (default = TRUE) indicating whether to ignore
#'                   capitalization.
#'
#' @return returns a document-term matrix of class "dgCMatrix"
#'
#' @export
#'
#' @examples
#'
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
#' ## example 1 with R 4.1 pipe
#' \donttest{
#' dtm_st <- dtm |>
#'   dtm_stopper(stop_list = c("world", "babies"))
#' }
#'
#' ## example 2 without pipe
#' dtm_st <- dtm_stopper(
#'   dtm,
#'   stop_list = c("world", "babies")
#' )
#'
#' ## example 3 precompiled stoplist
#' dtm_st <- dtm_stopper(
#'   dtm,
#'   stop_list = get_stoplist("snowball2014")
#' )
#'
#' ## example 4, stop top 2
#' dtm_st <- dtm_stopper(
#'   dtm,
#'   stop_termrank = 2L
#' )
#'
#' ## example 5, stop docfreq
#' dtm_st <- dtm_stopper(
#'   dtm,
#'   stop_docfreq = c(2, 5)
#' )
#'
dtm_stopper <- function(dtm,
                        stop_list = NULL,
                        stop_termfreq = NULL,
                        stop_termrank = NULL,
                        stop_termprop = NULL,
                        stop_docfreq = NULL,
                        stop_docprop = NULL,
                        stop_hapax = FALSE,
                        stop_null = FALSE,
                        omit_empty = FALSE,
                        dense = FALSE,
                        ignore_case = TRUE) {
  ## stop if all stop rules are NULL
  if (is.null(stop_list) &&
    is.null(stop_termfreq) &&
    is.null(stop_termrank) &&
    is.null(stop_termprop) &&
    is.null(stop_docfreq) &&
    is.null(stop_docprop) &&
    isFALSE(stop_hapax) &&
    isFALSE(stop_null) &&
    isFALSE(omit_empty)) {
    cli::cli_abort("No stop rules were provided.")
  }
  # convert all dtms to dgCMatrix class
  dtm <- .convert_mat_to_dgCMatrix(dtm)
  # get term frequencies
  term_freqs <- Matrix::colSums(dtm)

  ## Stop based on Term Frequency ##
  if (!is.null(stop_termfreq)) {
    terms_tf <- .terms_stop_termfreq(term_freqs, stop_termfreq)
  }
  ## Stop based on Term Proportion ##
  if (!is.null(stop_termprop)) {
    terms_tp <- .terms_stop_termprop(dtm, term_freqs, stop_termprop)
  }
  ## Stop based on Term Rank ##
  if (!is.null(stop_termrank)) {
    if (.check_whole_num(stop_termrank) &&
      length(stop_termrank) == 1L) {
      if (stop_termrank >= ncol(dtm)) {
        cli::cli_warn("{.arg stop_termrank} ({stop_termrank}) is at or above the number of terms in {.arg dtm} ({ncol(dtm)}); this will remove the entire vocabulary.")
      }
      terms_tr <- names(
        term_freqs[
          order(rank(term_freqs, "min"), decreasing = TRUE)
        ][seq_len(stop_termrank)]
      )
    } else {
      cli::cli_abort("{.arg stop_termrank} argument must be a single integer.")
    }
  }
  ## Stop based on Document Frequency ##
  if (!is.null(stop_docfreq)) {
    terms_df <- .terms_stop_docfreq(dtm, stop_docfreq)
  }
  ## Stop based on Document Proportion ##
  if (!is.null(stop_docprop)) {
    terms_dp <- .terms_stop_docprop(dtm, stop_docprop)
  }
  ## Stop hapax (terms occurring once)
  if (stop_hapax) {
    terms_hps <- names(term_freqs[term_freqs <= 1])
  }
  ## Stop null words (empty columns)
  if (stop_null) {
    terms_nul <- names(term_freqs[term_freqs == 0])
  }

  # if only stop_list is NULL, initialize the object
  if (is.null(stop_list)) {
    stop_list <- character(0)
  }
  if (is.null(stop_termfreq)) {
    terms_tf <- character(0)
  }
  if (is.null(stop_termprop)) {
    terms_tp <- character(0)
  }
  if (is.null(stop_termrank)) {
    terms_tr <- character(0)
  }
  if (is.null(stop_docfreq)) {
    terms_df <- character(0)
  }
  if (is.null(stop_docprop)) {
    terms_dp <- character(0)
  }
  if (isFALSE(stop_hapax)) {
    terms_hps <- character(0)
  }
  if (isFALSE(stop_null)) {
    terms_nul <- character(0)
  }

  # merge with all stop terms
  stop_list <- kit::funique(c(
    stop_list,
    terms_tf,
    terms_tp,
    terms_tr,
    terms_df,
    terms_dp,
    terms_hps,
    terms_nul
  ))
  ## use fast-not-match to only keep words NOT in the final stop_list
  if (isTRUE(ignore_case)) {
    indx <- tolower(colnames(dtm)) %fnin% tolower(stop_list)
  } else {
    indx <- colnames(dtm) %fnin% stop_list
  }

  ## stop words
  dtm <- dtm[, indx, drop = FALSE]

  ## omit empty rows
  if (omit_empty) {
    dtm <- .remove_empty_rows(dtm)
  }

  # force dense matrix
  if (dense) {
    dtm <- as.matrix(dtm)
  }

  return(dtm)
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
#' @examples
#' \donttest{
#' data(jfk_speech)
#' jfk_speech$sentence <- tolower(jfk_speech$sentence)
#' jfk_speech$sentence <- gsub("[[:punct:]]+", " ", jfk_speech$sentence)
#' dtm <- dtm_builder(jfk_speech, sentence, sentence_id)
#'
#' dtm_resampled <- dtm_resampler(dtm, alpha = 0.5)
#' }
#'
#' @export
dtm_resampler <- function(dtm, alpha = NULL, n = NULL) {
  # explicitly declare class for input
  dtm <- .convert_mat_to_dgCMatrix(dtm)

  if (!is.null(n) && !is.null(alpha)) {
    cli::cli_warn("`alpha` and `n` should not both be specified. Only `alpha` will be used.")
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
  sdtm <- methods::as(sdtm, "CsparseMatrix")

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
#'
#' @name vocab_builder
#' @author Dustin Stoltz
#'
#' @param data Data.frame with one column of texts
#' @param text Name of the column with documents' text
#'
#' @return returns a list of unique terms in a corpus
#'
#' @examples
#' \donttest{
#' data(jfk_speech)
#' jfk_speech$sentence <- tolower(jfk_speech$sentence)
#' jfk_speech$sentence <- gsub("[[:punct:]]+", " ", jfk_speech$sentence)
#' vocab <- vocab_builder(jfk_speech, sentence)
#' head(vocab)
#' }
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
#' Converts a DTM into a data frame with three columns:
#' documents, terms, frequency. Each row is a unique
#' document by term frequency. This is akin to `reshape2`
#' packages `melt` function, but works on a sparse matrix.
#' The resulting data frame is also equivalent to the
#' `tidytext` triplet tibble.
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
#' @return returns data frame with three columns: doc_id, term, freq
#'
#' @examples
#' \donttest{
#' data(jfk_speech)
#' jfk_speech$sentence <- tolower(jfk_speech$sentence)
#' jfk_speech$sentence <- gsub("[[:punct:]]+", " ", jfk_speech$sentence)
#' dtm <- dtm_builder(jfk_speech, sentence, sentence_id)
#'
#' dtm_melted <- dtm_melter(dtm)
#' head(dtm_melted)
#' }
#'
#' @export
dtm_melter <- function(dtm) {
  # explicitly declare class for input
  dtm <- .convert_mat_to_dgCMatrix(dtm)

  df_trpl <- Matrix::summary(dtm)
  df_trpl[, 1] <- rownames(dtm)[df_trpl$i]
  df_trpl[, 2] <- colnames(dtm)[df_trpl$j]
  colnames(df_trpl) <- c("doc_id", "term", "freq")

  return(df_trpl)
}


#' Represent Documents as Token-Integer Sequences
#'
#' First, each token in the vocabulary is mapped to an integer
#' in a lookup dictionary. Next, documents are converted to sequences
#' of integers where each integer is an index of the token
#' from the dictionary.
#'
#' @details
#' Function will return a matrix of integer sequences by default.
#' The columns will be the length of the longest document or
#' `maxlen`, with shorter documents padded with zeros. The
#' dictionary will be an attribute of the matrix accessed with
#' `attr(seq, "dic")`. If `matrix = FALSE`, the function will
#' return a list of integer sequences. The vocabulary will either
#' be each unique token in the corpus, or a the list of words
#' provided to the `vocab` argument. This kind of text
#' representation is used in [tensorflow](https://www.tensorflow.org/api_docs/python/tf/keras/preprocessing/text/Tokenizer)
#' and [keras](https://tensorflow.rstudio.com/reference/keras/texts_to_sequences).
#'
#'
#' @importFrom stringi stri_pad_left
#' @importFrom stringi stri_split
#' @importFrom kit funique
#' @importFrom fastmatch fmatch
#'
#' @name seq_builder
#' @author Dustin Stoltz
#'
#' @param data Data.frame with column of texts and column of document ids
#' @param text Name of the column with documents' text
#' @param doc_id Name of the column with documents' unique ids.
#' @param vocab Default is `NULL`, if a list of terms is provided,
#'              the function will return a DTM with terms restricted
#'              to this vocabulary. Columns will also be in the same
#'              order as the list of terms.
#' @param maxlen Integer indicating the maximum document length.
#'               If NULL (default), the length of the longest document is used.
#' @param matrix Logical, `TRUE` (default) returns a matrix, `FALSE` a list
#'
#' @return returns a matrix or list
#'
#' @examples
#' \donttest{
#' data(jfk_speech)
#' jfk_speech$sentence <- tolower(jfk_speech$sentence)
#' jfk_speech$sentence <- gsub("[[:punct:]]+", " ", jfk_speech$sentence)
#'
#' seqs <- seq_builder(jfk_speech, sentence, sentence_id)
#' dim(seqs)
#' }
#'
#' @export
seq_builder <- function(data,
                        text,
                        doc_id = NULL,
                        vocab = NULL,
                        maxlen = NULL,
                        matrix = TRUE) {
  # checks/creates doc_ids
  doc_id <- rlang::enquo(doc_id)

  if (!rlang::quo_is_null(doc_id)) {
    docs <- dplyr::pull(data, {{ doc_id }})
  } else {
    # add padding to numbers so it sorts in the order of data
    docs <- paste0(
      "doc_",
      stringi::stri_pad_left(
        seq_len(nrow(data)),
        nchar(nrow(data)),
        pad = "0"
      )
    )
  }
  # tokenizes by the fixed space pattern
  # outputs a nested list of tokens
  tokns <- stringi::stri_split(
    dplyr::pull(data, {{ text }}),
    fixed = " ", omit_empty = TRUE
  )

  if (is.null(maxlen)) {
    maxlen <- max(lengths(tokns))
  }

  tokns <- lapply(tokns, "length<-", maxlen)

  if (is.null(vocab)) {
    vects <- unlist(tokns,
      recursive = FALSE,
      use.names = FALSE
    )
    vocab <- kit::funique(vects)
    vocab <- vocab[!is.na(vocab)]
  }
  # convert to token-integer list
  tokns <- lapply(tokns,
    fastmatch::fmatch,
    vocab,
    nomatch = 0L
  )
  # get all unique integers,
  # use as index for dictionary
  dic <- vocab[
    kit::funique(
      unlist(tokns,
        recursive = FALSE,
        use.names = FALSE
      )
    )
  ]

  if (matrix) {
    tokns <- do.call(rbind, tokns)
    rownames(tokns) <- docs
    tokns <- methods::as(tokns, "CsparseMatrix")
  } else {
    names(tokns) <- docs
  }

  attr(tokns, "dic") <- dic

  return(tokns)
}



## ---- INTERNAL DTM SPECIFIC FUNCTIONS -------------------------------------- #

#' .convert_mat_to_dgCMatrix
#'
#' Takes any MATRIX class and converts it to dgCMatrix.
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
.convert_mat_to_dgCMatrix <- function(dtm) {
  if (class(dtm)[1] == "dgCMatrix") {
    return(dtm)
  }

  ## Make DTM dgCMatrix sparse ##
  # if simple_triplet_matrix like tm package
  if (inherits(dtm, "simple_triplet_matrix")) {
    # transpose TDM
    if (inherits(dtm, "TermDocumentMatrix")) {
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

  # convert base dense to sparse matrix
  if (inherits(dtm, "matrix")) {
    dtm <- Matrix::Matrix(dtm, sparse = TRUE)
  }

  # strip quanteda dfm class and other non-Matrix classes
  # by forcing conversion through the Matrix class hierarchy
  # ensure we end up with a pure dgCMatrix, not a subclass
  dtm <- methods::as(
      methods::as(
          methods::as(dtm, "dMatrix"),
          "generalMatrix"
      ), "CsparseMatrix"
  )

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
  if (!any(empty_docs)) {
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
#'
#' @keywords internal
#' @noRd
.dtm_error_handler <- function(e) {
  err <- conditionMessage(e)

  if (stringi::stri_detect_regex(
    err,
    "Dimnames\\[\\[?1\\]\\]?.*Dim\\[1\\]"
  )) {
    cli::cli_abort(
      c(
        "The document ID column is longer than the number of rows in the DTM.",
        "i" = "This is likely because the last row of the text column is empty."
      ),
      parent = e
    )
  } else if (stringi::stri_detect_fixed(err, "not found")) {
    cli::cli_abort(
      c(
        "The document ID column is not in the data frame.",
        "i" = "You may have a typo."
      ),
      parent = e
    )
  } else {
    stop(e)
  }
}

#' .terms_stop_filter
#'
#' Shared implementation behind `.terms_stop_termfreq()`,
#' `.terms_stop_termprop()`, `.terms_stop_docfreq()`, and
#' `.terms_stop_docprop()`: replaces `Inf` bounds with the natural min/max for
#' the given `mode`, validates the bounds, and returns the names of terms
#' falling outside `[min(bounds), max(bounds)]`.
#'
#' @param counts Named numeric vector of term/document counts or proportions
#' @param bounds Two-element numeric vector of stop rule bounds
#' @param mode "freq" (bounds must be integers >= 0) or "prop" (bounds must
#'             be real numbers in `[0, 1]`, and `counts` gets rounded to
#'             match the bounds' decimal precision before filtering)
#' @param arg_name Name of the calling argument, for error messages
#'
#' @keywords internal
#' @noRd
.terms_stop_filter <- function(counts, bounds, mode = c("freq", "prop"), arg_name) {
  mode <- match.arg(mode)

  if (mode == "freq") {
    # replace min/max Inf with the natural bounds
    if (is.infinite(bounds[1])) bounds[1] <- 0L
    if (is.infinite(bounds[2])) bounds[2] <- max(counts)

    # coerce to integers, if generates NA, abort
    suppressWarnings(bounds <- as.integer(bounds))
    if (anyNA(bounds)) {
      cli::cli_abort("`{arg_name}` argument must be two integers greater than or equal to 0. For example: {.code c(100L, 9000L)}.")
    }
  } else {
    # replace min/max Inf with the natural bounds
    if (is.infinite(bounds[1])) bounds[1] <- 0.00
    if (is.infinite(bounds[2])) bounds[2] <- 1.00

    if (!isTRUE(max(bounds) <= 1 && min(bounds) >= 0)) {
      cli::cli_abort("`{arg_name}` argument must be two real numbers bound between 1 and 0. For example: {.code c(0.01, 0.99)}.")
    }

    n_decs <- max(.n_decimal_places(max(bounds)), .n_decimal_places(min(bounds)))
    counts <- round(counts, n_decs)
  }

  ## find terms greater than the upper threshold OR less than the lower threshold
  names(counts[counts > max(bounds) | counts < min(bounds)])
}

#' .terms_stop_termfreq
#'
#' @param term_freqs Term frequencies
#' @param stop_termfreq Stop rules
#'
#' @keywords internal
#' @noRd
.terms_stop_termfreq <- function(term_freqs, stop_termfreq) {
  .terms_stop_filter(term_freqs, stop_termfreq, mode = "freq", arg_name = "stop_termfreq")
}

#' .terms_stop_termprop
#'
#' @param dtm Document-term matrix with terms as columns. Works with DTMs
#'            produced by any popular text analysis package, or you can use the
#'            `dtm_builder()` function.
#' @param term_freqs Term frequencies
#' @param stop_termprop Stop rules
#'
#' @keywords internal
#' @noRd
.terms_stop_termprop <- function(dtm, term_freqs, stop_termprop) {
  .terms_stop_filter(term_freqs / sum(dtm), stop_termprop, mode = "prop", arg_name = "stop_termprop")
}

#' .terms_stop_docfreq
#'
#' @param dtm Document-term matrix with terms as columns. Works with DTMs
#'            produced by any popular text analysis package, or you can use the
#'            `dtm_builder()` function.
#' @param stop_termfreq Stop rules
#'
#' @keywords internal
#' @noRd
.terms_stop_docfreq <- function(dtm, stop_docfreq) {
  .terms_stop_filter(Matrix::colSums(dtm != 0), stop_docfreq, mode = "freq", arg_name = "stop_docfreq")
}

#' .terms_stop_docprop
#'
#' @param dtm Document-term matrix with terms as columns. Works with DTMs
#'            produced by any popular text analysis package, or you can use the
#'            `dtm_builder()` function.
#' @param stop_termprop Stop rules
#'
#' @keywords internal
#' @noRd
.terms_stop_docprop <- function(dtm, stop_docprop) {
  .terms_stop_filter(Matrix::colSums(dtm != 0) / nrow(dtm), stop_docprop, mode = "prop", arg_name = "stop_docprop")
}
