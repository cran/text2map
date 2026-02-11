#' Calculate Concept Mover's Distance
#'
#' Concept Mover's Distance classifies documents of any length along a
#' continuous measure of engagement with a given concept of interest
#' using word embeddings.
#'
#' @details
#'
#' `CMDist()` requires three things: a (1) document-term matrix (DTM), a (2)
#' matrix of word embedding vectors, and (3) concept words or concept vectors.
#' The function uses *word counts* from the DTM and  *word similarities*
#' from the cosine similarity of their respective word vectors in a
#' word embedding model. The "cost" of transporting all the words in a
#' document to a single vector or a few vectors (denoting a
#' concept of interest) is the measure of engagement, with higher costs
#' indicating less engagement. For intuitiveness the output of `CMDist()`
#' is inverted such that higher numbers will indicate *more engagement*
#' with a concept of interest.
#'
#' The vector, or vectors, of the concept are specified in several ways.
#' The simplest involves selecting a single word from the word embeddings, the
#' analyst can also specify the concept by indicating a few words. The algorithm
#' then splits the overall flow between each concept word (roughly) depending on
#' which word in the document is nearest. The words need not be in the DTM, but
#' they must be in the word embeddings (the function will either stop or remove
#' words not in the embeddings).
#'
#' Instead of selecting a word already in the embedding space, the function can
#' also take a vector extracted from the embedding space in the form of a
#' centroid (which averages the vectors of several words) ,a direction (which
#' uses the offset of several juxtaposing words), or a region (which is built
#' by clustering words into $k$ regions). The [get_centroid()],
#' [get_direction()], and [get_regions()] functions will extract these.
#'
#' @references
#' Stoltz, Dustin S., and Marshall A. Taylor. (2019)
#' 'Concept Mover's Distance' \emph{Journal of Computational
#' Social Science} 2(2):293-313.
#' \doi{10.1007/s42001-019-00048-6}.\cr
#' Taylor, Marshall A., and Dustin S. Stoltz. (2020) 'Integrating semantic
#' directions with concept mover's distance to measure binary concept
#' engagement.' \emph{Journal of Computational Social Science} 1-12.
#' \doi{10.1007/s42001-020-00075-8}.\cr
#' Taylor, Marshall A., and Dustin S. Stoltz.
#' (2020) 'Concept Class Analysis: A Method for Identifying Cultural
#' Schemas in Texts.' \emph{Sociological Science} 7:544-569.
#' \doi{10.15195/v7.a23}.\cr
#'
#' @name CMDist
#' @author Dustin Stoltz and Marshall Taylor
#'
#' @importFrom Matrix sparseMatrix
#' @importFrom text2vec RWMD
#' @importFrom text2vec sim2
#' @importFrom methods as
#'
#' @param dtm Document-term matrix with words as columns. Works with DTMs
#'            produced by any popular text analysis package, or using the
#'            `dtm_builder()` function.
#' @param cw Vector with concept word(s) (e.g., `c("love", "money")`,
#'           `c("critical thinking")`)
#' @param cv Concept vector(s) as output from [get_direction()],
#'          [get_centroid()], or [get_regions()]
#'
#' @param wv Matrix of word embedding vectors (a.k.a embedding model)
#'           with rows as words.
#'
#' @param missing Indicates what action to take if words are not in embeddings.
#'               If `action = "stop"` (default), the function is stopped
#'               and an error messages states which words are missing.
#'               If `action = "remove"`,  output is the same as terms but
#'               missing words or rows with missing words are removed.
#'               Missing words will be printed as a message.
#' @param scale Logical (default = `FALSE`) uses `scale()` on output. This will
#'              set zero to the mean of the estimates, and scale by the
#'              standard deviation of the estimates. Document estimates will,
#'              therefore, be relative to other documents within that specific
#'              run, but not necessarily across discrete runs.
#'
#' @param sens_interval logical (default = `FALSE`), if `TRUE` several CMDs
#'                       will be estimate on N resampled DTMs, sensitivity
#'                       intervals are produced by returning the 2.5 and 97.5
#'                       percentiles of estimated CMDs for a given concept word
#'                       or concept vector.
#' @param alpha If `sens_interval = TRUE`, a number indicating the proportion
#'           of the document length to be resampled for sensitivity intervals.
#'           Default is 1 or 100 percent of each documents' length.
#' @param n_iters If `sens_interval = TRUE`, integer (default = 20L) indicates
#'                the number of resampled DTMs to produced for
#'                sensitivity intervals
#'
#' @param parallel Logical (default = `FALSE`), whether to parallelize estimate
#' @param threads If `parallel = TRUE`, an integer indicating
#'                attempts to connect to master before failing.
#' @param setup_timeout If `parallel = TRUE`, maximum number of seconds a worker
#'                      attempts to connect to master before failing.
#'
#' @returns Returns a data frame with the first column as document ids and each
#'          subsequent column as the CMD engagement corresponding to each
#'          concept word or concept vector. The upper and lower bound
#'          estimates will follow each unique CMD if `sens_interval = TRUE`.
#'
#' @seealso [CoCA], [get_direction], [get_centroid]
#'
#' @examples
#'
#'
#' # load example word embeddings
#' data(ft_wv_sample)
#'
#' # load example text
#' data(jfk_speech)
#'
#' # minimal preprocessing
#' jfk_speech$sentence <- tolower(jfk_speech$sentence)
#' jfk_speech$sentence <- gsub("[[:punct:]]+", " ", jfk_speech$sentence)
#'
#' # create DTM
#' dtm <- dtm_builder(jfk_speech, sentence, sentence_id)
#'
#' # example 1
#' cm.dists <- CMDist(dtm,
#'   cw = "space",
#'   wv = ft_wv_sample
#' )
#'
#' # example 2
#' space <- c("spacecraft", "rocket", "moon")
#' cen <- get_centroid(anchors = space, wv = ft_wv_sample)
#'
#' cm.dists <- CMDist(dtm,
#'   cv = cen,
#'   wv = ft_wv_sample
#' )
#' @export
#'
CMDist <- function(dtm, cw = NULL, cv = NULL, wv,
                   missing = "stop", scale = TRUE,
                   sens_interval = FALSE, alpha = 1, n_iters = 20L,
                   parallel = FALSE, threads = 2L, setup_timeout = 120L) {
  prep <- .prep_cmd_INPUT(dtm, cw, cv, wv, missing)

  # Workhorse CMD function: Linear Complexity RWMD ----------------------------
  if (parallel == FALSE) {
    fullDist <- text2vec::RWMD$new(prep$DTM, prep$wem)$sim2(prep$pDTM)
    fullDist <- t(fullDist[seq_len(prep$n_pd), , drop = FALSE])
  }

  if (parallel == TRUE) {
    fullDist <- .parDist2(prep, threads,
      setup_timeout,
      sens_interval = FALSE,
      n_iters = NULL, alpha = NULL
    )
  }

  # Calculate Sensitivity Intervals -------------------------------------------
  if (sens_interval == TRUE) {
    # Get CMDs from resampled DTMs
    if (parallel == FALSE) {
      sampList <- lapply(seq_len(n_iters), function(x) {
        sampDTM <- dtm_resampler(prep$DTM, alpha = alpha)
        sampDist <- text2vec::RWMD$new(sampDTM, prep$wem)$sim2(prep$pDTM)
        sampDist <- t(sampDist[seq_len(prep$n_pd), , drop = FALSE])

        return(sampDist)
      })
    }

    if (parallel == TRUE) {
      sampList <- .parDist2(
        prep = prep,
        threads = threads,
        setup_timeout = setup_timeout,
        sens_interval = TRUE,
        n_iters = n_iters,
        alpha = alpha
      )
    }

    # Get 0.05 and 0.95 percentile estimates
    dfInt <- .get_sensitivity_intervals(
      sampList,
      fullDist,
      prep,
      scale = scale
    )

    return(dfInt)
  }

  ## No Sensitivity Intervals -------------------------------------------------
  if (sens_interval == FALSE) {
    df <- as.data.frame(fullDist[, seq_len(prep$n_pd)])

    if (scale == TRUE) {
      df <- as.data.frame(scale(df))
    }

    #
    df <- cbind.data.frame(rownames(prep$DTM), df)
    rownames(df) <- NULL
    colnames(df) <- c("doc_id", prep$labels)
    return(df)
  }
}

#' Create lowercase alias for CMDist
#' @rdname CMDist
#' @export
cmdist <- CMDist


### INTERNAL CMD FUNCTIONS ### ------------------------------------------------

#' .prep_cmd_INPUT
#'
#' Private helper function to prepare the DTM and word embeddings, outputs
#' number of pseudo-documents and the labels for concept words and concept
#' vectors. Ensures unique labeling, and adds out of vocabulary words to DTM.
#'
#' @importFrom stringi stri_trim
#' @importFrom stringi stri_encode
#' @importFrom kit funique
#' @importFrom Matrix sparseMatrix
#'
#' @param dtm document-term matrix with words as columns
#' @param cw string of concept word or words
#' @param cv concept vectors (as output from `get_direction()`
#'           or `get_centroid()`)
#' @param wv A matrix of word embedding vectors (a.k.a embedding model)
#'           with rows as words.
#' @param missing What action to take of words are not in embeddings.
#'               If `missing = "stop"` (default), the function is stopped
#'               and an error messages states which words are missing.
#'               If action = "remove",  output is the same as terms but
#'               missing words or rows with missing words are removed.
#'               Missing words will be printed as a message.
#' @keywords internal
#' @noRd
.prep_cmd_INPUT <- function(dtm, cw = NULL, cv = NULL, wv, missing = "stop") {
  # convert all DTMs to dgCMatrix class
  if (class(dtm)[[1]] != "dgCMatrix") {
    dtm <- .convert_mat_to_dgCMatrix(dtm)
  }

  ## Output ##
  # Initialize list of:
  # (1) three matrices: dtm, pDTM, and subset word embeddings
  # (2) number of pseudo-docs
  # (3) labels for cmd output
  output <- list()
  length(output) <- 5L
  names(output) <- c("DTM", "pDTM", "wem", "n_pd", "labels")


  # ensure UTF-8 encoding
  rownames(wv) <- stringi::stri_encode(rownames(wv), to = "UTF-8")
  colnames(dtm) <- stringi::stri_encode(colnames(dtm), to = "UTF-8")

  # initialize number of pseudo-docs
  n_pd <- 0

  ## if Concept Words (cw) are provided
  if (!is.null(cw)) {
    ## ensure UTF-8 encoding
    cw <- stringi::stri_encode(cw, to = "UTF-8")
    ## Make sure there are no extra spaces for concept words
    cw <- stringi::stri_trim(cw)

    # check that concept words are in embeddings
    cw <- .check_term_in_embeddings(cw, wv, action = missing)

    # number of pseudo-docs
    n_pd <- length(cw)
    vocab <- kit::funique(unlist(strsplit(cw, " ", fixed = TRUE),
      recursive = FALSE,
      use.names = FALSE
    ))
    # create list of unique concept words
    st_cw <- strsplit(cw, " ", fixed = TRUE)

    ## add unique concept words if not in DTM already
    for (i in vocab) {
      if (!i %in% colnames(dtm)) {
        new <- matrix(0, nrow = nrow(dtm))
        colnames(new) <- i
        dtm <- cbind(dtm, new)
      }
    }
  }

  ## if Concept Vectors (cv) are provided
  # add Concept Vectors to DTM and word vector matrix
  if (!is.null(cv)) {
    n_pd <- n_pd + nrow(cv)
    # each cv must have a unique name
    rownames(cv) <- make.unique(rownames(cv), sep = "_")
    # add unique concept vectors to word embedding matrix
    wv <- rbind(wv, cv)

    cvec <- matrix(0, ncol = nrow(cv), nrow = nrow(dtm))
    colnames(cvec) <- rownames(cv)
    # add unique concept vectors to DTM
    dtm <- cbind(dtm, cvec)
    st_cv <- unlist(strsplit(colnames(cvec), " ", fixed = TRUE),
      recursive = FALSE,
      use.names = FALSE
    )
  }

  # Find shared vocabulary
  vocab <- base::intersect(colnames(dtm), rownames(wv))
  ## Prepare vocab of Word Embeddings and DTM
  wv <- wv[vocab, ]
  # Remove words in the DTM without word vectors
  dtm <- dtm[, vocab]

  # ---- OLD method of subsetting -------
  # ## Prepare vocab of Word Embeddings and DTM
  # wem <- wv[intersect(rownames(wv), colnames(dtm)), ]
  # # This is rare, but remove any NAs or RWMD won't like it
  # wem <- wem[rowSums(is.na(wem)) != ncol(wem), ]
  # # Remove words in the DTM without word vectors
  # dtm <- dtm[, intersect(colnames(dtm), rownames(wem))]

  ## New Concept Vocab
  # Create full list of unique vocab for each pseudo-doc
  if (!is.null(cv) && !is.null(cw)) {
    st <- c(st_cw, st_cv)
  }
  if (!is.null(cv) && is.null(cw)) {
    st <- st_cv
  }
  if (is.null(cv) && !is.null(cw)) {
    st <- st_cw
  }


  ## Pseudo-DTM ##
  # Initialize
  # pseudo-DTM must be at least two rows, even if only one concept word
  # creates a matrix of class ngCMatrix
  pDTM <- Matrix::sparseMatrix(
    dims = c(nrow = n_pd + 1, ncol(dtm)),
    i = {}, j = {}, repr = "C"
  )
  ## fill-in Pseudo-DTM ##
  colnames(pDTM) <- colnames(dtm)
  for (i in seq_len(n_pd)) {
    pDTM[i, st[[i]]] <- TRUE
  }
  # explicitly declare class
  pDTM <- methods::as(pDTM, "dMatrix")
  # pDTM <- methods::as(pDTM, "CsparseMatrix")
  # pDTM <- methods::as(
  #     methods::as(
  #         methods::as(pDTM, "dMatrix"),
  #         "generalMatrix"
  #     ), "CsparseMatrix"
  # )

  ## Make Labels for Each Pseudo-Doc ##
  # grabs the first word in the query for the label
  if (!is.null(cv) & !is.null(cw)) {
    cw_labs <- gsub("(^\\w+)\\s.+", "\\1", cw)
    labs <- c(cw_labs, unlist(st_cv,
      recursive = FALSE,
      use.names = FALSE
    ))
  }

  if (is.null(cv) & !is.null(cw)) {
    cw_labs <- gsub("(^\\w+)\\s.+", "\\1", cw)
    labs <- cw_labs
  }

  if (!is.null(cv) & is.null(cw)) {
    labs <- st_cv
  }

  labs <- make.unique(labs, sep = "_")

  rownames(pDTM) <- c(labs, "zee_extra_row")

  ## Output ##
  # make a list of:
  # (1) three matrices: dtm, pDTM, and subset word embeddings
  # (2) number of pseudo-docs
  # (3) labels for cmd output
  output$DTM <- dtm
  output$pDTM <- pDTM
  output$wem <- wv
  output$n_pd <- n_pd
  output$labels <- labs

  return(output)
}


#' .get_sensitivity_intervals
#'
#' Calculates the sensitivity intervals on a range of CMD estimates produced
#' by resampling the DTM.
#' 
#' For details about the algorithm used to calculate percentiles 
#' see documentation for the `quantile()` function in the stats 
#' package. The Default is type `7`: \eqn{\(m = 1-p\). 
#' \(p_k = \frac{k - 1}{n - 1}\)}, where, \(p_k = \mbox{mode}\[F(x_{k})\]\)
#'
#' @importFrom stats sd quantile
#'
#' @param sampList a list of CMD estimates on resampled DTMs
#' @param fullDist the CMD estimate from the original DTM
#' @param scale logical (default = `TRUE`) whether to scale the output
#' @param prep a list of objects output by `.prep_cmd_INPUT`
#' @param probs percentiles to be sampled (default = c(0.05, 0.95))
#' @param type algorithm used to calculate percentiles (see details)
#' @keywords internal
#' @noRd
.get_sensitivity_intervals <- function(sampList,
                                       fullDist,
                                       prep,
                                       probs = c(0.025, 0.975),
                                       type = 7,
                                       scale) {
  newList <- lapply(
    seq_along(prep$labels),
    function(i) {
      do.call(cbind, lapply(sampList, `[`, , i))
    }
  )

  newList <- lapply(
    seq_along(prep$labels),
    function(i) {
      bounds <- lapply(
        seq_len(nrow(newList[[i]])),
        function(j) {
          stats::quantile(newList[[i]][j, ],
            probs = probs,
            type = type,
            na.rm = FALSE
          )
        }
      )

      bounds <- t(do.call(cbind, bounds))

      bounds <- data.frame(
        actual = fullDist[, i],
        upper = bounds[, 2],
        lower = bounds[, 1]
      )

      if (scale == TRUE) {
        bounds <- (bounds - base::mean(bounds$actual)) /
          stats::sd(unlist(bounds,
            recursive = FALSE,
            use.names = FALSE
          ))
      }

      colnames(bounds) <- c(
        prep$labels[i],
        paste(prep$labels[i],
          c("upper", "lower"),
          sep = "_"
        )
      )

      return(bounds)
    }
  )

  # prepare estimates with sensitivity intervals
  dfInt <- do.call(cbind, newList)
  df <- cbind.data.frame(rownames(prep$DTM), dfInt)
  rownames(df) <- NULL
  colnames(df) <- c("doc_id", colnames(dfInt))

  return(df)
}

#' .parDist2
#'
#' Parallizes the computation of distances
#'
#' @importFrom foreach foreach %dopar%
#' @importFrom doParallel registerDoParallel
#' @importFrom parallel makeCluster stopCluster detectCores
#'
#' @param prep list output by .prep_cmd_INPUT
#' @param threads if parallel = TRUE, number of threads to parllalelize over
#'                (default = 2)
#' @param setup_timeout if parallel = TRUE, maximum number of seconds a worker
#'                      attempts to connect to master before failing.
#'
#' @keywords internal
#' @noRd
.parDist2 <- function(prep, threads, setup_timeout,
                      sens_interval, n_iters, alpha) {
  # error checks
  if (nrow(prep$DTM) < threads) {
    stop(paste(
      "More threads were input than your DTM has rows.\n",
      "Change to `parallel=FALSE` and retry."
    ))
  }

  if ((parallel::detectCores() - 1) < threads) {
    threads <- as.integer(parallel::detectCores() - 2)
    message(paste(
      "More threads were input than your CPU has! Reduced to",
      threads, "threads."
    ))
  }

  # Determine chunk-size to be processed by different threads
  ind <- .split_dtm(nrow(prep$DTM), threads)
  cl <- parallel::makeCluster(threads, setup_timeout = setup_timeout)
  doParallel::registerDoParallel(cl)

  if (sens_interval == FALSE) {
    # Compute distance in parallel threads
    outDist <- foreach::foreach(
      i = seq_len(nrow(ind)),
      .packages = c("text2vec", "text2map"),
      .combine = "rbind",
      .export = ".parDist2",
      .inorder = TRUE, .verbose = FALSE
    ) %dopar% {
      a <- as.integer(ind[i, 1])
      b <- as.integer(ind[i, 2])
      # Linear Complexity RWMD
      dist <- text2vec::RWMD$new(
        prep$DTM[a:b, , drop = FALSE],
        prep$wem
      )$sim2(prep$pDTM)
      dist <- t(dist[seq_len(prep$n_pd), , drop = FALSE])
    }
  }

  if (sens_interval == TRUE) {
    outDist <- foreach::foreach(
      # Compute distance in parallel threads
      i = seq_len(nrow(ind)),
      .packages = c("text2vec", "text2map"),
      .combine = "cbind",
      .export = c(".parDist2", "dtm_resampler"),
      .inorder = TRUE, .verbose = FALSE
    ) %dopar% {
      a <- as.integer(ind[i, 1])
      b <- as.integer(ind[i, 2])
      # Linear Complexity RWMD
      sampDist <- lapply(seq_len(n_iters), function(x) {
        sampDTM <- dtm_resampler(prep$DTM[a:b, ], alpha = alpha)
        sampDist <- text2vec::RWMD$new(sampDTM, prep$wem)$sim2(prep$pDTM)
        sampDist <- t(sampDist[seq_len(prep$n_pd), , drop = FALSE])
        return(sampDist)
      })
    }

    outDist <- lapply(seq_len(nrow(prep$DTM)), function(idx) {
      do.call(rbind, outDist[idx, seq_len(nrow(ind))])
    })
  }

  on.exit(parallel::stopCluster(cl))
  return(outDist)
}

#' .split_dtm
#'
#' Finds indices to split document-term matrix depending
#' upon how many cores are used in parallel processing.
#' Adapted from the bigparallelr package
#'
#' @param n_docs Integer indicating total number of documents
#' @param threads Number of threads to split over
#'
#' @return A matrix with 3 columns `lower`, `upper` and `size`.
#'
#' @keywords internal
#' @noRd
.split_dtm <- function(n_docs, threads) {
  if (threads > n_docs) {
    threads <- n_docs
  } else if (threads == 0) {
    threads <- 1
  }

  int <- n_docs / threads
  upper <- round(seq_len(threads) * int)
  lower <- c(1, upper[-threads] + 1)
  size <- c(upper[1], diff(upper))

  return(cbind(lower, upper, size))
}
