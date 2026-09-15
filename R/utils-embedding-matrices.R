#' Find the 'projection matrix' to a semantic vector
#'
#' "Project" each word in a word embedding matrix of \eqn{D} dimension along a
#' vector of \eqn{D} dimensions, extracted from the same embedding space.
#' The vector can be a single word, or a concept vector obtained from
#' [get_centroid()], [get_direction()], or [get_regions()].
#'
#' @details
#'
#' All the vectors in the matrix \eqn{A} are projected onto the a vector,
#' \eqn{v}, to find the projection matrix, \eqn{P}, defined as:
#' \deqn{P = \frac{A \cdot v}{v \cdot v} * v}
#'
#' @param wv Matrix of word embedding vectors (a.k.a embedding model)
#'           with rows as words.
#' @param vec Vector extracted from the embeddings
#'
#' @return A new word embedding matrix,
#'         each row of which is parallel to vector.
#'
#' @examples
#' \donttest{
#' data(ft_wv_sample)
#'
#' vec <- ft_wv_sample["space", ]
#' proj <- find_projection(wv = ft_wv_sample, vec = vec)
#' }
#'
#' @export
find_projection <- function(wv, vec) {
  stopifnot(ncol(wv) == length(vec))
  if (all(vec == 0)) {
    cli::cli_abort("{.arg vec} is a zero vector; projection is undefined.")
  }

  # provides the projection of one vector
  # to another vector (as opposed to a matrix)
  # https://stackoverflow.com/a/62495884/15855390
  # as.vector( (u %*% v) / (v %*% v) ) * v
  vec <- as.vector(vec)
  # RESULT will be orthogonal to vec
  # adapted from `wordVectors` package
  # t(x) %*% y is crossprod()
  RESULT <- crossprod(
    t(wv %*% vec) /
      as.vector(vec %*% vec),
    vec
  )

  return(RESULT)
}

#' Find the 'rejection matrix' from a semantic vector
#'
#' "Reject" each word in a word embedding matrix of \eqn{D} dimension
#' from a vector of \eqn{D} dimensions, extracted from the same
#' embedding space. The vector can be a single word, or a concept
#' vector obtained from [get_centroid()], [get_direction()],
#' or [get_regions()].
#'
#' @param wv Matrix of word embedding vectors (a.k.a embedding model)
#'           with rows as words.
#' @param vec Vector extracted from the embeddings
#'
#' @return A new word embedding matrix,
#'         each row of which is rejected from vector.
#'
#' @examples
#' \donttest{
#' data(ft_wv_sample)
#'
#' vec <- ft_wv_sample["space", ]
#' rej <- find_rejection(wv = ft_wv_sample, vec = vec)
#' }
#'
#' @export
find_rejection <- function(wv, vec) {
  stopifnot(ncol(wv) == length(vec))
  # The projection of the matrix that
  # is not parallel to the vector
  # adapted from `wordVectors` package
  RESULT <- wv - find_projection(wv, vec)
  return(RESULT)
}


#' Find a specified matrix transformation
#'
#' Given a matrix, \eqn{B}, of word embedding vectors (source) with
#' terms as rows, this function finds a transformed matrix following a
#' specified operation. These include: centering (i.e.
#' translation) and normalization (i.e. scaling). In the first, \eqn{B} is
#' centered by subtracting column means. In the second, \eqn{B} is
#' normalized by the L2 norm. Both have been found to improve
#' word embedding representations. The function also finds a transformed
#' matrix that approximately aligns \eqn{B}, with another matrix,
#' \eqn{A}, of word embedding vectors (reference), using Procrustes
#' transformation (see details). Finally, given a term-co-occurrence matrix
#' built on a local corpus, the function can "retrofit" pretrained
#' embeddings to better match the local corpus.
#'
#' @details
#' Aligning a source matrix of word embedding vectors, \eqn{B}, to a
#' reference matrix, \eqn{A}, has primarily been used as a post-processing step
#' for embeddings trained on longitudinal corpora for diachronic analysis
#' or for cross-lingual embeddings. Aligning preserves internal (cosine)
#' distances, while orient the source embeddings to minimize the sum of squared
#' distances (and is therefore a Least Squares problem).
#' Alignment is accomplished with the following steps:
#'   - translation: centering by column means
#'   - scaling: scale (normalizes) by the L2 Norm
#'   - rotation/reflection: rotates and a reflects to minimize
#'     sum of squared differences, using singular value decomposition
#'
#' Alignment is asymmetrical, and only outputs the transformed source matrix,
#' \eqn{B}. Therefore, it is typically recommended to align \eqn{B} to \eqn{A},
#' and then \eqn{A} to \eqn{B}. However, simply centering and norming
#' \eqn{A} after may be sufficient.
#'
#' @references
#' Mikel Artetxe, Gorka Labaka, and Eneko Agirre. (2018).
#' 'A robust self-learning method for fully unsupervised
#' cross-lingual mappings of word embeddings.' \emph{In Proceedings
#' of the 56th Annual Meeting of the Association for
#' Computational Linguistics}. 789-798\cr
#' Mikel Artetxe, Gorka Labaka, and Eneko Agirre. 2019.
#' 'An effective approach to unsupervised machine translation.'
#' \emph{In Proceedings of the 57th Annual Meeting of the Association
#' for Computational Linguistics}. 194-203\cr
#' Hamilton, William L., Jure Leskovec, and Dan Jurafsky. (2018).
#' 'Diachronic Word Embeddings Reveal Statistical Laws of Semantic Change.'
#' \url{https://arxiv.org/abs/1605.09096v6}.\cr
#' Lin, Zefeng, Xiaojun Wan, and Zongming Guo. (2019).
#' 'Learning Diachronic Word Embeddings with Iterative Stable
#' Information Alignment.' \emph{Natural Language Processing and
#' Chinese Computing}. 749-60. \doi{10.1007/978-3-030-32233-5_58}.\cr
#' Schlechtweg et al. (2019). 'A Wind of Change: Detecting and
#' Evaluating Lexical Semantic Change across Times and Domains.'
#' \url{https://arxiv.org/abs/1906.02979v1}.
#' Shoemark et al. (2019). 'Room to Glo: A Systematic Comparison
#' of Semantic Change Detection Approaches with Word Embeddings.'
#' \emph{Proceedings of the 2019 Conference on Empirical Methods in
#' Natural Language Processing}. 66-76. \doi{10.18653/v1/D19-1007}
#' Borg and Groenen. (1997). \emph{Modern Multidimensional Scaling}.
#' New York: Springer. 340-342
#'
#' @importFrom rsvd rsvd
#'
#' @param wv Matrix of word embedding vectors (a.k.a embedding model)
#'           with rows as terms (the source matrix to be transformed).
#' @param ref If `method = "align"`, this is the reference matrix
#'            toward which the source matrix is to be aligned.
#' @param method Character vector indicating the method to use for
#'               the transformation. Current methods include: "align",
#'               "norm", "center", and "retrofit" -- see details.
#'
#'
#' @return A new word embedding matrix,
#'         transformed using the specified method.
#'
#' @examples
#' \donttest{
#' data(ft_wv_sample)
#'
#' # center and normalize
#' wv_centered <- find_transformation(ft_wv_sample, method = "center")
#' wv_normed <- find_transformation(ft_wv_sample, method = "norm")
#'
#' # align requires a reference matrix (e.g. from a different time period)
#' # wv_aligned <- find_transformation(wv_source, ref = wv_ref, method = "align")
#' }
#'
#' @export
find_transformation <- function(wv,
                                ref = NULL,
                                method = c(
                                  "align", "norm",
                                  "center", "retrofit"
                                )) {
  stopifnot(inherits(wv, "matrix") || inherits(wv, "sparseMatrix"))
  method <- match.arg(method)

  if (method %in% c("align", "retrofit") && is.null(ref)) {
    cli::cli_abort("{.arg ref} is required when {.arg method} is {.val {method}}.")
  }

  RESULT <- switch(method,
    align = .procustes_align(wv, ref),
    norm = .l2_normalize(wv),
    center = .colmeans_translate(wv),
    retrofit = .retrofit(wv, ref)
  )

  return(RESULT)
}


## INTERNAL FUNCTIONS ## -------------------------------------------------------

.procustes_align <- function(wv, ref) {
  stopifnot(ncol(wv) == ncol(ref))
  if (nrow(wv) != nrow(ref)) {
    cli::cli_abort("{.arg wv} and {.arg ref} must have the same number of rows (the same terms, in the same order) to align; got {nrow(wv)} and {nrow(ref)}.")
  }

  # scaling (scale to unit length)
  wv <- .l2_normalize(wv)
  ref <- .l2_normalize(ref)
  # column center (translation)
  # following Schlechtweg et al. (2019)
  wv <- .colmeans_translate(wv)
  ref <- .colmeans_translate(ref)
  # svd (rotation/reflection)
  rot <- rsvd::rsvd(Matrix::crossprod(ref, wv))
  Q <- Matrix::tcrossprod(rot$v, rot$u) # Q = VU^T

  return(wv %*% Q)
}

# column center (translation)
# following Schlechtweg et al. (2019)
.colmeans_translate <- function(wv) {
  return(wv - rep(
    Matrix::colMeans(wv),
    rep.int(
      nrow(wv),
      ncol(wv)
    )
  ))
}

.l2_normalize <- function(x) {
  norm_vec <- 1 / sqrt(Matrix::rowSums(x^2))
  # when sum row elements == 0
  norm_vec[is.infinite(norm_vec)] <- 0
  norm_vec[is.na(norm_vec)] <- 0

  # This is the old way...
  # if (inherits(wv, "sparseMatrix")) {
  #     return(Matrix::Diagonal(x = norm_vec) %*% wv)
  # } else {
  #     return(wv * norm_vec)
  # }
  return(x * norm_vec)
}

.retrofit <- function(wv, ref) {
  # only keep shared vocab (this could be tweaked later)
  vocab <- base::intersect(rownames(wv), rownames(ref))
  if (length(vocab) == 0) {
    cli::cli_abort("{.arg wv} and {.arg ref} share no vocabulary (no common rownames); retrofitting requires at least some overlapping terms.")
  }
  if (length(vocab) < ncol(wv)) {
    cli::cli_abort("{.arg wv} and {.arg ref} share only {length(vocab)} term{?s}, fewer than {.arg wv}'s {ncol(wv)} dimensions; retrofitting requires at least as many shared terms as {.arg wv} has dimensions.")
  }
  wv <- wv[vocab, , drop = FALSE]
  ref <- ref[vocab, vocab, drop = FALSE]

  # get SVD embeddings for TCM
  ref <- find_transformation(ref, method = "norm")
  ref <- rsvd::rsvd(ref, k = ncol(wv))
  ref <- ref$v %*% diag(ref$d)

  # align ref embeddings to pretrained
  wv <- find_transformation(wv, method = "norm")
  ref <- find_transformation(ref, wv, method = "align")

  # add ref embeddings to pretrained
  ref <- wv + ref
  ref <- find_transformation(ref, method = "norm")

  return(ref)
}


#' Check that all terms are in word embeddings, removes them or stops function
#'
#' When vectors, lists, or data.frames with terms or term pairs are input
#' to [get_direction()], [get_centroid()], or [CMDist()] this function
#' makes sure all terms are present in the word embeddings provided.
#'
#' @details
#' If `action = "remove"`, output is the same, but missing terms
#' or rows with missing terms are removed. If `action = "stop"`, the function
#' will stop with an error. In both cases, the missing terms are printed.
#'
#' @importFrom kit funique
#'
#' @param terms List or data.frame of terms or term pairs
#' @param wv Matrix of word embedding vectors (a.k.a embedding model)
#'           with rows as terms.
#' @param action what action to take if terms are not in embeddings.
#'               If action = "stop" (default), the function is stopped
#'               and an error messages states which terms are missing.
#'               If action = "remove",  missing terms or rows with missing
#'               terms are removed. Missing terms will be printed as a message.
#'
#' @return If no terms are missing, the original terms are returned
#'         If `action = "remove"`, output is same as terms,
#'         missing terms or rows with missing terms removed
#' @noRd
.check_term_in_embeddings <- function(terms, wv, action = "stop") {
  action <- match.arg(action, c("stop", "remove"))
  vocab <- kit::funique(unlist(strsplit(unlist(terms), " ")))
  bad_words <- unlist(vocab)[!(unlist(vocab) %in% rownames(wv))]

  ## If no missing terms, return terms ##
  if (identical(bad_words, character(0))) {
    return(terms)
  }

  ## Create the stop message ##
  if (length(bad_words) > 10L) {
    n_words <- length(bad_words) - 10L
    bad_words_msg <- paste(bad_words[seq_len(10)], collapse = "; ")
    bad_words_msg <- paste0(bad_words_msg, " and ", n_words, " more term(s)")
  } else {
    bad_words_msg <- paste(bad_words, collapse = "; ")
  }

  stop_msg <- paste0(
    "The following have no matching word vectors: ",
    bad_words_msg
  )

  if (action == "stop") {
    cli::cli_abort(stop_msg)
  }

  ### If we want to remove the missing terms ###
  if (action == "remove") {
    # for DATA FRAMES or TIBBLES
    if (is.data.frame(terms)) {
      any_words <- paste(bad_words, collapse = "|")

      if (ncol(terms) == 2) {
        terms <- terms[!grepl(any_words, unlist(terms[, 1])), ]
        terms <- terms[!grepl(any_words, unlist(terms[, 2])), ]
        # stop if removing term creates an empty list
        if (nrow(terms) == 0) {
          cli::cli_abort(stop_msg)
        }
      }

      if (ncol(terms) == 1) {
        terms <- terms[!grepl(any_words, unlist(terms[, 1])), ,
          drop = FALSE
        ]
        # stop if removing term creates an empty list
        if (identical(terms, character(0)) ||
          nrow(terms) == 0) {
          cli::cli_abort(stop_msg)
        }
      }
    }

    # if LIST
    # lists are used for pooling rather than paired
    if (!is.data.frame(terms) && is.list(terms)) {
      if (length(terms) == 2) {
        terms <- lapply(
          terms,
          function(x) {
            x[!x %in% bad_words]
          }
        )

        # stop if removing term creates an empty list
        if (identical(terms[[1]], character(0))) {
          cli::cli_abort(stop_msg)
        }
        if (identical(terms[[2]], character(0))) {
          cli::cli_abort(stop_msg)
        }
      }

      if (length(terms) == 1) {
        terms <- list(terms[[1]][!terms[[1]] %in% bad_words])
        # stop if removing term creates an empty list
        if (identical(terms[[1]], character(0))) {
          cli::cli_abort(stop_msg)
        }
      }
    }

    # if VECTOR (character; lists are handled above and are also
    # `is.vector()`, so this must exclude them explicitly)
    if (is.character(terms)) {
      # each element may be a multi-word phrase (e.g. "critical thinking");
      # drop the whole phrase if any of its constituent words is missing,
      # since a phrase can't be represented if part of it has no vector
      has_bad_word <- vapply(
        strsplit(terms, " ", fixed = TRUE),
        function(x) any(x %in% bad_words),
        logical(1)
      )
      terms <- terms[!has_bad_word]
      # stop if removing term creates an vector
      if (length(unlist(terms)) == 0) {
        cli::cli_abort(stop_msg)
      }
    }
  }


  message(
    "The following (and any associated terms) removed
because there are no matching word vectors: ",
    bad_words_msg
  )

  return(terms)
}


#' Positive Pointwise Mutual Information Weighting
#'
#' Computes the positive pointwise mutual information (PPMI) weighting of a
#' term co-occurrence matrix (TCM). PPMI is a standard transformation applied
#' prior to building word embeddings via matrix factorization (e.g. SVD): it
#' down-weights chance co-occurrences and clips negative associations to
#' zero, yielding a non-negative matrix amenable to factorization.
#'
#' The estimator uses marginal probabilities (row and column sums) rather
#' than a diagonal-count proxy, following the standard formulation of Levy
#' & Goldberg (2014). Smoothing, if requested, is applied to the raw counts
#' (including zero cells) before estimating probabilities.
#'
#' @param tcm A term co-occurrence matrix; either a dense `matrix` or a
#'   sparse `Matrix::dgCMatrix`. Row and column names are preserved.
#' @param smooth A non-negative numeric scalar added to every cell of `tcm`
#'   (including zeros) before estimating probabilities. Defaults to `0`,
#'   yielding standard PPMI. Values greater than zero nudge the whole
#'   distribution and reduce the influence of rare co-occurrences.
#' @return A sparse `dgCMatrix` of the same dimensions as `tcm` containing
#'   PPMI weights, with row and column names preserved.
#' @references Levy, O., & Goldberg, Y. (2014). Neural word embedding as
#'   implicit matrix factorization. *Advances in Neural Information
#'   Processing Systems (NeurIPS)*.
#' @author Dustin Stoltz
#' @examples
#' \donttest{
#' tcm <- matrix(c(2, 1, 0, 1, 3, 1, 0, 1, 2), nrow = 3,
#'               dimnames = list(c("a", "b", "c"), c("a", "b", "c")))
#' weight_ppmi(tcm)
#' }
#' @export
weight_ppmi <- function(tcm, smooth = 0) {

  if (!is.matrix(tcm) && !inherits(tcm, "sparseMatrix")) {
    cli::cli_abort(
      "{.arg tcm} must be a matrix or sparse Matrix, not {typeof(tcm)}."
    )
  }

  if (!is.numeric(smooth) || length(smooth) != 1L ||
      is.na(smooth) || smooth < 0) {
    cli::cli_abort(
      "{.arg smooth} must be a non-negative numeric scalar."
    )
  }

  if (!inherits(tcm, "dgCMatrix")) {
    tcm <- methods::as(tcm, "CsparseMatrix")
  }

  if (smooth > 0) {
    tcm <- methods::as(tcm + smooth, "CsparseMatrix")
  }

  N <- sum(tcm)
  if (N == 0) {
    out <- Matrix::Matrix(0, nrow = nrow(tcm), ncol = ncol(tcm),
                          sparse = TRUE)
    dimnames(out) <- dimnames(tcm)
    return(out)
  }

  rsum <- Matrix::rowSums(tcm)
  csum <- Matrix::colSums(tcm)

  i0 <- tcm@i
  p  <- tcm@p
  x  <- tcm@x

  col_idx <- rep(seq_along(csum), diff(p))
  row_idx <- i0 + 1L

  pmi_x <- log(x * N / (rsum[row_idx] * csum[col_idx]))
  pmi_x[!is.finite(pmi_x)] <- 0
  pmi_x[pmi_x < 0] <- 0

  keep <- pmi_x > 0
  out <- Matrix::sparseMatrix(
    i = row_idx[keep],
    j = col_idx[keep],
    x = pmi_x[keep],
    dims = tcm@Dim,
    dimnames = tcm@Dimnames,
    index1 = TRUE
  )

  out
}
