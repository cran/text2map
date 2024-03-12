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
#' @export
find_projection <- function(wv, vec) {
  stopifnot(ncol(wv) == length(vec))

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
#' and then \eqn{A} to \eqn{B}. However, simplying centering and norming
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
#' Shoemark et a. (2019). 'Room to Glo: A Systematic Comparison
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
#'               "norm", "center", and "refrofit" -- see details.
#'
#'
#' @return A new word embedding matrix,
#'         transformed using the specified method.
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
  wv <- wv[vocab, ]
  ref <- ref[vocab, vocab]

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
    bad_words_msg <- paste(bad_words_msg, " and ", n_words, " more term(s)")
  } else {
    bad_words_msg <- bad_words
  }

  stop_msg <- paste0(
    "The following have no matching word vectors: ",
    paste(bad_words, collapse = "; ")
  )

  if (action == "stop") {
    stop(stop_msg)
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
          stop(stop_msg)
        }
      }

      if (ncol(terms) == 1) {
        terms <- terms[!grepl(any_words, unlist(terms[, 1])), ,
          drop = FALSE
        ]
        # stop if removing term creates an empty list
        if (identical(terms, character(0)) ||
          nrow(terms) == 0) {
          stop(stop_msg)
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
          stop(stop_msg)
        }
        if (identical(terms[[2]], character(0))) {
          stop(stop_msg)
        }
      }

      if (length(terms) == 1) {
        terms <- list(terms[[1]][!terms[[1]] %in% bad_words])
        # stop if removing term creates an empty list
        if (identical(terms[[1]], character(0))) {
          stop(stop_msg)
        }
      }
    }

    # if VECTOR
    if (is.vector(terms)) {
      terms <- terms[!terms %in% bad_words]
      # stop if removing term creates an vector
      if (length(unlist(terms)) == 0) {
        stop(stop_msg)
      }
    }
  }


  message(
    "The following (and any associated terms) removed
because there are no matching word vectors: ",
    paste0(paste(bad_words, collapse = "; "))
  )

  return(terms)
}


