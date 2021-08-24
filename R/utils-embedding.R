#' Gets anchor terms from precompiled lists
#'
#' Produces a data.frame of juxtaposed word pairs used to extract
#' a semantic direction in word embeddings. Can be used as input
#' to `get_direction()`.
#'
#' @details
#' Sets of juxtaposed "anchor" pairs are adapted from published work
#' and associated with a particular semantic relation. These should
#' be used as a starting point, not as "ground truth."
#'
#' @name get_anchors
#' @author Dustin Stoltz
#'
#' @param relation String indicating a semantic relation, 26 relations are
#'                 available in the dataset but should be used as a starting
#'                 point. Available relations include: activity, affluence,
#'                 age, attractiveness, borders, concreteness, cultivation,
#'                 dominance, education, gender, government, purity, safety,
#'                 sexuality, skills, status, valence, whiteness
#'
#' @return returns a tibble with two columns
#'
#' @examples
#'
#' gen <- get_anchors(relation = "gender")
#'
#' @export
get_anchors <- function(relation) {

  # this allows the data to be accessed without attaching the package
  alist <- eval(parse(text = "text2map::anchor_lists"))
  anchor_list <- alist[alist$relation == relation, c("add", "subtract")]

  return(anchor_list)
}

#' Word embedding semantic direction extractor
#'
#' `get_direction()` outputs a vector corresponding to one pole of a
#' "semantic direction" built from sets of antonyms or juxtaposed terms.
#' The output can be used as an input to `CMDist()` and `CoCA()`.
#'
#' @details
#' Semantic directions can be estimated in using a few methods:
#' - 'paired' (default): each individual term is subtracted from exactly one
#'                       other paired term. there must be the same number of
#'                       words for each side of the direction (although one
#'                       word may be used more than once).
#' - 'pooled': terms corresponding to one side of a direction are first
#'             averaged, and then these averaged vectors are subtracted.
#'             A different number of terms can be used for each side of
#'             the direction.
#' - 'L2': the vector is calculated the same as with 'pooled'
#'         but is then divided by the L2 'Euclidean' norm
#' - 'PCA': vector offsets are calculated for each paired terms, as with
#'          'pooled', if `n_dirs = 1L` (the default) then the direction is the
#'          first principal component. Users can return more than one direction
#'          by increasing the `n_dirs` parameter.
#'
#' @references
#' Bolukbasi, T., Chang, K. W., Zou, J., Saligrama, V., and Kalai, A. (2016).
#' Quantifying and reducing stereotypes in word embeddings. arXiv preprint
#' \url{https://arxiv.org/abs/1606.06121v1}.\cr
#' Bolukbasi, Tolga, Kai-Wei Chang, James Zou, Venkatesh Saligrama,
#' Adam Kalai (2016). 'Man Is to Computer Programmer as Woman Is to Homemaker?
#' Debiasing Word Embeddings.' Proceedings of the 30th International Conference
#' on Neural Information Processing Systems. 4356–4364.
#' \url{https://dl.acm.org/doi/10.5555/3157382.3157584}.\cr
#' Taylor, Marshall A., and Dustin S. Stoltz. (2020)
#' 'Concept Class Analysis: A Method for Identifying Cultural
#' Schemas in Texts.' \emph{Sociological Science} 7:544-569.
#' \doi{10.15195/v7.a23}.\cr
#' Taylor, Marshall A., and Dustin S. Stoltz. (2020) 'Integrating semantic
#' directions with concept mover’s distance to measure binary concept
#' engagement.' \emph{Journal of Computational Social Science} 1-12.
#' \doi{10.1007/s42001-020-00075-8}.\cr
#' Kozlowski, Austin C., Matt Taddy, and James A. Evans. (2019). 'The geometry
#' of culture: Analyzing the meanings of class through word embeddings.'
#' \emph{American Sociological Review} 84(5):905-949.
#' \doi{10.1177/0003122419877135}.\cr
#' Arseniev-Koehler, Alina, and Jacob G. Foster. (2020). 'Machine learning
#' as a model for cultural learning: Teaching an algorithm what it means to
#' be fat.' arXiv preprint \url{https://arxiv.org/abs/2003.12133v2}.\cr
#'
#' @name get_direction
#' @author Dustin Stoltz
#'
#' @importFrom stats prcomp
#'
#' @param anchors Two column data frame of juxtaposed 'anchor' terms
#' @param wv Matrix of word embedding vectors (a.k.a embedding model)
#'           with rows as words.
#' @param method Indicates the method used to generate vector offset.
#'               Default is 'paired'. See details.
#' @param missing What action to take if words are not in embeddings.
#'               If action = "stop" (default), the function is stopped
#'               and an error messages states which words are missing.
#'               If action = "remove",  output is the same as terms but
#'               missing words or rows with missing words are removed.
#'               Missing words will be printed as a message.
#' @param n_dirs If `method = "PCA"`, an integer indicating how many directions
#'               to return. Default = `1L`, indicating a single,
#'               bipolar, direction.
#'
#' @return returns a one row matrix
#'
#' @examples
#'
#' # load example word embeddings
#' data(ft_wv_sample)
#'
#' # create anchor list
#' gen <- data.frame(
#'   add = c("woman"),
#'   subtract = c("man")
#' )
#'
#' dir <- get_direction(anchors = gen, wv = ft_wv_sample)
#'
#' dir <- get_direction(
#'   anchors = gen, wv = ft_wv_sample,
#'   method = "PCA", n = 1L
#' )
#'
#' @export
get_direction <- function(anchors, wv,
                          method = "paired",
                          missing = "stop",
                          n_dirs = 1L) {

  # check that word vectors exist for each word
  anchors <- .check_term_in_embeddings(anchors, wv, action = missing)

  if (!is.null(ncol(anchors)) &&
    ncol(anchors) == 2) {
    anchors_add <- anchors[, 1, drop = TRUE]
    anchors_sub <- anchors[, 2, drop = TRUE]
  } else {
    if (length(anchors) == 2) {
      anchors_add <- anchors[[1]]
      anchors_sub <- anchors[[2]]
    } else {
      stop("get_direction requires two sets of juxtaposing terms")
    }
  }

  # PAIRED
  # take the mean of a set of word vector differences
  # between a collection of antonym word pairs as used
  # in Kozlowski et al. 2019 and Stoltz and Taylor 2019
  if (method == "paired") {

    # subtract vectors for words in column 2 from words in column 1
    direction <- wv[anchors_add, , drop = FALSE] -
      wv[anchors_sub, , drop = FALSE]
    # get the average of the resulting differences
    direction <- t(as.matrix(colMeans(direction)))
  }

  # POOLED
  # average  the vectors for words on each pole,
  # then take the difference between these two average
  # as used in Larsen et al 2015 and Arseniev-Koehler and Foster 2020
  if (method == "pooled") {
    mu1 <- t(as.matrix(colMeans(
      wv[anchors_add, , drop = FALSE]
    )))
    mu2 <- t(as.matrix(colMeans(
      wv[anchors_sub, , drop = FALSE]
    )))
    direction <- mu1 - mu2
  }

  # L^2 Euclidean norm of PAIRED
  # as used in Bolukbasi et al. 2016
  # Quantifying and Reducing Stereotypes in Word Embeddings
  if (method == "L2") {
    direction <- wv[anchors_add, , drop = FALSE] -
      wv[anchors_sub, , drop = FALSE]
    # get the average of the resulting differences
    direction <- t(as.matrix(colMeans(direction)))
    # divide by Euclidean norm
    direction <- direction / norm(direction, type = "2")
  }

  # Direction is the first principal component
  # as used in Bolukbasi et al. 2016
  # Man is to Computer Programmer as Woman is to Homemaker?
  if (method == "PCA") {
    # yes, these are inverted for a reason
    direction <- wv[anchors_sub, , drop = FALSE] -
      wv[anchors_add, , drop = FALSE]
    direction <- t(stats::prcomp(direction,
      center = FALSE,
      rank. = n_dirs
    )$rotation)
  }

  if (!method %in% c("pooled", "paired", "L2", "PCA")) {
    stop("method must be 'pooled', 'paired', 'L2' or 'PCA'")
  }

  # create unique name for new vector
  rownames(direction) <- make.unique(
    replicate(
      n_dirs,
      paste0(anchors_add[1], "_pole")
    ),
    sep = "_"
  )

  return(direction)
}

#' Word embedding semantic centroid extractor
#'
#' `get_centroid()` requires a list of terms, one column data.frame or matrix.
#' The function outputs an averaged vector from a set of anchor terms' word
#' vectors. This average is roughly equivalent to the intersection of the
#' contexts in which each word is used. This semantic centroid can be used
#' for a variety of ends, and specifically as input to `CMDist()`.
#'
#' @name get_centroid
#' @author Dustin Stoltz
#'
#' @param anchors List of terms to be averaged
#' @param wv Matrix of word embedding vectors (a.k.a embedding model)
#'           with rows as words.
#' @param missing What action to take if anchor words are not in embeddings.
#'               If action = "stop" (default), the function is stopped
#'               and an error messages states which words are missing.
#'               If action = "remove",  output is the same as terms but
#'               missing words or rows with missing words are removed.
#'               Missing words will be printed as a message.
#'
#' @return returns a one row matrix
#'
#' @examples
#'
#' # load example word embeddings
#' data(ft_wv_sample)
#'
#' space <- c("spacecraft", "rocket", "moon")
#'
#' cen <- get_centroid(anchors = space, wv = ft_wv_sample)
#'
#' @export
get_centroid <- function(anchors, wv, missing = "stop") {

  # if data.frame or matrix convert first column to list of terms
  if (!is.null(ncol(anchors))) {
    anchors <- unlist(anchors[, 1])
  } else {
    if (is.recursive(anchors)) {
      # if nested list, only first list is used
      anchors <- unlist(anchors[[1]])
    } else {
      anchors <- unlist(anchors)
    }
  }

  # check that word vectors exist for each word
  anchors <- .check_term_in_embeddings(anchors, wv, action = missing)

  # select vectors for anchor words
  centroid <- wv[anchors[(anchors %in% rownames(wv))], , drop = FALSE]
  # average the resulting vector
  centroid <- t(as.matrix(colMeans(centroid)))

  # create unique name for new vector
  rownames(centroid) <- paste0(anchors[1], "_centroid")
  return(centroid)
}

#' Word embedding semantic region extractor
#'
#' Given a set of word embeddings of \eqn{d} dimensions and \eqn{v} vocabulary,
#' `get_regions()` finds \eqn{k} semantic regions in \eqn{d} dimensions. This, in
#' effect, learns latent topics from an embedding space (a.k.a. topic modeling),
#' which are directly comparable to both terms (with cosine similarity
#' using `get_cosine()`) and documents (with Concept Mover's distance
#' using `CMDist()`).
#'
#' @details
#' To group words into more encompassing "semantic regions" we use \eqn{k}-means
#' clustering. We choose \eqn{k}-means primarily for it's ubiquity and the wide
#' range of available diagnostic tools for \eqn{k}-means cluster.
#'
#' A word embedding matrix of \eqn{d} dimensions and \eqn{v} vocabulary is
#' "clustered" into \eqn{k} semantic regions which have \eqn{d} dimensions.
#' Each region is represented by a single point defined by the \eqn{d}
#' dimensional vector. The process discretely assigns all word vectors are
#' assigned to a given region so as to minimize some error function, however
#' as the resulting regions are in the same dimensions as the word embeddings,
#' we can measure each terms similarity to each region. This, in effect,
#' is a mixed membership topic model similar to topic modeling by Latent
#' Dirichlet Allocation.
#'
#' We use the `kmeans` function from the `mlpack` package, which offers several
#' algorithms for each "Lloyd iteration," we use the "naive" as the default.
#' Options include:
#' - "naive": O(kN) Lloyd's approach
#' - "pelleg-moore": Pelleg-Moore tree-based algorithm
#' - "elkan": Elkan’s triangle-inequality based algorithm
#' - "hamerly" (default): Hamerly’s modification to Elkan’s algorithm
#' - "dualtree": dual-tree k-means
#' - "dualtree-covertree": dual-tree k-means sing the cover tree
#'
#' @references
#' Butnaru, Andrei M., and Radu Tudor Ionescu.  (2017)
#' 'From image to text classification: A novel approach
#' based on clustering word embeddings.'
#' \emph{Procedia computer science}. 112:1783-1792.
#' \doi{10.1016/j.procs.2017.08.211}.\cr
#' Zhang, Yi, Jie Lu, Feng Liu, Qian Liu, Alan Porter,
#' Hongshu Chen, and Guangquan Zhang. (2018).
#' 'Does Deep Learning Help Topic Extraction? A Kernel
#' K-Means Clustering Method with Word Embedding.'
#' \emph{Journal of Informetrics}. 12(4):1099–1117.
#' \doi{10.1016/j.joi.2018.09.004}.\cr
#' Arseniev-Koehler, Alina and Cochran, Susan D and
#' Mays, Vickie M and Chang, Kai-Wei and Foster,
#' Jacob Gates (2021) 'Integrating topic modeling
#' and word embedding to characterize violent deaths'
#' \doi{10.31235/osf.io/nkyaq}\cr
#'
#' @name get_regions
#' @author Dustin Stoltz
#'
#' @importFrom mlpack kmeans
#'
#' @param wv Matrix of word embedding vectors (a.k.a embedding model)
#'           with rows as words.
#' @param k_regions Integer indicating the k number of regions to return
#' @param max_iter Integer indicating the maximum number of iterations
#'                 before k-means terminates.
#' @param algorithm Character indicating the algorithm to use for the Lloyd
#'             iteration (’naive’, ’pelleg-moore’, ’elkan’, ’hamerly’ (default),
#'             ’dualtree’, or ’dualtree-covertree’).
#' @param seed Integer indicating a random seed. Default is 0, which calls
#'               'std::time(NULL)'.
#'
#' @return returns a matrix of class "dgCMatrix" with k rows and d dimensions
#'
#' @examples
#'
#' # load example word embeddings
#' data(ft_wv_sample)
#'
#' my.regions <- get_regions(
#'   wv = ft_wv_sample,
#'   k_regions = 10L,
#'   max_iter = 10L,
#'   algorithm = "hamerly",
#'   seed = 01984
#' )
#'
#' @export
#'
get_regions <- function(wv,
                        k_regions = 5L,
                        max_iter = 20L,
                        algorithm = "hamerly",
                        seed = 0) {
  suppressWarnings(
    # suppress warning about setting seed
    # appears to be a problem with mlpack
    regions <- mlpack::kmeans(
      clusters = k_regions,
      input = wv,
      max_iterations = max_iter,
      algorithm = algorithm,
      seed = seed
    )$centroid
  )

  rownames(regions) <- paste0("region ", seq_len(nrow(regions)))

  regions <- Matrix::Matrix(regions, sparse = TRUE)

  return(regions)
}

#' Find the 'projection matrix' to a semantic vector
#'
#' "Project" each word in a word embedding matrix of \eqn{D} dimension along a
#' vector of \eqn{D} dimensions, extracted from the same embedding space.
#' The vector can be a single word, or a concept vector obtained from
#' `get_centroid()`, `get_direction()`, or `get_regions()`.
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
  # this would provide the projection of one vector
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
#' "Reject" each word in a word embedding matrix of \eqn{D} dimension from a vector
#' of \eqn{D} dimensions, extracted from the same embedding space.
#' The vector can be a single word, or a concept vector obtained
#' from `get_centroid()`, `get_direction()`, or `get_regions()`.
#'
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



## INTERNAL FUNCTIONS ## -------------------------------------------------------

#' Check that all terms are in word embeddings, removes them or stops
#'
#' When vectors, lists, or data.frames with words or word pairs are input
#' to `get_direction()`, `get_centroid()`, or `CMDist()` this function
#' makes sure all words are present in the word embeddings provided.
#'
#' @details
#'
#' If action = "remove", output is the same as terms but missing words
#' or rows with missing words are removed. If action = "stop",  the function
#' will stop with an error. In both cases, the missing words are printed.
#'
#' @param terms List or data.frame of terms or term pairs
#' @param wv Matrix of word embedding vectors (a.k.a embedding model)
#'           with rows as words.
#' @param action what action to take if words are not in embeddings.
#'               If action = "stop" (default), the function is stopped
#'               and an error messages states which words are missing.
#'               If action = "remove",  output is the same as terms but
#'               missing words or rows with missing words are removed.
#'               Missing words will be printed as a message.
#'
#' @return If no words are missing, the original terms are returned
#'         If action = "remove", output is same as terms,
#'         missing words or rows with missing words removed
#' @noRd
.check_term_in_embeddings <- function(terms, wv, action = "stop") {
  vocab <- unique(unlist(strsplit(unlist(terms), " ")))
  bad_words <- unlist(vocab)[!(unlist(vocab) %in% rownames(wv))]

  ## If no missing words, return terms ##
  if (identical(bad_words, character(0))) {
    return(terms)
  }

  ## Create the stop message ##
  stop_msg <- paste0(
    "The following have no matching word vectors: ",
    paste(bad_words, collapse = "; ")
  )

  if (action == "stop") {
    stop(stop_msg)
  }

  ### If we want to remove the missing words ###
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
        terms <- terms[!grepl(any_words, unlist(terms[, 1])), , drop = FALSE]
        # stop if removing term creates an empty list
        if (identical(terms, character(0)) ||
          nrow(terms) == 0) {
          stop(stop_msg)
        }
      }
    }

    # if LIST
    # lists are used for pooling,
    # rather than paired semantic directions
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


#' Read word embedding models from `bin` or `vec` files
#'
#' This an internal function, currently in development. Word embeddings are
#' commonly in `bin` or `vec` (i.e. word2vec) format. This function will
#' read these files into the `R` environment.
#'
#' @details
#' Setting `n_vocab` will limit the number of rows that will be read in.
#' As rows of embeddings tend to be sorted by their frequencies in the training
#' corpus, this argument will return the most frequent words in descending
#' order. Reading may take a while and cannot be easily parallelized since
#' connection objects cannot be "shared" across processes. Therefore,
#' setting `n_vocab` can speed up this process.
#'
#' This function is adapted from \url{github.com/bmschmidt/wordVectors}
#'
#' @importFrom utils txtProgressBar setTxtProgressBar read.table
#'
#' @param path Path to word embedding model file
#' @param dims Integer indicating number of dimensions of the embeddings. If
#'             `NULL` (default), dimensions will be assumed from the file.
#' @param format Character indicating whether it is a `bin` or `vec`,
#'               If `NULL` (the default), the function will assume by the
#'               ending of the file name.
#' @param n_vocab Integer indicating number of rows (i.e. words) to read in
#'                to the environment (default is `Inf` which will read all).
#'                See details.
#'
#'
#' @return An embedding matrix
#'
#' @noRd
.read_embeddings <- function(path,
                             dims = NULL,
                             format = NULL,
                             n_vocab = Inf) {
  if (is.null(format)) {
    if (rev(strsplit(path, "\\.")[[1]])[1] == "bin") {
      message("Assuming binary format...")
      format <- "bin"
    }

    if (rev(strsplit(path, "\\.")[[1]])[1] == "vec") {
      message("Assuming word2vec format...")
      format <- "vec"
    }
  }


  if (format == "bin") {

    # open connection to file
    con <- file(path, "rb")

    # rows
    row_number <- ""
    mostRecent <- ""
    while (mostRecent != " ") {
      mostRecent <- readChar(con, 1)
      row_number <- paste0(row_number, mostRecent)
    }
    row_number <- as.integer(row_number)

    # cols
    col_number <- ""
    while (mostRecent != "\n") {
      mostRecent <- readChar(con, 1)
      col_number <- paste0(col_number, mostRecent)
    }
    col_number <- as.integer(col_number)


    if (n_vocab < row_number) {
      message(paste(
        "Reading the first", n_vocab,
        "rows of a binary file of",
        row_number, "rows and", col_number, "columns"
      ))
      row_number <- n_vocab
    } else {
      message(paste(
        "Reading a binary file of", row_number,
        "rows and", col_number, "columns"
      ))
    }

    returned_columns <- as.integer(col_number)
    if (!is.null(dims)) {
      returned_columns <- dims
    }

    ## Read a row
    rownames <- rep("", row_number)

    read_row_func <- function(x) {
      # create progress bar
      pb <- utils::txtProgressBar(
        min = 0,
        max = row_number,
        style = 3
      )

      out <- vapply(
        seq_len(x),
        function(i, pb) {
          utils::setTxtProgressBar(pb, i)
          rowname <- ""
          mostRecent <- ""
          while (TRUE) {
            mostRecent <- readChar(con, 1)
            if (mostRecent == " ") {
              break
            }
            if (mostRecent != "\n") {
              # Some versions end with newlines, some don't.
              rowname <- paste0(rowname, mostRecent)
            }
          }
          rownames[i] <<- rowname

          row <- readBin(con,
            numeric(),
            size = 4,
            n = col_number,
            endian = "little"
          )

          if (!is.null(dims)) {
            row <- row[dims]
          }

          return(row)
        },
        FUN.VALUE = as.array(rep(0, returned_columns)),
        pb = pb
      )

      close(pb)
      close(con)

      out <- t(out)
      rownames(out) <- rownames

      return(out)
    }

    RESULT <- read_row_func(row_number)

    suppressWarnings(closeAllConnections())
  }

  if (format == "vec") {
    if (is.null(dims)) {
      test <- utils::read.table(
        path,
        header = FALSE,
        skip = 1L,
        nrows = 1L,
        quote = "",
        comment.char = ""
      )

      returned_columns <- ncol(test) - 1L
    } else {
      returned_columns <- dims
    }

    # read .vec files
    wv_mat <- utils::read.table(
      path,
      header = FALSE,
      skip = 1,
      nrows = n_vocab,
      colClasses = c("character", rep("numeric", returned_columns)),
      quote = "",
      comment.char = ""
    )

    names(wv_mat)[1] <- "word"
    wv_mat$word[is.na(wv_mat$word)] <- "NA"

    RESULT <- as.matrix(wv_mat[, colnames(wv_mat) != "word"])
    rownames(RESULT) <- wv_mat$word
  }

  colnames(RESULT) <- paste0("V", seq_len(returned_columns))
  return(RESULT)
}
