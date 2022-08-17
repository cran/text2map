#' Gets anchor terms from precompiled anchor lists
#'
#' Produces a data.frame of juxtaposed word pairs used to extract
#' a semantic direction in word embeddings. Can be used as input
#' to [get_direction()].
#'
#' @details
#' Sets of juxtaposed "anchor" pairs are adapted from published work
#' and associated with a particular semantic relation. These should
#' be used as a starting point, not as "ground truth."
#'
#' Available relations include:
#' - activity
#' - affluence
#' - age
#' - attractiveness
#' - borders
#' - concreteness
#' - cultivation
#' - dominance
#' - education
#' - gender
#' - government
#' - purity
#' - safety
#' - sexuality
#' - skills
#' - status
#' - valence
#' - whiteness
#'
#' @name get_anchors
#' @author Dustin Stoltz
#'
#' @param relation String indicating a semantic relation, 26 relations are
#'                 available in the dataset (see details) but should be used
#'                 as a starting point.
#'
#' @return returns a tibble with two columns
#'
#' @examples
#'
#' gen <- get_anchors(relation = "gender")
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
#' The output can be used as an input to [CMDist()] and [CoCA()].
#'
#' @details
#' Semantic directions can be estimated in using a few methods:
#' - 'paired' (default): each individual term is subtracted from exactly one
#'                       other paired term. there must be the same number of
#'                       terms for each side of the direction (although one
#'                       word may be used more than once).
#' - 'pooled': terms corresponding to one side of a direction are first
#'             averaged, and then these averaged vectors are subtracted.
#'             A different number of terms can be used for each side of
#'             the direction.
#' - 'L2': the vector is calculated the same as with 'pooled'
#'         but is then divided by the L2 'Euclidean' norm
#' - 'PCA': vector offsets are calculated for each pair of terms,
#'          as with 'paired', and if `n_dirs = 1L` (the default)
#'          then the direction is the first principal component.
#'          Users can return more than one direction by increasing
#'          the `n_dirs` parameter.
#'
#' @references
#' Bolukbasi, T., Chang, K. W., Zou, J., Saligrama, V., and Kalai, A. (2016).
#' Quantifying and reducing stereotypes in word embeddings. arXiv preprint
#' \url{https://arxiv.org/abs/1606.06121v1}.\cr
#' Bolukbasi, Tolga, Kai-Wei Chang, James Zou, Venkatesh Saligrama,
#' Adam Kalai (2016). 'Man Is to Computer Programmer as Woman Is to Homemaker?
#' Debiasing Word Embeddings.' Proceedings of the 30th International Conference
#' on Neural Information Processing Systems. 4356-4364.
#' \url{https://dl.acm.org/doi/10.5555/3157382.3157584}.\cr
#' Taylor, Marshall A., and Dustin S. Stoltz. (2020)
#' 'Concept Class Analysis: A Method for Identifying Cultural
#' Schemas in Texts.' \emph{Sociological Science} 7:544-569.
#' \doi{10.15195/v7.a23}.\cr
#' Taylor, Marshall A., and Dustin S. Stoltz. (2020) 'Integrating semantic
#' directions with concept mover's distance to measure binary concept
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
#'           with rows as terms.
#' @param method Indicates the method used to generate vector offset.
#'               Default is 'paired'. See details.
#' @param missing what action to take if terms are not in embeddings.
#'               If action = "stop" (default), the function is stopped
#'               and an error messages states which terms are missing.
#'               If action = "remove",  missing terms or rows with missing
#'               terms are removed. Missing terms will be printed as a message.
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
#'     add = c("woman"),
#'     subtract = c("man")
#' )
#'
#' dir <- get_direction(anchors = gen, wv = ft_wv_sample)
#'
#' dir <- get_direction(
#'     anchors = gen, wv = ft_wv_sample,
#'     method = "PCA", n = 1L
#' )
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
        direction <- Matrix::t(
            stats::prcomp(direction,
                center = TRUE,
                scale. = nrow(direction) > 1L,
                rank. = n_dirs
            )$rotation
        )
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
#' The function outputs an averaged vector from a set of anchor terms' word
#' vectors. This average is roughly equivalent to the intersection of the
#' contexts in which each word is used. This semantic centroid can be used
#' for a variety of ends, and specifically as input to [CMDist()].
#' `get_centroid()` requires a list of terms, string of terms, data.frame
#' or matrix. In the latter two cases, the first column will be used. The
#' vectors are aggregated using the simple average. Terms can be repeated,
#' and are therefore "weighted" by their counts.
#'
#' @name get_centroid
#' @author Dustin Stoltz
#'
#' @param anchors List of terms to be averaged
#' @param wv Matrix of word embedding vectors (a.k.a embedding model)
#'           with rows as words.
#' @param missing what action to take if terms are not in embeddings.
#'               If action = "stop" (default), the function is stopped
#'               and an error messages states which terms are missing.
#'               If action = "remove",  missing terms or rows with missing
#'               terms are removed. Missing terms will be printed as a message.
#'
#' @return returns a one row matrix
#'
#' @examples
#'
#' # load example word embeddings
#' data(ft_wv_sample)
#'
#' space1 <- c("spacecraft", "rocket", "moon")
#'
#' cen1 <- get_centroid(anchors = space1, wv = ft_wv_sample)
#'
#' space2 <- c("spacecraft rocket moon")
#' cen2 <- get_centroid(anchors = space2, wv = ft_wv_sample)
#'
#' identical(cen1, cen2)
#' @export
get_centroid <- function(anchors, wv, missing = "stop") {

    # if data.frame or matrix convert first column to list of terms
    if (!is.null(ncol(anchors))) {
        anchors <- unlist(anchors[, 1],
            recursive = FALSE,
            use.names = FALSE
        )
    } else {
        if (is.recursive(anchors)) {
            # if nested list, only first list is used
            anchors <- unlist(anchors[[1]],
                recursive = FALSE,
                use.names = FALSE
            )
        } else {
            anchors <- unlist(anchors,
                recursive = FALSE,
                use.names = FALSE
            )
        }
    }

    anchors <- unlist(
        stringi::stri_split(
            anchors,
            fixed = " ",
            omit_empty = TRUE
        ),
        recursive = FALSE,
        use.names = FALSE
    )

    # check that word vectors exist for each word
    anchors <- .check_term_in_embeddings(anchors, wv, action = missing)

    # select vectors for anchor words
    centroid <- wv[anchors[(anchors %in% rownames(wv))], , drop = FALSE]
    # average the resulting vector
    centroid <- t(as.matrix(colMeans(centroid)))

    # create unique name for new vector
    first_word <- strsplit(anchors[1], " ")[[1]][1]
    rownames(centroid) <- paste0(first_word, "_centroid")
    return(centroid)
}

#' Word embedding semantic region extractor
#'
#' Given a set of word embeddings of \eqn{d} dimensions and \eqn{v} vocabulary,
#' [get_regions()] finds \eqn{k} semantic regions in \eqn{d} dimensions.
#' This, in effect, learns latent topics from an embedding space (a.k.a.
#' topic modeling), which are directly comparable to both terms (with
#' cosine similarity) and documents (with Concept Mover's distance
#' using [CMDist()]).
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
#' We use the `KMeans_arma` function from the `ClusterR` package which
#' uses the Armadillo library.
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
#' \emph{Journal of Informetrics}. 12(4):1099-1117.
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
#' @importFrom ClusterR KMeans_arma
#'
#' @param wv Matrix of word embedding vectors (a.k.a embedding model)
#'           with rows as words.
#' @param k_regions Integer indicating the k number of regions to return
#' @param max_iter Integer indicating the maximum number of iterations
#'                 before k-means terminates.
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
#'     wv = ft_wv_sample,
#'     k_regions = 10L,
#'     max_iter = 10L,
#'     seed = 01984
#' )
#' @export
#'
get_regions <- function(wv,
                        k_regions = 5L,
                        max_iter = 20L,
                        seed = 0) {
    regions <- ClusterR::KMeans_arma(
        data = wv,
        clusters = k_regions,
        n_iter = max_iter,
        seed = seed,
        verbose = FALSE
    )

    regions <- as(regions, "matrix")

    rownames(regions) <- paste0("region_", seq_len(nrow(regions)))

    regions <- Matrix::Matrix(regions, sparse = TRUE)

    return(regions)
}

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
    vocab <- intersect(rownames(wv), rownames(ref))
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
