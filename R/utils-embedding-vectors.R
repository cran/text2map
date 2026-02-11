#' Gets anchor terms from precompiled anchor lists
#' 
#' Produces a data.frame of juxtaposed word pairs used to extract
#' a semantic direction from word embeddings. Can be used as input
#' to [get_direction()].
#'
#' @details
#' Sets of juxtaposed "anchor" pairs are adapted from published work
#' and associated with a particular semantic relation. These should
#' be used as a starting point, not as a "ground truth."
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
#'                 available in the dataset (see details).
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
  anchor_list <- alist[alist$relation == relation, c("pole1", "pole2")]

  return(anchor_list)
}

#' Word embedding semantic direction extractor
#'
#' `get_direction()` outputs a vector corresponding to one pole of a
#' "semantic direction" built from sets of antonyms or juxtaposed terms.
#' The output can be used as an input to [CMDist()] and [CoCA()]. Anchors
#' must be a two-column data.frame or a list of length == 2.
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
#' \doi{10.5555/3157382.3157584}.\cr
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
#' @param anchors A data frame or list of juxtaposed 'anchor' terms
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
#'   pole1 = c("woman"),
#'   pole2 = c("man")
#' )
#'
#' dir <- get_direction(anchors = gen, wv = ft_wv_sample)
#'
#' dir <- get_direction(
#'   anchors = gen, wv = ft_wv_sample,
#'   method = "PCA", n = 1L
#' )
#' @export
get_direction <- function(anchors, wv,
                          method = "paired",
                          missing = "stop",
                          n_dirs = 1L) {
  # ensure UTF-8 encoding
  rownames(wv) <- stringi::stri_encode(rownames(wv), to = "UTF-8")
  # check that word vectors exist for each word
  anchors[] <- .check_term_in_embeddings(anchors[], wv, action = missing)

  if (!is.null(ncol(anchors)) &&
    ncol(anchors) == 2) {
    anchors_a <- anchors[, 1, drop = TRUE]
    anchors_z <- anchors[, 2, drop = TRUE]
  } else {
    if (length(anchors) == 2) {
      anchors_a <- anchors[[1]]
      anchors_z <- anchors[[2]]
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
    direction <- wv[anchors_a, , drop = FALSE] -
      wv[anchors_z, , drop = FALSE]
    # get the average of the resulting differences
    direction <- t(as.matrix(colMeans(direction)))
  }

  # POOLED
  # average  the vectors for words on each pole,
  # then take the difference between these two average
  # as used in Larsen et al 2015 and Arseniev-Koehler and Foster 2020
  if (method == "pooled") {
    mu1 <- t(as.matrix(colMeans(
      wv[anchors_a, , drop = FALSE]
    )))
    mu2 <- t(as.matrix(colMeans(
      wv[anchors_z, , drop = FALSE]
    )))
    direction <- mu1 - mu2
  }

  # L^2 Euclidean norm of PAIRED
  # as used in Bolukbasi et al. 2016
  # Quantifying and Reducing Stereotypes in Word Embeddings
  if (method == "L2") {
    direction <- wv[anchors_a, , drop = FALSE] -
      wv[anchors_z, , drop = FALSE]
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
    direction <- wv[anchors_z, , drop = FALSE] -
      wv[anchors_a, , drop = FALSE]
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
      paste0(anchors_a[1], "_pole")
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
  # ensure UTF-8 encoding
  rownames(wv) <- stringi::stri_encode(rownames(wv), to = "UTF-8")
  anchors[] <- .check_term_in_embeddings(anchors[], wv, action = missing)

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
#' my_regions <- get_regions(
#'   wv = ft_wv_sample,
#'   k_regions = 10L,
#'   max_iter = 10L,
#'   seed = 01984
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


#' Evaluate anchor sets in defining semantic relations
#'
#' This function evaluates how well an anchor set
#' defines a semantic relations using one of two methods: 
#' pairdir (which only evaluates semantic directions) or relco
#' which evaluations semantic directions, semantic centroids
#' and compound concepts). See details.
#' 
#'
#' @details
#'
#' PairDir evaluates how parallel two anchor sets are when used to define 
#' a semantic direction. According to Boutyline and Johnston (2023):
#'
#' "We find that  PairDir -- a measure of parallelism between the offset
#' vectors (and thus of the internal reliability of the estimated relation)
#'  -- consistently outperforms other reliability metrics in
#' explaining axis accuracy."
#'
#' Boutyline and Johnston only consider analyst specified pairs. However,
#' if `all = TRUE`, all pairwise combinations of terms between each set
#' are evaluated. This can allow for unequal sets of anchors, however this 
#' increases computational complexity considerably.
#' 
#' Relco (anchor reliability coefficient) evaluates how well individual anchors
#' index a given semantic relation in comparison to a set of non-anchor words. 
#' This can be used on semantic directions, semantic relations, or compound concepts.
#' See Taylor et al (2025) for details; see also the `CMDist()` function.
#'
#' @references
#' Boutyline, Andrei, and Ethan Johnston. 2023.
#' “Forging Better Axes: Evaluating and Improving
#' the Measurement of Semantic Dimensions in Word Embeddings.”
#' \doi{10.31235/osf.io/576h3}
#' 
#' Taylor, Marshall, et al. 2025. "A Simulation-Based Slope Metric 
#' for Anchor List Reliability in Word Embedding Spaces."
#' \doi{10.31235/osf.io/sc2ub_v3}

#'
#' @importFrom text2vec sim2
#'
#'
#' @param anchors A data frame or list of 'anchor' terms
#' @param wv Matrix of word embedding vectors (a.k.a embedding model)
#'           with rows as terms.
#' @param non_anchors For 'relco', terms that are not anchors (random, unrelated, or
#'                    distinctive terms).
#' @param method Which metric used to evaluate, 'pairdir' or 'relco'
#' @param all Logical (default `FALSE`). Whether to evaluate all possible
#'            pairwise combinations of two sets of anchors. If `FALSE` only
#'            the input pairs are used in evaluation and anchor sets must be
#'            of equal lengths.
#' @param type For 'relco', indicate which kind of relation, 
#'             "direction", "centroid", "compound"
#' @param conf For 'relco', confidence interval
#' @param dir_method For 'relco' and `type = "direction"`, indicate the 
#'                   method for calculating direction ("paired", "pooled", "L2", 
#'                   "PCA"), See `get_direction()` for details.
#' @param n_runs For 'relco', number of runs
#' @param null For 'relco', null hypothesis, default is 0.
#' @param alpha For 'relco', significance level
#' @param seed For 'relco', set sampling seed
#' @param order_non_anchors Logical (default `FALSE`). For 'relco', should the order of 
#'                          the non-anchor terms be fixed between each run
#' @param summarize Logical (default `TRUE`). Returns a dataframe with AVERAGE
#'                  scores for input pairs along with each pairs' contribution.
#'                  If `summarize = FALSE`, returns a list with each
#'                  offset matrix, each contribution, and the average score.
#'
#' 
#' @return dataframe or list
#'
#' @examples
#'
#'
#' # load example word embeddings
#' data(ft_wv_sample)
#'
#' df_anchors <- data.frame(
#'   a = c("rest", "rested", "stay", "stand"),
#'   z = c("coming", "embarked", "fast", "move")
#' )
#' 
#' # test pairdir
#' test_anchors(df_anchors, ft_wv_sample, method = "pairdir")
#' test_anchors(df_anchors, ft_wv_sample, method = "pairdir", all = TRUE)
#' 
#' # test relco
#' non_anchors <- c("writ", "alloys", "ills", "atlas", "saturn", "cape", "unfolds")
#' ## centroid
#' test_anchors(df_anchors[, 1], ft_wv_sample, method = "relco", 
#'              type = "centroid", non_anchors = non_anchors)
#' ## compound
#' test_anchors(df_anchors$a, ft_wv_sample, method = "relco", 
#'              type = "compound", non_anchors = non_anchors)
#' ## direction
#' test_anchors(df_anchors, ft_wv_sample, method = "relco", 
#'              type = "direction", dir_method = "paired", 
#'              non_anchors = non_anchors)
#' test_anchors(df_anchors, ft_wv_sample, method = "relco", 
#'              type = "direction", dir_method = "pooled", 
#'              non_anchors = non_anchors)
#' @export
test_anchors <- function(anchors, 
                         wv, 
                         non_anchors = NULL,
                         method, 
                         all = FALSE, 
                         type = c("direction", "centroid", "compound"), 
                         conf = 0.95, 
                         dir_method = c("paired", "pooled", "L2", "PCA"), 
                         n_runs = 100, 
                         null = 0, 
                         alpha =  0.5,
                         seed = NULL,
                         order_non_anchors = FALSE, 
                         summarize = TRUE) {
  
  
  res <- switch(method,
    pairdir = test_anchors.pairdir(anchors, wv, all = all, summarize = summarize),
    relco = test_anchors.relco(anchors, non_anchors, wv, type, dir_method, conf, 
                               n_runs, null, alpha, seed, order_non_anchors)
  )

  return(res)
}

## INTERNAL FUNCTIONS ## -------------------------------------------------------

#' Evaluate anchor sets with PairDir
#'
#' Evaluates how well an anchor set defines a semantic direction using a
#' technique developed by Boutyline and Johnston (2023).
#'
#' @details
#' If `all = TRUE`, all pairwise combinations of terms between each set
#' are evaluated. This increases computational complexity considerably.
#'
#' @references
#' Boutyline, Andrei, and Ethan Johnston. 2023.
#' “Forging Better Axes: Evaluating and Improving
#' the Measurement of Semantic Dimensions in Word Embeddings.”
#' \doi{10.31235/osf.io/576h3}
#'
#' @importFrom text2vec sim2
#'
#' @param anchors Anchor terms
#' @param wv Matrix of word embedding vectors (a.k.a embedding model)
#'           with rows as terms.
#' @param all Logical (default `FALSE`). Whether to evaluate all possible
#'            pairwise combinations of two sets of anchors. If `FALSE` only
#'            the input pairs are used in evaluation and anchor sets must be
#'            of equal lengths.
#'
#' @return list
#' @noRd
test_anchors.pairdir <- function(anchors, wv, all = FALSE, summarize = TRUE) {

    
  if (class(anchors) %fin% "data.frame") {  

      if(ncol(anchors) != 2) {
        stop("`anchors` data.frame must only have 2 columns")
      }
    
    anchors <- as.list(anchors)

  } else if (length(anchors) != 2) stop("`anchors` list must be of length 2")

  # prep
  anchors_a <- anchors[[1]]
  anchors_z <- anchors[[2]]


  if (all) {
    # find all combinations of unique terms in each set
    df_combos <- expand.grid(unique(anchors_a), unique(anchors_z))
    anchors_a <- as.character(df_combos$Var1)
    anchors_z <- as.character(df_combos$Var2)
  } else {
    # must be sets of same size
    if (!identical(length(anchors_a), length(anchors_z))) {
      warn_msg <- paste0(
        "If method = 'pairdir' and `all = FALSE`,",
        "anchor sets must be of equal lengths."
      )
      stop(warn_msg)
    }
  }

  res <- list()
  res$offset <- wv[anchors_z, ] - wv[anchors_a, ]
  rownames(res$offset) <- paste0(anchors_a, "-", anchors_z)
  res$cosine <- text2vec::sim2(res$offset, method = "cosine", norm = "l2")

  diag(res$cosine) <- 0
  res$contribution <- rowSums(res$cosine) / (nrow(res$cosine) - 1)
  #
  res$cosine[lower.tri(res$cosine, diag = TRUE)] <- NA
  res$pair_dir <- mean(res$cosine, na.rm = TRUE)
  #
  if (summarize) {
    res <- data.frame(
      anchor_pair = c("AVERAGE", names(res$contribution)),
      pair_dir = c(res$pair_dir, res$contribution),
      row.names = NULL
    )
  }
  return(res)
}

