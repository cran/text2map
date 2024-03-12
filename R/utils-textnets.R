#' Find a similarities between documents
#'
#' Given a document-term matrix (DTM) this function returns the
#' similarities between documents using a specified method (see details).
#' The result is a square document-by-document similarity matrix (DSM),
#' equivalent to a weighted adjacency matrix in network analysis.
#'
#' @details
#' Document similarity methods include:
#' - projection: finds the one-mode projection matrix from the two-mode DTM
#' using `tcrossprod()` which measures the shared vocabulary overlap
#' - cosine: compares row vectors using cosine  similarity
#' - jaccard: compares proportion of common words to unique words in
#' both documents
#' - wmd: word mover's distance to compare documents (requires word
#' embedding vectors), using linear-complexity relaxed word mover's distance
#' - centroid: represents each document as a centroid of their respective
#' vocabulary, then uses cosine similarity to compare centroid vectors
#' (requires word embedding vectors)
#'
#' @importFrom Matrix tcrossprod
#' @importFrom Matrix rowSums
#' @importFrom text2vec sim2
#' @importFrom text2vec RWMD
#'
#' @name doc_similarity
#' @author Dustin Stoltz
#'
#' @param x Document-term matrix with terms as columns.
#' @param y Optional second matrix (default = `NULL`).
#' @param method Character vector indicating similarity method, including
#'               projection, cosine, wmd, and centroid (see Details).
#' @param wv Matrix of word embedding vectors (a.k.a embedding model)
#'           with rows as words. Required for "wmd" and "centroid"
#'           similarities.
#'
#' @examples
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
#' dsm_prj <- doc_similarity(dtm, method = "projection")
#' dsm_cos <- doc_similarity(dtm, method = "cosine")
#' dsm_wmd <- doc_similarity(dtm, method = "wmd", wv = ft_wv_sample)
#' dsm_cen <- doc_similarity(dtm, method = "centroid", wv = ft_wv_sample)
#'
#' @export
doc_similarity <- function(x,
                           y = NULL,
                           method,
                           wv = NULL) {
    x <- .convert_mat_to_dgCMatrix(x)

    if (!is.null(y)) {
        y <- .convert_mat_to_dgCMatrix(y)
    }

    dsm <- switch(method,
        projection = Matrix::tcrossprod(x),
        cosine = text2vec::sim2(x, method = "cosine"),
        wmd = .doc_similarity.wmd(x, wv),
        centroid = .doc_similarity.centroid(x, wv)
    )

    return(dsm)
}


#' Find a specified document centrality metric
#'
#' Given a document-term matrix or a document-similarity matrix,
#' this function returns specified text network-based centrality measures.
#' Currently, this includes degree, eigenvector, betweenness, and
#' spanning.
#'
#' @details
#' If a document-term matrix is provided, the function obtains the one-mode
#' document-level projection to get the document-similarity matrix using
#' `tcrossprod()`. If a one-mode document-similarity matrix is provided, then
#' this step is skipped. This way document similiarities may be obtained
#' using other methods, such as Word-Mover's Distance (see `doc_similarity`).
#' The diagonal is ignored in all calculations.
#'
#' Document centrality methods include:
#' - degree: Opsahl's weighted degree centrality with tuning parameter "alpha"
#' - between: vertex betweenness centrality using Brandes' method
#' - eigen: eigenvector centrality using Freeman's method
#' - span: Modified Burt's constraint following Stoltz and Taylor's method,
#'   uses a tuning parameter "alpha" and the output is scaled.
#'
#' @references
#' Brandes, Ulrik
#' (2000) 'A faster algorithm for betweenness centrality'
#' \emph{Journal of Mathematical Sociology}. 25(2):163-177
#' \doi{10.1080/0022250X.2001.9990249}.\cr
#' Opsahl, Tore, et al.
#' (2010) 'Node centrality in weighted networks: Generalizing degree
#' and shortest paths.' \emph{Social Networks}. 32(3)245:251
#' \doi{10.1016/j.socnet.2010.03.006}\cr
#' Stoltz, Dustin; Taylor, Marshall
#' (2019) 'Textual Spanning: Finding Discursive Holes in Text Networks'
#' \emph{Socius}. \doi{10.1177/2378023119827674}\cr
#'
#' @importFrom Matrix tcrossprod
#' @importFrom Matrix rowSums
#' @importFrom igraph graph_from_adjacency_matrix
#' @importFrom igraph betweenness
#' @importFrom igraph E
#'
#' @name doc_centrality
#' @author Dustin Stoltz
#'
#' @param mat Document-term matrix with terms as columns or a
#'            document-similarity matrix with documents as rows and columns.
#' @param method Character vector indicating centrality method, including
#'               "degree", "eigen", "span", and "between".
#' @param alpha Number (default = 1) indicating the tuning parameter for
#'              weighted metrics.
#' @param two_mode Logical (default = TRUE), indicating whether the input matrix
#'                 is two mode (i.e. a document-term matrix) or one-mode
#'                 (i.e. document-similarity matrix)
#'
#' @return A dataframe with two columns
#'
#' @examples
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
#' ddeg <- doc_centrality(dtm, method = "degree")
#' deig <- doc_centrality(dtm, method = "eigen")
#' dbet <- doc_centrality(dtm, method = "between")
#' dspa <- doc_centrality(dtm, method = "span")
#'
#' # with a document-similarity matrix (dsm)
#'
#' dsm <- doc_similarity(dtm, method = "cosine")
#' ddeg <- doc_centrality(dsm, method = "degree", two_mode = FALSE)
#'
#' @export
doc_centrality <- function(mat,
                           method,
                           alpha = 1L,
                           two_mode = TRUE) {
    mat <- .convert_mat_to_dgCMatrix(mat)

    if (two_mode) {
        mat <- Matrix::tcrossprod(mat)
    }
    diag(mat) <- 0


    cen <- switch(method,
        degree = .doc_centrality.degree(mat, alpha),
        between = .doc_centrality.between(mat, alpha),
        eigen = .doc_centrality.eigen(mat),
        span = .doc_centrality.span(mat, alpha)
    )

    df <- data.frame(
        doc_id = rownames(mat),
        cen = cen
    )
    names(df)[2] <- method

    return(df)
}


## ---- INTERNAL TEXTNET SPECIFIC FUNCTIONS ---------------------------------- #
#' Weighted degree centrality
#'
#' @importFrom Matrix rowSums
#'
#' @param mat Input matrix
#'
#' @keywords internal
#' @noRd
.doc_centrality.degree <- function(mat, alpha) {
    non <- Matrix::rowSums(mat != 0)
    den <- Matrix::rowSums(mat)
    res <- (non) * ((den / (non))^alpha)

    return(res)
}

#' Weighted betweenness centrality
#'
#' @importFrom igraph betweenness
#' @importFrom igraph E
#' @importFrom igraph graph_from_adjacency_matrix
#'
#' @param mat Input matrix
#'
#' @keywords internal
#' @noRd
.doc_centrality.between <- function(mat, alpha) {
    mat <- igraph::graph_from_adjacency_matrix(mat,
        mode = "undirected",
        weighted = TRUE, diag = FALSE
    )
    igraph::E(mat)$weight <- (1 / igraph::E(mat)$weight)^alpha
    res <- igraph::betweenness(mat)
    return(res)
}
#' Eigenvector centrality
#' @param mat Input matrix
#'
#' @keywords internal
#' @noRd
.doc_centrality.eigen <- function(mat) {
    # first eigenvector of the adjacency matrix
    res <- abs(eigen(mat)$vector[, 1])
    return(res)
}

#' Textual spanning
#'
#' @importFrom Matrix rowSums
#' @importFrom stats sd
#'
#' @param mat Input matrix
#'
#' @keywords internal
#' @noRd
.doc_centrality.span <- function(mat, alpha) {
    mat[] <- mat[] / .doc_centrality.degree(mat, alpha)
    # --------------------
    # invert PS for division
    mat2 <- mat
    mat2@x <- mat@x^-1
    mat2 <- mat2 %*% mat
    # sum paths of length two
    mat <- (mat + mat2)^2
    # --------------------
    res <- Matrix::rowSums(mat)
    # scaling and inverting
    res <- ((res - base::mean(res)) / stats::sd(res)) * -1
    return(res)
}

#' Centroid Similarity
#'
#' @importFrom text2vec sim2
#'
#' @param x Input matrix
#' @param wv Matrix of word embedding vectors
#'
#' @keywords internal
#' @noRd
.doc_similarity.centroid <- function(x, wv) {
    # Find shared vocabulary
    vocab <- base::intersect(colnames(x), rownames(wv))
    ## Prepare vocab of Word Embeddings and DTM
    wv <- wv[vocab, ]
    # Remove words in the DTM without word vectors
    x <- x[, vocab]
    # DTM weight by RF
    x <- find_transformation(x, method = "norm")
    # multiple RF DTM by word vectors
    cen <- as.matrix(x %*% wv)
    res <- text2vec::sim2(cen, method = "cosine")
    return(res)
}

#' Word Mover's Distance (Similarity)
#'
#' @importFrom text2vec RWMD
#' @importFrom text2vec sim2
#'
#' @param x Input matrix
#' @param wv Matrix of word embedding vectors
#'
#' @keywords internal
#' @noRd
.doc_similarity.wmd <- function(x, wv) {
    ## Prepare vocab of Word Embeddings and DTM
    wem <- wv[intersect(rownames(wv), colnames(x)), ]
    # This is rare, but remove any NAs or RWMD won't like it
    wem <- wem[rowSums(is.na(wem)) != ncol(wem), ]
    # Remove words in the DTM without word vectors
    x <- x[, intersect(colnames(x), rownames(wem))]

    res <- text2vec::RWMD$new(x, wem)$sim2(x)
    return(res)
}
