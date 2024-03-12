#' Performs Concept Class Analysis (CoCA)
#'
#' CoCA outputs schematic classes derived from documents' engagement
#' with multiple bi-polar concepts (in a Likert-style fashion).
#' The function requires a (1) DTM of a corpus which can be obtained using any
#' popular text analysis package, or from the [dtm_builder()] function, and (2)
#' semantic directions as output from the [get_direction()].
#' [CMDist()] works under the hood. Code modified from the `corclass` package.
#'
#'
#'
#' @references
#' Taylor, Marshall A., and Dustin S. Stoltz.
#' (2020) 'Concept Class Analysis: A Method for Identifying Cultural
#' Schemas in Texts.' \emph{Sociological Science} 7:544-569.
#' \doi{10.15195/v7.a23}.\cr
#' Boutyline, Andrei. 'Improving the measurement of shared cultural
#' schemas with correlational class analysis: Theory and method.'
#' Sociological Science 4.15 (2017): 353-393.
#' \doi{10.15195/v4.a15}\cr
#'
#' @name CoCA
#' @author Dustin Stoltz and Marshall Taylor
#'
#' @importFrom igraph leading.eigenvector.community graph_from_adjacency_matrix
#'
#' @param dtm Document-term matrix with words as columns. Works with DTMs
#'            produced by any popular text analysis package, or you can use the
#'            `dtm_builder()` function.
#' @param wv Matrix of word embedding vectors (a.k.a embedding model)
#'           with rows as words.
#' @param directions direction vectors output from get_direction()
#' @param filter_sig logical (default = TRUE), sets 'insignificant'
#'                   ties to 0 to decrease noise and increase stability
#' @param filter_value Minimum significance cutoff.
#'                     Absolute row correlations below
#'                     this value will be set to 0
#' @param zero_action If 'drop', CCA drops rows with
#'                    0 variance from the analyses (default).
#'                    If 'ownclass', the correlations between 0-variance
#'                    rows and all other rows is set 0, and the correlations
#'                    between all pairs of 0-var rows are set to 1
#'
#' @returns Returns a named list object of class `CoCA`. List elements include:
#' \itemize{
#' \item membership: document memberships
#' \item modules: schematic classes
#' \item cormat: correlation matrix
#' }
#'
#' @seealso [CMDist], [get_direction]
#'
#' @examples
#'
#' #' # load example word embeddings
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
#' # create semantic directions
#' gen <- data.frame(
#'   add = c("woman"),
#'   subtract = c("man")
#' )
#'
#' die <- data.frame(
#'   add = c("alive"),
#'   subtract = c("die")
#' )
#'
#' gen_dir <- get_direction(anchors = gen, wv = ft_wv_sample)
#' die_dir <- get_direction(anchors = die, wv = ft_wv_sample)
#'
#' sem_dirs <- rbind(gen_dir, die_dir)
#'
#' classes <- CoCA(
#'   dtm = dtm,
#'   wv = ft_wv_sample,
#'   directions = sem_dirs,
#'   filter_sig = TRUE,
#'   filter_value = 0.05,
#'   zero_action = "drop"
#' )
#'
#' print(classes)
#' @export
#'
CoCA <- function(dtm, wv = NULL, directions = NULL,
                 filter_sig = TRUE, filter_value = 0.05,
                 zero_action = c("drop", "ownclass")) {

  # get CMDs for multiple semantic directions
  cmds <- CMDist(
    dtm = dtm,
    cv = directions,
    wv = wv,
    scale = TRUE
  )

  # get correlational classes
  classes <- .get_cor_class(
    cmds[, -1],
    filter_sig,
    filter_value,
    zero_action
  )

  class(classes) <- "CoCA"

  return(classes)
}

#' Create lowercase alias for CoCA
#' @rdname CoCA
#' @export
coca <- CoCA



## Modified from Boutyline's print.cca method in the corclass package
#' Prints CoCA class information
#'
#' @param x CoCA object returned by `CoCA()`
#' @param ... Arguments to be passed to methods
#' @method print CoCA
#'
#' @return prints a message indicating the classes and sizes
#'
#' @export
print.CoCA <- function(x, ...) {
  cat(
    "CoCA found", length(unique(x$membership)),
    "schematic classes in the corpus. Sizes:",
    table(x$membership)
  )

  degen <- vapply(x$modules, function(m1) {
    m1$degenerate
  }, logical(1))

  if (any(degen)) {
    cat("NOTE: result contains ", sum(degen),
      " degenerate class(es): ",
      paste("#", which(degen),
        sep = "",
        collapse = " "
      ), ".\n",
      sep = ""
    )
  }
}

## Modified from Boutyline's plot.cca function in the corclass package
#' Plot CoCA
#'
#' @importFrom qgraph qgraph
#'
#' @param x CoCA object returned by [CoCA()]
#' @param module index for which module to plot (default = NULL)
#' @param cutoff minimum absolute value of correlations to plot
#' @param repulse repulse radius in the spring layout
#' @param min edges with absolute weights under this value are
#'            not shown (default = 0.15)
#' @param max highest weight to scale the edge widths too (default = 1)
#' @param main title for plot (default = NULL)
#' @param ... Arguments to be passed to methods
#'
#' @return returns `qgraph` object
#'
#' @method plot CoCA
#'
#' @export
plot.CoCA <- function(x, module = NULL, cutoff = 0.05, repulse = 1.86,
                      min = .15, max = 1, main = NULL, ...) {
  if (missing(module)) {
    stop("Please specify the schematic class you'd
             like to plot using the 'module = ' argument.")
  }

  if (x$modules[[module]]$degenerate == TRUE) {
    stop(paste("Module #", module,
      " is degenerate (one or more column
                    correlations are undefined).",
      sep = ""
    ))
  }

  if (is.null(main)) {
    samp <- nrow(x$modules[[module]]$cmds)

    qgraph::qgraph(x$modules[[module]]$cormat,
      graph = "cor",
      minimum = min, maximum = max, threshold = "sig",
      sampleSize = samp,
      alpha = cutoff, layout = "spring", repulsion = repulse,
      label.cex = 2,
      posCol = "black", negCol = "black", negDashed = TRUE,
      borders = TRUE, shape = "circle", label.prop = 0.75,
      curveAll = FALSE, edge.labels = FALSE, edge.label.cex = 0.45, esize = 8,
      title = paste("Class #", module),
      labels = rownames(x$modules[[module]]$cormat)
    )
  } else {
    samp <- nrow(x$modules[[module]]$cmds)

    qgraph::qgraph(x$modules[[module]]$cormat,
      graph = "cor",
      minimum = min, maximum = max, threshold = "sig",
      sampleSize = samp,
      alpha = cutoff, layout = "spring", repulsion = repulse,
      label.cex = 2,
      posCol = "black", negCol = "black", negDashed = TRUE,
      borders = TRUE, shape = "circle", label.prop = 0.75,
      curveAll = FALSE, edge.labels = FALSE, edge.label.cex = 0.45, esize = 8,
      title = main,
      labels = rownames(x$modules[[module]]$cormat)
    )
  }
}

# INTERNAL FUNCTIONS -----------------------------------------------------------

#' get_cor_class
#'
#' Modified from Boutyline's cca function in the `corclass` package
#' Divides matrix into schematic classes based on row correlations.
#'
#' @importFrom utils capture.output
#' @importFrom igraph graph_from_adjacency_matrix
#' @importFrom igraph cluster_leading_eigen
#'
#' @param cmds CMD scores on multiple semantic directions for a set of documents
#'             as output from [CMDist()]
#' @param filter_sig logical (default = TRUE), sets 'insignificant'
#'                   ties to 0 to decrease noise and increase stability
#' @param filter_value Minimum significance cutoff.
#'                     Absolute row correlations below
#'                     this value will be set to 0
#' @param zero_action If "drop", CCA drops rows with
#'                    0 variance from the analyses (default).
#'                    If "ownclass", the correlations between
#'                    0-variance rows and all other rows is set 0, and the
#'                    correlations between all pairs of 0-var rows are set to 1
#' @noRd
.get_cor_class <- function(cmds,
                           filter_sig = TRUE,
                           filter_value = 0.01,
                           zero_action = c("drop", "ownclass")) {
  cormat <- .make_cormat(cmds, zero_action)

  if (filter_sig == TRUE) {
    cormat <- .filter.insignif(cormat,
      ncol(cmds),
      filter_value = filter_value
    )
  }

  graph <- igraph::graph_from_adjacency_matrix(cormat,
    mode = "undirected",
    weighted = TRUE,
    diag = FALSE
  )

  comm <- igraph::cluster_leading_eigen(graph)

  modules <- .separate(attr(cormat, "cmds"), membership = comm$membership)

  val <- list(
    membership = comm$membership,
    modules = modules,
    cormat = cormat
  )
  class(val) <- "cca"

  paste(utils::capture.output(print(val)), collapse = "\n")

  return(invisible(val))
}


#' .make_cormat
#'
#' Modified from Boutyline's cca function in the `corclass` package
#' Make a correlation matrix from data frame
#'
#' @param cmds CMD scores on multiple semantic directions for a set of documents
#'             as output from [CMDist()]
#' @param zero_action zero_action If "drop", CCA drops rows with
#'                    0 variance from the analyses (default).
#'                    If "ownclass", the correlations between
#'                    0-variance rows and all other rows is set 0, and the
#'                    correlations between all pairs of 0-var rows are set to 1
#' @noRd
.make_cormat <- function(cmds, zero_action) {
  cmds2 <- cmds
  # Floating point imprecision may make 0-variance rows
  # appear to have variance slightly higher than 0.
  zeros <- which(apply(cmds2, 1, stats::var) <= 0.000000001)

  if (zero_action[1] == "drop" && (length(zeros) > 0)) {
    cmds2 <- cmds2[-zeros, ]
  }

  rv <- abs(stats::cor(t(cmds2)))

  attributes(rv)$zeros <- zeros
  attributes(rv)$zero_action <- zero_action[1]
  attributes(rv)$cmds <- cmds2

  if ((zero_action[1] == "ownclass") && length(zeros) > 0) {
    rv[zeros, ] <- 0
    rv[, zeros] <- 0
    rv[zeros, zeros] <- 1
  }

  diag(rv) <- 0

  return(rv)
}


#' .filter.insignif
#'
#' Filter significance at p <= filter_value (two-tailed).
#'
#' @importFrom stats cor qt var
#'
#' @param cormat correlation matrix
#' @param n_vars number of variables correlated
#' @param filter_value significance level below which to filter
#'
#' @noRd
.filter.insignif <- function(cormat, n_vars, filter_value = 0.05) {
  suppressWarnings(tvalues <- cormat * sqrt((n_vars - 2) / (1 - cormat^2)))

  if (any(is.infinite(tvalues))) {
    tvalues[is.infinite(tvalues)] <- 99999 # a very big number
  }
  cutoff <- abs(stats::qt(filter_value / 2, n_vars))

  isolates_pre <- sum(apply(cormat, 1, sum) == 0)
  cormat[tvalues < cutoff] <- 0
  isolates_post <- sum(apply(cormat, 1, sum) == 0)

  if (isolates_post > isolates_pre) {
    warn1 <- paste(
      "Significance filtering left", isolates_post - isolates_pre,
      "rows with no non-zero ties. The CCA result will contain at
         least one small degenerate class."
    )
    warning(warn1)
  }

  return(cormat)
}

#' .separate
#'
#' Separate a data frame and cormat according to the membership vector
#'
#' @param cmds CMD scores on multiple semantic directions for a set of documents
#'             as output from [CMDist()]
#' @param membership designates which class
#'
#' @noRd
.separate <- function(cmds, membership) {
  ids <- sort(unique(membership))
  modules <- list()

  for (i in seq_len(length(ids))) {
    curmod <- list()
    class(curmod) <- "cca.module"
    curmod$cmds <- cmds[membership == ids[i], ]
    curmod$cormat <- stats::cor(curmod$cmds)
    curmod$degenerate <- any(is.na(curmod$cormat))
    modules[[i]] <- curmod
  }

  return(modules)
}
