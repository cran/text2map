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
#' @importFrom graphics par
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
#' @return Returns a named list object of class `CoCA`. List elements include:
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
#' # create semantic directions
#' gen <- data.frame(
#'   pole1 = c("woman"),
#'   pole2 = c("man")
#' )
#'
#' die <- data.frame(
#'   pole1 = c("alive"),
#'   pole2 = c("die")
#' )
#' 
#' hot <- data.frame(
#'   pole1 = c("hot"),
#'   pole2 = c("icebergs")
#' )
#'
#' gen_dir <- get_direction(anchors = gen, wv = ft_wv_sample)
#' die_dir <- get_direction(anchors = die, wv = ft_wv_sample)
#' hot_dir <- get_direction(anchors = hot, wv = ft_wv_sample)
#'
#' sem_dirs <- rbind(gen_dir, die_dir, hot_dir)
#'
#' classes <- CoCA(
#'   dtm = dtm,
#'   wv = ft_wv_sample,
#'   directions = sem_dirs,
#'   filter_sig = TRUE,
#'   filter_value = 0.1,
#'   zero_action = "drop"
#' )
#'
#' print(classes)
#' @export
#'
CoCA <- function(dtm, wv = NULL, directions = NULL,
                 filter_sig = TRUE, filter_value = 0.05,
                 zero_action = c("drop", "ownclass")) {

  zero_action <- match.arg(zero_action)

  if (is.null(directions)) {
    cli::cli_abort("{.arg directions} is required. Provide semantic direction vectors from {.fn get_direction}.")
  }
  if (is.null(wv)) {
    cli::cli_abort("{.arg wv} is required. Provide a word embedding matrix.")
  }

  # get CMDs for multiple semantic directions
  cmds <- CMDist(
    dtm = dtm,
    cv = directions,
    wv = wv,
    scale = TRUE
  )

  n_dirs <- ncol(cmds) - 1

  if (n_dirs < 2) {
    cli::cli_abort("{.arg directions} must provide at least 2 directions to compute document correlations; only {n_dirs} {?was/were} provided.")
  }

  if (n_dirs < 3) {
    if (isTRUE(filter_sig)) {
      cli::cli_warn("N directions must be >= 3 to calculate significance. Setting `filter_sig = FALSE`.")
    }
    filter_sig <- FALSE
  }

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
#' @return Invisibly returns NULL
#'
#' @examples
#' \donttest{
#' data(ft_wv_sample)
#' data(jfk_speech)
#' jfk_speech$sentence <- tolower(jfk_speech$sentence)
#' jfk_speech$sentence <- gsub("[[:punct:]]+", " ", jfk_speech$sentence)
#' dtm <- dtm_builder(jfk_speech, sentence, sentence_id)
#'
#' gen <- data.frame(pole1 = c("man", "new", "choose"), pole2 = c("nation", "world", "peace"))
#' space <- data.frame(pole1 = c("space", "moon", "science"), pole2 = c("nation", "world", "freedom"))
#' power <- data.frame(pole1 = c("great", "power", "new"), pole2 = c("peace", "freedom", "world"))
#'
#' gen_dir <- get_direction(anchors = gen, wv = ft_wv_sample)
#' space_dir <- get_direction(anchors = space, wv = ft_wv_sample)
#' power_dir <- get_direction(anchors = power, wv = ft_wv_sample)
#' directions <- rbind(gen_dir, space_dir, power_dir)
#'
#' classes <- CoCA(dtm, wv = ft_wv_sample, directions = directions)
#' print(classes)
#' }
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
#' @param x CoCA object returned by [CoCA()]
#' @param module Integer, which schematic class to plot. Defaults to `NULL`,
#'               but a value is effectively required: omitting it or passing
#'               `NULL` errors, prompting you to specify one. Must be a
#'               single whole number between 1 and the number of modules
#'               found in `x` (`length(x$modules)`), else errors with a
#'               message stating the valid range.
#' @param min edges with absolute weights under this value are
#'            not shown (default = 0.15)
#' @param max highest weight to scale the edge widths too (default = 1)
#' @param label.cex label size scaling factor (default = 1.2)
#' @param seed random seed for layout reproducibility (default = 123)
#' @param main title for plot (default = NULL)
#' @param ... Arguments to be passed to methods
#'
#' @return invisibly returns the igraph object
#'
#' @examples
#' \donttest{
#' data(ft_wv_sample)
#' data(jfk_speech)
#' jfk_speech$sentence <- tolower(jfk_speech$sentence)
#' jfk_speech$sentence <- gsub("[[:punct:]]+", " ", jfk_speech$sentence)
#' dtm <- dtm_builder(jfk_speech, sentence, sentence_id)
#'
#' gen <- data.frame(pole1 = c("man", "new", "choose"), pole2 = c("nation", "world", "peace"))
#' space <- data.frame(pole1 = c("space", "moon", "science"), pole2 = c("nation", "world", "freedom"))
#' power <- data.frame(pole1 = c("great", "power", "new"), pole2 = c("peace", "freedom", "world"))
#'
#' gen_dir <- get_direction(anchors = gen, wv = ft_wv_sample)
#' space_dir <- get_direction(anchors = space, wv = ft_wv_sample)
#' power_dir <- get_direction(anchors = power, wv = ft_wv_sample)
#' directions <- rbind(gen_dir, space_dir, power_dir)
#'
#' classes <- CoCA(dtm, wv = ft_wv_sample, directions = directions)
#' }
#'
#' @method plot CoCA
#'
#' @export
plot.CoCA <- function(x, module = NULL, min = .15, max = 1,
                      label.cex = 1.2, seed = 123, main = NULL, ...) {
  if (missing(module) || is.null(module)) {
    cli::cli_abort("Please specify the schematic class you'd like to plot using the {.arg module} argument.")
  }
  n_modules <- length(x$modules)
  if (!is.numeric(module) || length(module) != 1 ||
      module != as.integer(module) || module < 1 || module > n_modules) {
    cli::cli_abort("{.arg module} must be a single whole number between 1 and {n_modules} (the number of schematic classes found).")
  }

  if (isTRUE(x$modules[[module]]$degenerate)) {
    cli::cli_abort("Module #{module} is degenerate (one or more column correlations are undefined).")
  }

  cormat <- x$modules[[module]]$cormat

  # Filter small correlations
  cormat[abs(cormat) < min] <- 0

  # Build igraph object
  g <- igraph::graph_from_adjacency_matrix(
    cormat,
    mode = "undirected",
    weighted = TRUE,
    diag = FALSE
  )

  # Remove isolated vertices
  isolated <- igraph::degree(g) == 0
  if (any(isolated)) {
    g <- igraph::delete_vertices(g, which(isolated))
  }

  # If graph is empty (no edges), warn and return
  if (igraph::ecount(g) == 0) {
    cli::cli_warn("No edges remain after filtering. Try lowering the {.arg min} parameter.")
    return(invisible(NULL))
  }

  # Edge width scaled by absolute correlation
  ew <- abs(igraph::E(g)$weight)
  if (length(ew) > 1 && diff(range(ew)) > 0) {
    igraph::E(g)$width <- 1 + (ew - min(ew)) / diff(range(ew)) * 9
  } else {
    igraph::E(g)$width <- 5
  }

  # Edge color and line type
  igraph::E(g)$color <- "darkgray"
  igraph::E(g)$lty <- "solid"
  igraph::E(g)$lty[igraph::E(g)$weight < 0] <- "dashed"
  igraph::E(g)$color[igraph::E(g)$weight < 0] <- "lightgray"

  # Vertex attributes
  igraph::V(g)$shape <- "circle"
  igraph::V(g)$label.cex <- label.cex
  igraph::V(g)$label.color <- "black"
  igraph::V(g)$frame.color <- "black"
  igraph::V(g)$color <- "white"

  # Scale vertex size to fit labels
  label_lens <- nchar(igraph::V(g)$name)
  igraph::V(g)$size <- 22 + label_lens * 3.5

  # Layout: Fruchterman-Reingold requires positive weights
  g_layout <- igraph::delete_edges(g, which(is.na(igraph::E(g)$weight)))
  igraph::E(g_layout)$weight <- abs(igraph::E(g_layout)$weight)
  if (!is.null(seed)) {
    restore_rng <- .rng_guard() # nolint: object_usage_linter. defined in utils-perm.R; lintr's per-file usage check doesn't see cross-file package-internal helpers
    on.exit(restore_rng(), add = TRUE)
    set.seed(seed)
  }
  lay <- igraph::layout_with_fr(g_layout, niter = 500)

  # Plot
  title <- if (is.null(main)) paste("Class #", module) else main
  oldmar <- par("mar")
  par(mar = c(2, 2, 3, 2))
  on.exit(par(mar = oldmar))
  plot(g, layout = lay, main = title)

  invisible(g)
}

# INTERNAL FUNCTIONS -----------------------------------------------------------

#' get_cor_class
#'
#' Modified from Boutyline's cca function in the `corclass` package
#' Divides matrix into schematic classes based on row correlations.
#'
#'
#' @param cmds CMD scores on multiple semantic directions for a set of documents
#'             as output from [CMDist()]
#' @param filter_sig logical (default = TRUE), sets 'insignificant'
#'                   ties to 0 to decrease noise and increase stability.
#'                   N directions must be > 2.
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

  if (isTRUE(filter_sig)) {
    cormat <- .filter.insignif(cormat,
      n_dirs = ncol(cmds),
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

  # Floating point imprecision may make 0-variance rows
  # appear to have variance slightly higher than 0.
  zeros <- which(apply(cmds, 1, stats::var) <= .Machine$double.xmin)

  if (zero_action[1] == "drop" && (length(zeros) > 0)) {
    cmds <- cmds[-zeros, ]
  }

  rv <- abs(stats::cor(t(cmds)))

  attributes(rv)$zeros <- zeros
  attributes(rv)$zero_action <- zero_action[1]
  attributes(rv)$cmds <- cmds

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
#' @param n_dirs number of directions correlated
#' @param filter_value significance level below which to filter
#'
#' @noRd
.filter.insignif <- function(cormat, n_dirs, filter_value = 0.05) {
  suppressWarnings( tvalues <- cormat * sqrt((n_dirs - 2) / (1 - cormat^2)) )

  if ( any(is.infinite(tvalues)) ) {
    tvalues[is.infinite(tvalues)] <- .Machine$double.xmax # a very big number
  }
  cutoff <- abs( stats::qt(filter_value / 2, n_dirs - 2) )

  isolates_pre <- sum(apply(cormat, 1, sum) == 0)
  cormat[tvalues < cutoff] <- 0
  isolates_post <- sum(apply(cormat, 1, sum) == 0)

  if (isolates_post > isolates_pre) {
    cli::cli_warn(paste(
      "Significance filtering left", isolates_post - isolates_pre,
      "rows with no non-zero ties.",
      "The CCA result will contain at least one small degenerate class."
    ))
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

  for (i in seq_along(ids)) {
    curmod <- list()
    class(curmod) <- "cca.module"
    curmod$cmds <- cmds[membership == ids[i], ]
    curmod$cormat <- stats::cor(curmod$cmds)
    curmod$degenerate <- anyNA(curmod$cormat)
    modules[[i]] <- curmod
  }

  return(modules)
}
