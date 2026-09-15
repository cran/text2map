## INTERNAL FUNCTION ## 

#' Evaluate anchor sets with relco -- called by test_anchors()
#'
#' Evaluates how well an anchor set defines a semantic centroid, semantic 
#' direction, or a compound concept using the technique defined by
#' Taylor et al. (2025)
#'
#' @details
#' Evaluates anchor sets across multiple runs, each time sampling a focal
#' anchor term and a subset of non-anchor (noise) terms to construct a
#' pseudo-DTM. The reliability coefficient is the slope of a regression
#' of cosine distances on inverse rank across runs.
#'
#' @references
#' Taylor, Stoltz, and Kozlowski (2025)
#' "The Reliability Coefficient: Quantifying Measurement Error in
#' Conceptual Measurement with Word Embeddings"
#' \doi{https://osf.io/preprints/socarxiv/sc2ub_v3}
#'
#' @importFrom Matrix Matrix
#' @importFrom tibble rownames_to_column
#' @importFrom dplyr mutate across
#' @importFrom tidyselect where
#' @importFrom tibble as_tibble
#' @importFrom stats t.test na.omit coef setNames
#'
#' @param anchors Anchor terms
#' @param wv Matrix of word embedding vectors (a.k.a embedding model)
#'           with rows as terms.
#' @param non_anchors Terms that are not anchors (random, unrelated, 
#'                    or distinctive terms)
#' @param type Type of relation, `direction`, `centroid`, or `compound`.
#' @param dir_method Method to calculate direction.
#' @param conf Confidence interval
#' @param n_runs Number of runs
#' @param null Null hypothesis (default is 0).
#' @param alpha Term selection weighting (default is 0.5)
#' @param seed Set sampling seed
#' @param order_non_anchors Logical (default `FALSE`). If TRUE, the subset
#'                          of non-anchor terms is fixed across runs instead of
#'                          randomly sampled.
#' @return list or data.frame
#' @noRd
test_anchors.relco <- function(anchors,
                  non_anchors, 
                  wv, 
                  type = c("direction", "centroid", "compound"), 
                  dir_method = c("paired", "pooled", "L2", "PCA"),
                  conf = 0.95,
                  n_runs = 100, 
                  null = 0, 
                  alpha =  0.5,
                  seed = NULL,
                  order_non_anchors = FALSE) {
  
  type <- match.arg(type)
  dir_method <- match.arg(dir_method)
  
  if (n_runs < 5) {
    cli::cli_warn("`n_runs` must be 5 or greater; coercing `n_runs` from {n_runs} to 5")
    n_runs <- as.integer(5)
  }
  
  ## ----------------------------------------------------------------------------
  # Initialize 
  ## ----------------------------------------------------------------------------
  
  if(!is.null(seed)) {
    restore_rng <- .rng_guard() # nolint: object_usage_linter. defined in utils-perm.R; lintr's per-file usage check doesn't see cross-file package-internal helpers
    on.exit(restore_rng(), add = TRUE)
    set.seed(seed)
  }
  
  ls_relco <- list()
  ls_relco[["run_scores"]]    <- data.frame(matrix(ncol = n_runs, nrow = 1))
  ls_relco[["run_intercept"]] <- data.frame(matrix(ncol = n_runs, nrow = 1))
  ls_relco[["raw_scores"]]    <- list()
  ls_relco[["term_contrib"]]  <- list()
  ls_relco[["anchor_terms"]]  <- list()
  ls_relco[["non_anchor_terms"]]  <- list()
  ls_relco[["term_conf"]]     <- list()
  ls_relco[["term_runs"]]     <- list()
  
  ## ----------------------------------------------------------------------------
  # Setup Terms
  ## ----------------------------------------------------------------------------

  # deal with data.frames
  if (inherits(anchors, "data.frame")) {
    
    if(type == "direction") {
      if(ncol(anchors)  > 2) cli::cli_abort("{.arg anchors} data.frame must have {.code ncol() == 2}.")
    }
    if(type %in% c("centroid", "compound")) {
      if(ncol(anchors) != 1) cli::cli_abort("{.arg anchors} data.frame must have {.code ncol() == 1}.")
    }

    # convert data.frame to list
    anchors <- as.list(anchors)
  }
  
  if (type %in% c("centroid", "compound")) {
    if (inherits(anchors, "list")) {
      if (length(anchors) > 1) {
        cli::cli_abort("For centroid/compound, {.arg anchors} must be a character vector, a list of one character vector, or a 1-column data.frame.")
      }
    }
    anchors <- unlist(anchors, use.names = FALSE)
  }
  
  ##
  if (!inherits(anchors, "character") && !inherits(anchors, "list")) {
    cli::cli_abort("{.arg anchors} must be either a character vector or a list of character vectors.")
  }
  if (!inherits(non_anchors, "character")) {
    cli::cli_abort("{.arg non_anchors} must be a character vector.")
  }

  if (type %in% c("centroid", "compound") && length(anchors) < 3) {
    cli::cli_abort("{.arg anchors} must have at least 3 terms.")
  }
  if (type == "direction" && any(lengths(anchors) < 3)) {
    cli::cli_abort("Each pole of {.arg anchors} must have at least 3 terms.")
  }

  ##
  if (!identical(intersect(unlist(anchors), non_anchors), character(0))) {
    cli::cli_abort("{.arg non_anchors} must not share any terms with {.arg anchors}.")
  }
  if (any(duplicated(unlist(anchors)))) {
    cli::cli_abort("{.arg anchors} must not have duplicated terms.")
  }
  if (any(duplicated(non_anchors))) {
    cli::cli_abort("{.arg non_anchors} must not have duplicated terms.")
  }
  
  ## ----------------------------------------------------------------------------
  if (type == "compound" || type == "centroid") {
    
    ## define constants
    mid_anchors <- length(anchors) * .5
    n_cols <- floor(mid_anchors) - 1 # get n cols from anchors (1 for focal term)
    
    if (length(non_anchors) < floor(mid_anchors)) {
      cli::cli_abort("{.arg non_anchors} must have a length >= half the {.arg anchors} length.")
    }
    
    # random NOISE terms portion
    if (order_non_anchors) {
      .random_terms <- stats::na.omit(non_anchors[seq_len(floor(mid_anchors))])
    } else {
      .random_terms <- sample(non_anchors, replace = FALSE, size = floor(mid_anchors))
    }
    
    .anchor_list <- anchors
    # initialize probability of being selected
    weights <- rep(1, length(.anchor_list))
    # subset WVs
    wv <- wv[rownames(wv) %in% c(.anchor_list, .random_terms), ]
  }
  
  ## ----------------------------------------------------------------------------
  if (type == "direction") {

    ## define constants
    mid_anchors <- lengths(anchors)[which.min(lengths(anchors))] * .5
    n_cols <- floor(mid_anchors) - 1 # get n cols from anchors (1 for focal term)
    
    # random NOISE terms portion
    if (order_non_anchors) {
      .random_terms <- stats::na.omit(non_anchors[seq_len(floor(mid_anchors))])
    } else {
      .random_terms <- sample(non_anchors, replace = FALSE, size = floor(mid_anchors))
    }
    
    .anchor_list1 <- anchors[[1]]
    .anchor_list2 <- anchors[[2]]
    # initialize probability of being selected
    weights1 <- rep(1, length(.anchor_list1))
    weights2 <- rep(1, length(.anchor_list2))
    # subset WVs
    wv <- wv[rownames(wv) %in% c(.anchor_list1, .anchor_list2, .random_terms), ]
  }
  
  ## ----------------------------------------------------------------------------
  # create initial simulated DTM
  # for centroids: dim(k+1, k+k) and for directions: dim(k+k+1, k+k+k)
  ## ----------------------------------------------------------------------------
  .anchors_dtm <- Matrix::Matrix(FALSE, nrow = (floor(mid_anchors) + 1), 
                                 ncol = (floor(mid_anchors) * 2), sparse = TRUE)
  .anchors_dtm[] <- cbind(upper.tri(.anchors_dtm[ , seq_len(floor(mid_anchors))], diag = TRUE),
                          lower.tri(.anchors_dtm[ , (floor(mid_anchors) + 1):ncol(.anchors_dtm)]))
  
  if (type == "direction") {
    .extra_dtm <- Matrix::Matrix(FALSE, 
                                 nrow = (floor(mid_anchors) + 1),
                                 ncol = (floor(mid_anchors) + 1), sparse = TRUE)
    .anchors_dtm <- rbind(cbind(.anchors_dtm, .extra_dtm[, -ncol(.extra_dtm)]), 
                          cbind(.extra_dtm, .anchors_dtm[, -ncol(.anchors_dtm)]))
    .anchors_dtm <- .anchors_dtm[-nrow(.anchors_dtm),]
  }
  
  rownames(.anchors_dtm) <- seq_len(nrow(.anchors_dtm))
  
  ## ----------------------------------------------------------------------------
  #
  ## ----------------------------------------------------------------------------
  
  # how many runs
  for (rdx in seq_len(n_runs)) {
    
    # re-order non_anchors
    temp_non_anchors <- sample(.random_terms, replace = FALSE)
    ls_relco[["non_anchor_terms"]][[rdx]] <- temp_non_anchors
    
    if (type == "compound" || type == "centroid") {
      
      focal <- sample(.anchor_list, size = 1, prob = weights)
      temp_cols <- sample(.anchor_list[.anchor_list != focal], size = n_cols, replace = FALSE)
      temp_anchors <- sample(.anchor_list[!.anchor_list %in% c(focal, temp_cols)])
      
      dimnames(.anchors_dtm)[[2]] <- c(focal, temp_cols, temp_non_anchors)
      # down weight focal term for next run
      weights[which(.anchor_list %in% focal)] <- weights[which(.anchor_list %in% focal)] * alpha
      ls_relco[["anchor_terms"]][[rdx]] <- temp_anchors
    }
    
    if (type == "direction") {
      # pole1
      focal1 <- sample(.anchor_list1, size = 1, prob = weights1)
      temp_cols1 <- sample(.anchor_list1[.anchor_list1 != focal1], size = n_cols, replace = FALSE)
      temp_anchors1 <- sample(.anchor_list1[!.anchor_list1 %in% c(focal1, temp_cols1)])
      # pole2
      focal2 <- sample(.anchor_list2, size = 1, prob = weights2)
      temp_cols2 <- sample(.anchor_list2[.anchor_list2 != focal2], size = n_cols, replace = FALSE)
      temp_anchors2 <- sample(.anchor_list2[!.anchor_list2 %in% c(focal2, temp_cols2)])
      
      dimnames(.anchors_dtm)[[2]] <- c(focal1, temp_cols1, temp_non_anchors, focal2, temp_cols2)
      # down weight focal term for next run
      weights1[which(.anchor_list1 %in% focal1)] <- weights1[which(.anchor_list1 %in% focal1)] * alpha
      weights2[which(.anchor_list2 %in% focal2)] <- weights2[which(.anchor_list2 %in% focal2)] * alpha
      #
      ls_relco[["anchor_terms"]][["pole1"]][[rdx]] <- temp_anchors1
      ls_relco[["anchor_terms"]][["pole2"]][[rdx]] <- temp_anchors2
    }
    
    ## ----------------------------------------------------------------------------
    # Run CMD on pseudo DTMs
    ## ----------------------------------------------------------------------------
    
    if (type == "compound") {
      .compound <- paste(temp_anchors, collapse = " ")
      run_cmd <- CMDist(dtm = .anchors_dtm, cw = .compound, wv = wv, scale = FALSE)
    }
    
    if (type == "centroid") {
      .centroid <- get_centroid(anchors = temp_anchors, wv = wv)
      run_cmd <- CMDist(dtm = .anchors_dtm, cv = .centroid, wv = wv, scale = FALSE)
    }
    
    if (type == "direction") {
      .direction <- get_direction(anchors = list(temp_anchors1, temp_anchors2),
                                            method = dir_method, wv = wv)
      run_cmd <- CMDist(dtm = .anchors_dtm, cv = .direction, wv = wv, scale = FALSE)
    }
    
    ## ----------------------------------------------------------------------------
    # Get term-level contributions
    ## ----------------------------------------------------------------------------
    
    ls_relco[["raw_scores"]][[rdx]] <- run_cmd[, 2]
    run_cmd$inverse_rank <- rev(as.numeric(run_cmd$doc_id))/max(rev(as.numeric(run_cmd$doc_id)))
    mod_lm <- coef(stats::lm(run_cmd[,2] ~ inverse_rank, data = run_cmd))
    
    ls_relco[["run_scores"]][1,rdx]    <- mod_lm[2]
    ls_relco[["run_intercept"]][1,rdx] <- mod_lm[1]
    
    zinv_rank <- scale(run_cmd$inverse_rank)
    temp_term <- scale(run_cmd[,2]) * (zinv_rank/sum(zinv_rank^2) * (sd(run_cmd[ ,2]) / sd(run_cmd$inverse_rank)))
    
    ## ----------------------------------------------------------------------------
    if (type == "compound" || type == "centroid") {
      ls_relco[["term_contrib"]][[rdx]] <- data.frame(diff = temp_term[1] - temp_term[2], 
                                                      term = focal)
    }
    
    if (type == "direction") {
      ls_relco[["term_contrib"]][[rdx]] <- data.frame(
        diff = c(temp_term[1] - temp_term[2], 
                 temp_term[floor(mid_anchors) + 1] - temp_term[floor(mid_anchors) + 2]),
        term = c(focal1, focal2)
      )
    }
  }
  
  ## ----------------------------------------------------------------------------
  ## ----------------------------------------------------------------------------
  
  df_ctrb <- do.call(rbind, ls_relco[["term_contrib"]])
  
  ## ----------------------------------------------------------------------------    
  if (type == "compound" || type == "centroid") {
    
    for (i_term in .anchor_list) {
      rowid <- df_ctrb$term == i_term
      ls_relco[["term_conf"]][[i_term]] <- .conf_int(df_ctrb[rowid, ]$diff, ci = conf)
      ls_relco[["term_runs"]][[i_term]] <- df_ctrb[rowid, ]$diff
    }
  }
  
  ## ----------------------------------------------------------------------------
  if (type == "direction") {
    
    for (i_term1 in .anchor_list1) {
      rowid <- df_ctrb$term == i_term1
      ls_relco[["term_conf"]]$pole1[[i_term1]] <- .conf_int(df_ctrb[rowid, ]$diff, ci = conf)
      ls_relco[["term_runs"]]$pole1[[i_term1]] <- df_ctrb[rowid, ]$diff  
    }
    
    for (i_term2 in .anchor_list2) {
      rowid <- df_ctrb$term == i_term2
      ls_relco[["term_conf"]]$pole2[[i_term2]] <- .conf_int(df_ctrb[rowid, ]$diff, ci = conf)
      ls_relco[["term_runs"]]$pole2[[i_term2]] <- df_ctrb[rowid, ]$diff
    }
  }
  
  ## ----------------------------------------------------------------------------
  # t-test
  mod_ttest <- stats::t.test(ls_relco[["run_scores"]], mu = null, alternative = "greater") # t-test
  mod_ttest$alt_hyp <- paste0("True global reliability coefficient > ", null)
  mod_ttest$null <- null
  
  ## ----------------------------------------------------------------------------
  # create word-level contribution data.frame
  if (type == "compound" || type == "centroid") {
    df_relco <- as.data.frame(do.call(rbind, as.list(ls_relco$term_conf) )) |> 
      tibble::rownames_to_column(var = "term") |>
      dplyr::mutate(dplyr::across(where(is.numeric), ~round(., 7))) |>
      tibble::as_tibble()
    
    attr(df_relco, "top_terms") <- df_relco[order(-df_relco$mean), ]$term[seq_along(.anchor_list)]
  }
  
  if (type == "direction") {
    
    df_relco_pole1 <- as.data.frame(do.call(rbind, as.list(ls_relco$term_conf$pole1))) |> 
      tibble::rownames_to_column(var = "term") |>
      dplyr::mutate(dplyr::across(where(is.numeric), ~round(., 7))) |>
      tibble::as_tibble() |> 
      dplyr::mutate(pole = "pole1")
    
    df_relco_pole2 <- as.data.frame(do.call(rbind, as.list(ls_relco$term_conf$pole2) )) |> 
      tibble::rownames_to_column(var = "term") |>
      dplyr::mutate(dplyr::across(where(is.numeric), ~round(., 7))) |>
      tibble::as_tibble() |> 
      dplyr::mutate(pole = "pole2")
    
    df_relco <- rbind(df_relco_pole1, df_relco_pole2)
    
    top_terms <- list(pole1 = df_relco_pole1[order(-df_relco_pole1$mean), ]$term[seq_along(.anchor_list1)],
                      pole2 = df_relco_pole2[order( df_relco_pole2$mean), ]$term[seq_along(.anchor_list2)])
    
    attr(df_relco, "top_terms") <- top_terms    
  }
  
  # add attributes to data.frame
  attr(df_relco, "relation_type")    <- type
  attr(df_relco, "confidence_level") <- conf
  attr(df_relco, "null")             <- null
  attr(df_relco, "alpha")            <- alpha
  attr(df_relco, "seed")             <- seed
  attr(df_relco, "n_runs")           <- n_runs
  attr(df_relco, "total_score")      <- mean(as.numeric(ls_relco[["run_scores"]]), na.rm = TRUE)
  attr(df_relco, "total_intercept")  <- mean(as.numeric(ls_relco[["run_intercept"]]), na.rm = TRUE)
  attr(df_relco, "t_test")           <- mod_ttest
  attr(df_relco, "confidence_int")   <- .conf_int(as.numeric(ls_relco[["run_scores"]]), ci = conf)
  ##
  attr(df_relco, "term_runs")        <- ls_relco$term_runs
  attr(df_relco, "run_scores")       <- ls_relco$run_scores
  attr(df_relco, "raw_scores")       <- ls_relco$raw_scores
  attr(df_relco, "intercepts")       <- ls_relco[["run_intercept"]]
  attr(df_relco, "anchor_terms")     <- ls_relco[["anchor_terms"]]
  attr(df_relco, "non_anchor_terms") <- ls_relco[["non_anchor_terms"]]
  ##
  class(df_relco) <- c("relco", "tbl", "data.frame")
  
  return(df_relco)
}


## ----------------------------------------------------------------------------
# Printing for relco
## ----------------------------------------------------------------------------

#' @importFrom cli style_italic
#' @importFrom pillar tbl_sum
#'
#' @param x relco table
#'
#' @keywords internal
#' @noRd
#' @exportS3Method pillar::tbl_sum
tbl_sum.relco <- function(x,...) {
  
  relation <- attr(x, "relation_type")
  
  if(relation == "direction") {
    n_top_p1 <- min(5, length(attr(x, "top_terms")$pole1))
    n_top_p2 <- min(5, length(attr(x, "top_terms")$pole2))
    hdr <- c(
      "Relation Type" = cli::style_italic(relation),
      "Global Reliability Coefficient" = cli::style_italic(round(attr(x, "total_score"), 4)),
      "Confidence Interval (two-tailed)" = cli::style_italic(paste0(round(attr(x, "confidence_int")["lower_ci"], 4), " to ", round(attr(x, "confidence_int")["upper_ci"], 4), " at ", attr(x, "confidence_level")*100, "%")),
      "t-test" = cli::style_italic(paste0("t = ", round(attr(x, "t_test")$statistic, 4), ", df = ", attr(x, "t_test")$parameter[["df"]], ", p-value = ", round(attr(x, "t_test")$p.value, 4))),
      "Alternative Hypothesis" = cli::style_italic(attr(x, "t_test")$alt_hyp),
      "Term-Level Contributions" = ""
    )
    hdr <- c(
      hdr[1:2],
      setNames(cli::style_italic(paste(head(attr(x, "top_terms")$pole1, 5), collapse = ", ")),
               paste0(n_top_p1, " Highest Contributors (Pole 1)")),
      setNames(cli::style_italic(paste(head(attr(x, "top_terms")$pole2, 5), collapse = ", ")),
               paste0(n_top_p2, " Highest Contributors (Pole 2)")),
      hdr[3:6]
    )
    return(hdr)
    
  } else {
    n_top <- min(5, length(attr(x, "top_terms")))
    hdr <- c(
      "Relation Type" = cli::style_italic(relation),
      "Global Reliability Coefficient" = cli::style_italic(round(attr(x, "total_score"), 4)),
      setNames(cli::style_italic(paste(head(attr(x, "top_terms"), 5), collapse = ", ")),
               paste0(n_top, " Highest Contributors")),
      "Confidence Interval (two-tailed)" = cli::style_italic(paste0(round(attr(x, "confidence_int")["lower_ci"], 4), " to ", round(attr(x, "confidence_int")["upper_ci"], 4), " at ", attr(x, "confidence_level")*100, "%")),
      "t-test" = cli::style_italic(paste0("t = ", round(attr(x, "t_test")$statistic, 4), ", df = ", attr(x, "t_test")$parameter[["df"]], ", p-value = ", round(attr(x, "t_test")$p.value, 4))),
      "Alternative Hypothesis" = cli::style_italic(attr(x, "t_test")$alt_hyp),
      "Term-Level Contributions" = ""
    )
    return(hdr)
  }  
}

#' @importFrom pillar tbl_format_footer
#' @importFrom pillar style_subtle
#' @importFrom cli symbol
#'
#' @param x relco table
#' @param setup tibble setup
#'
#' @keywords internal
#' @noRd
#' @exportS3Method pillar::tbl_format_footer
tbl_format_footer.relco <- function(x, setup,...) {
  n_extra_rows <- setup$rows_total - 10L
  if(n_extra_rows > 0) {

  extra_footer1 <- pillar::style_subtle(paste0("# ", 
              cli::symbol$info, " ", n_extra_rows, " more terms"))
  extra_footer2 <- pillar::style_subtle(paste0("# ", 
              cli::symbol$info, " Use `print(n = ...)` to see more terms"))
  return(c(extra_footer1, extra_footer2))
    
  } else {

  return(c(""))
    
  }
}
