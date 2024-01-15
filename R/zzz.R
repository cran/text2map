
# ON LOAD ----
# default threads to 2 for CRAN
# users will need to set threads
.onLoad <- function(libname, pkgname) {
  # CRAN OMP THREAD LIMIT
  Sys.setenv("OMP_THREAD_LIMIT" = 2L)
  options("text2vec.mc.cores" = 2L)
  # stop CRAN from testing "donttest"
  Sys.setenv("_R_CHECK_DONTTEST_EXAMPLES_" = FALSE)

}



#' Read word embedding models from `bin` or `vec` files
#'
#' This an internal function, in development. Word embeddings are
#' commonly in `bin` or `vec` (i.e. word2vec) format. This function will
#' read these files into the `R` environment.
#'
#' @details
#' Setting `n_vocab` will limit the number of rows that will be read in.
#' As rows of embeddings tend to be sorted by their frequencies in the training
#' corpus, this argument will return the most frequent terms in descending
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
#' @param n_vocab Integer indicating number of rows (i.e. terms) to read in
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
