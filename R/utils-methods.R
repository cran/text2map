#' Import Matrix
#'
#' Get methods and classes from Matrix
#'
#' @import methods
#' @import Matrix
#' @importMethodsFrom Matrix colSums
#' @importMethodsFrom Matrix t
#' @importMethodsFrom Matrix colMeans
#' @importMethodsFrom Matrix rowSums
#' @importMethodsFrom Matrix rowMeans
#' @importMethodsFrom Matrix %*%
#' @importMethodsFrom Matrix as.matrix
#' @importMethodsFrom Matrix as.array
#' @importMethodsFrom Matrix as.logical
#' @importMethodsFrom Matrix as.numeric
#' @importMethodsFrom Matrix as.vector
#' @importMethodsFrom Matrix diag
#' @importMethodsFrom Matrix norm
#' @importMethodsFrom Matrix tail
#' @importMethodsFrom Matrix head
#' @importMethodsFrom Matrix mean
#' @importMethodsFrom Matrix dim
#' @importMethodsFrom Matrix length
#' @importMethodsFrom Matrix image
#' @importMethodsFrom Matrix all
#' @importMethodsFrom Matrix any
#' @importMethodsFrom Matrix is.na
#' @importMethodsFrom Matrix dimnames
#' @importMethodsFrom Matrix [
#' @importClassesFrom Matrix dgCMatrix dgTMatrix
#' @keywords internal
#' @export
#' @name Matrix
NULL

#' Pipe operator
#'
#' See \code{magrittr::\link[magrittr:pipe]{\%>\%}} for details.
#'
#' @name %>%
#' @rdname pipe
#' @keywords internal
#' @export
#' @importFrom magrittr %>%
#' @usage lhs \%>\% rhs
#' @param lhs A value or the magrittr placeholder.
#' @param rhs A function call using the magrittr semantics.
#' @return The result of calling `rhs(lhs)`.
NULL
