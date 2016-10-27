#' Transpose a big matrix
#'
#' Transpose a big matrix and write to a file.
#'
#' @param X an object of class big.matrix. The rows of \code{X} contain the
#' observed variables, the columns of \code{X} contain samples.
#'
#' @param file.out a character string. The name of the output file.
#'
#' @param ord a numeric vector. The order in which new variables will be
#' written (the size of \cite{ord} can be smaller than \cite{X} if you do not
#' want to write some variables).
#'
#' @param sep a character, the field delimiter.
#'
#' @param type a character string. The type of data. Set \cite{NA} if you do
#' not know (but preferably specified).
#'
#' @param row.names a logical. If \cite{TRUE}, the first column contains the
#' row names.
#'
#' @param maxp a numeric. If \code{X} is big, it will be splitted into parts
#' with \code{maxp} elements. It will not change results, but it is
#' necessary if your computer does not have enough RAM. Set to a lower value
#' if you still have problems.
#'
#' @param verbose a logical. Set \code{FALSE} if you do not want to see any
#' information during the selection procedure.
#'
#' @param ... further arguments passed to \cite{read.big.matrix}.
#'
#' @return A numeric, a number of variables successfully written.
#'
#' @examples
#' \dontrun{
#' data <- matrix(sample(0:2, 20, replace=T), 2, 10)
#' rownames(data) <- c("X1", "X2")
#' write.table(data, "X.txt", col.names=FALSE)
#' X <- read.big.matrix("X.txt", sep=" ", type="char", has.row.names=TRUE)
#' # if it is possible, set type="char", reading will be quicker
#' transposeBigMatrix(X, sep=" ")
#' }
#'
#' @export

transposeBigMatrix <-
  function(X, file.out="Xtrans.txt", ord=NULL, sep="\t", type="char",
           row.names=TRUE, maxp=1e7, verbose=TRUE, ...) {

  n <- ncol(X)
  part <- ceiling(maxp/nrow(X))
  parts <- split(1:n, ceiling(seq_along(1:n)/part))
  lp <- length(parts)
  if (verbose) {
    message("Writing to the file...")
    pb <- utils::txtProgressBar(min=0, max=lp, style=3)
  }
  if (is.null(ord))
    ord <- 1:nrow(X)
  utils::write.table(t(rownames(X)[ord]), file.out, quote=FALSE, col.names=FALSE,
              row.names=FALSE)
  for (i in seq_along(parts)) {
    XX <- t(X[ord, parts[[i]]])
    utils::write.table(XX, file.out, quote=FALSE, append=TRUE, col.names=FALSE,
                row.names=FALSE)
    if (verbose)
      utils::setTxtProgressBar(pb, i)
  }
  if (verbose) {
    close(pb)
    message(length(ord), " variables saved to ", file.out)
  }

  return(length(ord))
}
