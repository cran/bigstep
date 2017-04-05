#' Combine, transpose big matrices and change colnames
#'
#' Combine and/or transpose big matrices and write to a file with new
#' names of columns
#'
#' @param X an object of class big.matrix.
#'
#' @param Z an object of class big.matrix. Specify if you want to combine
#' matrices.
#'
#' @param file.out a character string. The name of the output file.
#'
#' @param transX a logical. If \code{TRUE}, the matrix \code{X} will be
#' transposed.
#'
#' @param transZ a logical. If \code{TRUE}, the matrix \code{Z} will be
#' transposed.
#'
#' @param new.names a vector of strings. Specify if you want to change names
#' of variables.
#'
#' @param maxp a numeric. If \code{X} is big, it will be splitted into parts
#' with \code{maxp} elements. It will not change results, but it is
#' necessary if your computer does not have enough RAM. Set to a lower value
#' if you still have problems.
#'
#' @param verbose a logical. Set \code{FALSE} if you do not want to see any
#' information during the selection procedure.
#'
#' @return A numeric, a number of variables successfully written.
#'
#' @examples
#' \dontrun{
#' library(bigmemory)
#' X <- matrix(sample(0:2, 20, replace=TRUE), 2, 10)
#' Z <- matrix(sample(0:2, 20, replace=TRUE), 2, 10)
#' write.table(X, "X.txt", col.names=FALSE, row.names=FALSE)
#' write.table(Z, "Z.txt", col.names=FALSE, row.names=FALSE)
#' X <- read.big.matrix("X.txt", sep=" ", type="char")
#' Z <- read.big.matrix("Z.txt", sep=" ", type="char")
#' # if it is possible, set type="char", reading will be quicker
#' names <- c("X1", "X2", "Z1", "Z2")
#' combineBigMatrices(X, Z, transX=TRUE, transZ=TRUE, new.names=names)
#' }
#'
#' @export

combineBigMatrices <-
  function(X, Z=NULL, file.out="XZ.txt", transX=FALSE, transZ=FALSE,
           new.names=NULL, maxp=1e7, verbose=TRUE) {

    if (transX) {
      n <- ncol(X)
      p <- nrow(X)
      if (is.null(new.names))
        names <- rownames(X)
    } else {
      n <- nrow(X)
      p <- ncol(X)
      if (is.null(new.names))
        names <- colnames(X)
    }
    if (!is.null(Z)) {
      if (transZ) {
        n2 <- ncol(Z)
        p <- p + nrow(Z)
        if (is.null(new.names))
          names <- c(names, rownames(Z))
      } else {
        n2 <- nrow(Z)
        p <- p + ncol(Z)
        if (is.null(new.names))
          names <- c(names, colnames(X))
      }
      stopifnot(n == n2)
    }

    if (!is.null(new.names)) {
      names <- new.names
      stopifnot(length(names) == p)
    }
    if (length(names) < p)
      names <- 1:p
    names <- t(names)

    part <- ceiling(maxp/p)
    parts <- split(1:n, ceiling(seq_along(1:n)/part))
    lp <- length(parts)

    if (verbose) {
      message("Writing to the file...")
      pb <- utils::txtProgressBar(min=0, max=lp, style=3)
    }
    utils::write.table(names, file.out, quote=FALSE, col.names=FALSE,
                       row.names=FALSE)
    if (is.null(Z)) {
      for (i in seq_along(parts)) {
        if (transX) {
          XX <- t(X[, parts[[i]]])
        } else {
          XX <- X[parts[[i]], ]
        }
        utils::write.table(XX, file.out, quote=FALSE, append=TRUE,
                           col.names=FALSE, row.names=FALSE)
        if (verbose)
          utils::setTxtProgressBar(pb, i)
      }
    } else {
      for (i in seq_along(parts)) {
        if (transX) {
          XX <- t(X[, parts[[i]]])
        } else {
          XX <- X[parts[[i]], ]
        }
        if (transZ) {
          ZZ <- t(Z[, parts[[i]]])
        } else {
          ZZ <- Z[parts[[i]], ]
        }
        utils::write.table(cbind(XX, ZZ), file.out, quote=FALSE, append=TRUE,
                           col.names=FALSE, row.names=FALSE)
        if (verbose)
          utils::setTxtProgressBar(pb, i)
      }
    }

    if (verbose) {
      close(pb)
      message(p, " variables saved to ", file.out)
    }
    return(p)
  }
