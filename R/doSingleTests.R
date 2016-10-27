#' Single tests
#'
#' Peform correlation tests between a vector y and a matrix X.
#'
#' @param X a numeric matrix or an object of class big.matrix (see 'Details').
#' The rows of \code{X} contain the samples, the columns of \code{X} contain
#' the observed variables.
#'
#' @param y a numeric vector of responses. The length of \code{y} must equal
#' the number of rows of \code{X}.
#'
#' @param maxp a numeric. If \code{X} is big, it will be splitted into parts
#' with \code{maxp} elements. It will not change results, but it is
#' necessary if your computer does not have enough RAM. Set to a lower value
#' if you still have problems.
#'
#' @param verbose a logical. Set \code{FALSE} if you do not want to see any
#' information during the selection procedure.
#'
#' @return A list with the following components:
#' \item{r}{a numeric vector with the Pearson correlation coefficients}
#' \item{t}{a numeric vector with t-values}
#' \item{pv}{a numeric vector with p-values}

#' @examples
#' set.seed(1)
#' n <- 100
#' M <- 10
#' X <- matrix(rnorm(M*n), ncol=M)
#' y <- X[, 2] - X[, 3] + X[, 6] - X[, 10] + rnorm(n, sd=1)
#' doSingleTests(X, y)
#'
#' @export

doSingleTests <- function(X, y, maxp=1e7, verbose=TRUE) {
  bigdata <- methods::is(X, "big.matrix")
  nX <- ncol(X)
  r <- numeric(nX)
  n <- numeric(nX)
  if (verbose)
    message("Performing single tests...")

  if (bigdata) {
    ind <- 1:nX
    part <- round(maxp/nrow(X))
    parts <- split(ind, ceiling(ind/part))
    lp <- length(parts)
    if (verbose)
      pb <- utils::txtProgressBar(min=0, max=lp, style=3)
    for (j in seq_along(parts)) {
      vars <- parts[[j]]
      XX <- as.matrix(X[, vars])
      for (i in seq_along(vars)) {
        r[vars[i]] <- suppressWarnings(stats::cor(y, XX[, i], use="complete.obs"))
        n[vars[i]] <- sum(!is.na(XX[, i]))
      }
      if (verbose)
        utils::setTxtProgressBar(pb, j)
    }
    if (verbose)
      close(pb)
  } else {
    n <- colSums(!is.na(X))
    for (i in seq_along(r))
      r[i] <- suppressWarnings(stats::cor(y, X[, i], use="complete.obs"))
  }

  t <- r*sqrt((n - 2)/(1 - r^2))
  pv <- 2*(1 - stats::pnorm(abs(t)))
  return(list(r=r, t=t, pv=pv))
}
