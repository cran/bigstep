#' Single tests
#'
#' Perform the likelihood ratio tests between two regression models: 1) with
#' only the intercept, 2) with the intercept and every single variable
#' from the matrix \code{X}.
#'
#' @param X a numeric matrix or an object of class big.matrix (see 'Details').
#' The rows of \code{X} contain the samples, the columns of \code{X} contain
#' the observed variables. If your have variables in rows, see 'Details'.
#'
#' @param y a numeric vector of responses. The length of \code{y} must equal
#' the number of rows of \code{X}.
#'
#' @param fitFun a function which fits the regression model and calculate
#' the logarithm of the likelihood function (loglike). You can use your own
#' function or one of these: \code{fitLinear}, \code{fitLogistic},
#' \code{fitPoisson}.
#'
#' @param fastST a logical. If \code{TRUE}, the Pearson correlation
#' coefficients between \code{y} and all columns of \code{X} are calculated
#' instead of the likelihood ratio tests (see ?bigstep). It is faster but
#' works only if you do not have any missing values.
#'
#' @param maxp a numeric. If \code{X} is big, it will be splitted into parts
#' with \code{maxp} elements. It will not change results, but it is
#' necessary if your computer does not have enough RAM. Set to a lower value
#' if you still have problems.
#'
#' @param verbose a logical. Set \code{FALSE} if you do not want to see any
#' information during the selection procedure.
#'
#' @return A numeric vector with p-values of the likelihood ratio test
#' (or the Pearson correlation test if \code{fastST=TRUE}).

#' @examples
#' set.seed(1)
#' n <- 100
#' M <- 10
#' X <- matrix(rnorm(M*n), ncol=M)
#' y <- X[, 2] - X[, 3] + X[, 6] - X[, 10] + rnorm(n)
#' singleTests(X, y)
#'
#' @export

singleTests <- function(X, y, fitFun=fitLinear, fastST=FALSE, maxp=1e6,
                        verbose=TRUE) {
  nX <- ncol(X)
  n <- length(y)
  ind <- 1:nX
  part <- round(maxp/nrow(X))
  parts <- split(ind, ceiling(ind/part))
  lp <- length(parts)

  if (verbose) {
    message("Performing single tests...")
    pb <- utils::txtProgressBar(min=0, max=lp, style=3)
  }

  if (!fastST) {
    Xm <- matrix(1, n, 2)
    L0 <- calculateLogLik(Xm[, 1, drop=F], y, fitFun)  # loglik for null model
    L1 <- numeric(nX)
    for (j in seq_along(parts)) {
      vars <- parts[[j]]
      XX <- as.matrix(X[, vars])
      for (i in seq_along(vars)) {
        Xm[, 2] <- XX[, i]
        L1[vars[i]] <- calculateLogLik(Xm, y, fitFun)
      }
      if (verbose)
        utils::setTxtProgressBar(pb, j)
    }
    if (verbose)
      close(pb)
    LR <- 2*(L1 - L0)
    pv <- stats::pchisq(LR, 1, lower=FALSE)

  } else {

    r <- numeric(nX)
    y <- (y - mean(y))/sd(y)
    for (j in seq_along(parts)) {
      vars <- parts[[j]]
      XX <- as.matrix(X[, vars])
      m <- colMeans(XX)
      sd <- matrixStats::colSds(XX, center=m)
      XX <- t((t(XX) - m)/sd)
      for (i in seq_along(vars))
        r[vars[i]] <- crossprod(y, XX[, i])
      if (verbose)
        utils::setTxtProgressBar(pb, j)
    }
    if (verbose)
      close(pb)
    r <- r/(n - 1)
    t <- r*sqrt((n - 2)/(1 - r^2))
    pv <- 2*stats::pt(abs(t), n-2, lower=FALSE)
  }

  return(pv)
}
