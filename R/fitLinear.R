#' Linear regression
#'
#' Fit the linear regression model and calculate the log-likelihood.
#'
#' @param X a numeric matrix.
#' @param y a numeric vector.
#' @return A number, the log-likelihood.
#' @examples
#' set.seed(1)
#' n <- 100
#' M <- 10
#' X <- matrix(rnorm(M*n), ncol=M)
#' y <- X[, 2] - X[, 3] + X[, 6] - X[, 10] + rnorm(n)
#' fitLinear(X, y)
#' @export

fitLinear <- function(X, y) {
  model <- RcppEigen::fastLmPure(X, y)
  rss <- sum(model$residuals^2)
  n <- length(y)
  loglik <- -n/2*log(rss/n)
  return(loglik)
}


