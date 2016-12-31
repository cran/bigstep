#' Poisson regression
#'
#' Fit the Poisson regression model and calculate the log-likelihood.
#'
#' @param X a numeric matrix.
#' @param y a numeric vector.
#' @return A number, the log-likelihood.
#' @examples
#' set.seed(1)
#' n <- 100
#' M <- 10
#' X <- matrix(rnorm(M*n), ncol=M)
#' mu <- X[, 2] - X[, 3] + X[, 6] - X[, 10]
#' y <- rpois(n, exp(mu))
#' fitPoisson(X, y)
#' @export

fitPoisson <- function(X, y) {
  model <- speedglm::speedglm.wfit(y, X, family=stats::poisson())
  loglik <- model$logLik
  return(loglik)
}
