#' Logistic regression
#'
#' Fit the logistic regression model and calculate the log-likelihood.
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
#' p <- 1/(1 + exp(-mu))
#' y <- rbinom(n, 1, p)
#' fitLogistic(X, y)
#' @export

fitLogistic <- function(X, y) {
  model <- speedglm::speedglm.wfit(y, X, family=stats::binomial())
  loglik <- model$logLik
  return(loglik)
}
