#' mBIC
#'
#' Calculate mBIC (modified Bayesian Information Criterion).
#'
#' @param loglik a numeric, the log-likelihood.
#' @param n an integer > 0, a number of observations.
#' @param k an integer >= 0, a number of selected variables.
#' @param p an integer > 0, a number of all variables or a weight.
#' @param const numeric > 0, the expected number of significant variables.
#' @return A number, a value of mBIC.
#' @examples
#' mbic(loglik=10, n=100, k=5, p=50)
#' @export

mbic <- function(loglik, n, k, p, const=4) {
  stopifnot(p > 0, p/const > 1)
  mbic.v <- bic(loglik, n, k) + 2*k*log(p/const - 1)
  return(mbic.v)
}
