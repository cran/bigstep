#' BIC
#'
#' Calculate BIC (Bayesian Information Criterion).
#'
#' @param loglik a numeric, the log-likelihood.
#' @param n an integer > 0, a number of observations.
#' @param k an integer >= 0, a number of selected variables.
#' @return A number, a value of BIC.
#' @examples
#' bic(loglik=10, n=100, k=5)
#' @export

bic <- function(loglik, n, k) {
  stopifnot(n > 0, k >= 0)
  bic.v <- -2*loglik + k*log(n)
  return(bic.v)
}
