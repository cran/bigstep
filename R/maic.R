#' mAIC
#'
#' Calculate mAIC (modified Akaike Information Criterion).
#'
#' @param loglik a numeric, the log-likelihood.
#' @param n an integer > 0, a number of observations.
#' @param k an integer >= 0, a number of selected variables.
#' @param p an integer > 0, a number of all variables or a weight.
#' @param const numeric > 0, the expected number of significant variables.
#' @return A number, a value of mAIC.
#' @examples
#' maic(loglik=10, n=100, k=5, p=50)
#' @export

maic <- function(loglik, n, k, p, const=4) {
  stopifnot(p > 0, p/const > 1)
  maic.v <- aic(loglik, n, k) + 2*k*log(p/const - 1)
  return(maic.v)
}
