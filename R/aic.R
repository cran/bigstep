#' AIC
#'
#' Calculate AIC (Akaike Information Criterion).
#'
#' @param loglik a numeric, the log-likelihood.
#' @param n an integer > 0, a number of observations.
#' @param k an integer >= 0, a number of selected variables.
#' @return A number, a value of AIC.
#' @examples
#' aic(loglik=10, n=100, k=5)
#' @export

aic <- function(loglik, n, k) {
  stopifnot(n > 0, k >= 0)
  aic.v <- -2*loglik + 2*k
  return(aic.v)
}
