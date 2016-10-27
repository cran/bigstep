#' BIC
#'
#' Calculate BIC (Bayesian Information Criterion).
#'
#' @param rss a numeric > 0, the residual sum of squares.
#' @param n an integer > 0, a number of observations.
#' @param k an integer >= 0, a number of selected variables.
#' @return A number, a value of BIC.
#' @examples
#' bic(rss=10, n=100, k=5)
#' @export

bic <- function(rss, n, k) {
  stopifnot(n > 0, k >= 0, rss > 0)
  bic.v <- n*log(rss/n) + k*log(n)
  return(bic.v)
}
