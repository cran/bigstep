#' mBIC2
#'
#' Calculate mBIC2 (the second version of modified Bayesian Information
#' Criterion).
#'
#' @param loglik a numeric, the log-likelihood.
#' @param n an integer > 0, a number of observations.
#' @param k an integer >= 0, a number of selected variables.
#' @param p an integer > 0, a number of all variables or a weight.
#' @param const numeric > 0, the expected number of significant variables.
#' @return A number, a value of mBIC2.
#' @examples
#' mbic2(loglik=10, n=100, k=5, p=50)
#' @export

mbic2 <- function(loglik, n, k, p, const=4) {
  penalty <- ifelse(k<170, 2*log(factorial(k)), 2*sum(log(1:k)))
  # if k is big, log(factorial(k))=Inf
  mbic2.v <- mbic(loglik, n, k, p, const) - penalty
  return(mbic2.v)
}
