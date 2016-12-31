#' mAIC2
#'
#' Calculate mAIC2 (the second version of modified Akaike Information
#' Criterion).
#'
#' @param loglik a numeric, the log-likelihood.
#' @param n an integer > 0, a number of observations.
#' @param k an integer >= 0, a number of selected variables.
#' @param p an integer > 0, a number of all variables or a weight.
#' @param const numeric > 0, the expected number of significant variables.
#' @return A number, a value of mAIC2.
#' @examples
#' mbic2(loglik=10, n=100, k=5, p=50)
#' @export

maic2 <- function(loglik, n, k, p, const=4) {
  penalty <- ifelse(k<170, 2*log(factorial(k)), 2*sum(log(1:k)))
  # if k is big, log(factorial(k))=Inf
  maic2.v <- maic(loglik, n, k, p, const) - penalty
  return(maic2.v)
}
