#' mAIC2
#'
#' Calculate mAIC2 (the second version of modified Akaike Information
#' Criterion).
#'
#' @param rss a numeric > 0, the residual sum of squares.
#' @param n an integer > 0, a number of observations.
#' @param k an integer >= 0, a number of chosen variables.
#' @param p an integer > 0, a number of all variables or a weight.
#' @param const numeric > 0, the expected number of significant variables.
#' @return A number, a value of mAIC2.
#' @examples
#' mbic2(rss=10, n=100, k=5, p=50)
#' @export


maic2 <- function(rss, n, k, p, const=4) {
  penalty <- ifelse(k<170, 2*log(factorial(k)), 2*sum(log(1:k)))
  # if k is big, log(factorial(k))=Inf
  maic2.v <- maic(rss, n, k, p, const) - penalty
  return(maic2.v)
}
