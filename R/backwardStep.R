# Find the best variable to remove from the model (according to the selected
# citerion)
backwardStep <- function(Xm, y, fitFun=fitLinear, crit=mbic, stay=1, ...) {

  nXm <- ncol(Xm) # there is always the intercept in X
  if (nXm <= stay)
    return(list(drop=0, crit.v=Inf))
  n <- length(y)
  drop <- 0

  loglik <- calculateLogLik(Xm, y, fitFun)
  crit.v <- R.utils::doCall(crit, loglik=loglik, n=n, k=nXm, Xm=Xm, ...)

  for (i in (stay + 1):nXm) {
    Xm.new <- Xm[, -i, drop=FALSE]
    loglik <- calculateLogLik(Xm.new, y, fitFun)
    crit.v.new <- R.utils::doCall(crit, loglik=loglik, n=n, k=nXm-1, Xm=Xm, ...)
    if (crit.v.new < crit.v) {
      crit.v <- crit.v.new
      drop <- i
    }
  }

  return(list(drop=drop, crit.v=crit.v))
}
