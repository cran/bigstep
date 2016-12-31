# The stepwise procedure using the selected criterion.
stepwise <- function(X, y, fitFun=fitLinear, crit=mbic, Xm=NULL, stay=1,
                     ord=1:ncol(X), maxp=1e6, verbose=TRUE, ...) {

  n <- length(y)
  loglik <- calculateLogLik(Xm, y, fitFun)
  crit.v <- R.utils::doCall(crit, loglik=loglik, n=n, k=ncol(Xm), Xm=Xm, ...)

  repeat {
    mf <- forwardStep(X, y, fitFun, crit, Xm, ord, maxp, ...)
    mb <- backwardStep(Xm, y, fitFun, crit, stay, ...)
    crit.v.new <- min(mf$crit.v, mb$crit.v)
    if (crit.v.new < crit.v) {
      if (mf$crit.v < mb$crit.v) {
        Xm <- cbind(Xm, X[, mf$add, drop=F])
      } else {
        Xm <- Xm[, -mb$drop, drop=FALSE]
      }
      if (verbose)
        message(".", appendLF=FALSE)
      crit.v <- crit.v.new
    } else {
      break
    }
  }

  return(Xm)
}
