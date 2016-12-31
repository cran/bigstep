# The multi-backward procedure using the selected criterion.
multiBackward <- function(Xm, y, fitFun=fitLinear, crit=mbic, stay=1, minb=0,
                          verbose=TRUE, ...) {

  nXm <- ncol(Xm)  # first column is an intercept
  n <- length(y)
  drop <- NULL

  loglik <- calculateLogLik(Xm, y, fitFun)
  crit.v <- R.utils::doCall(crit, loglik=loglik, n=n, k=nXm, Xm=Xm, ...)

  while (nXm > minb + stay) {
    model <- backwardStep(Xm, y, fitFun, crit, stay, ...)
    crit.v.new <- model$crit.v
    if (crit.v.new < crit.v) {
      if (verbose)
        message(".", appendLF=FALSE)
      crit.v <- crit.v.new
      Xm <- Xm[, -model$drop, drop=FALSE]
      nXm <- nXm - 1
    } else {
      break # there are no more appropriate variables to remove
    }
  }

  return(Xm)
}
