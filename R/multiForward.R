# The multi-forward procedure using crit.multif.
multiForward <-
  function(X, y, fitFun=fitLinear, Xm=NULL, ord=1:ncol(X), crit.multif=bic,
           maxf=min(ncol(X), 70), maxp=1e6, verbose=TRUE, ...) {

  nX <- ncol(X)
  nXm <- ncol(Xm) # there is always the intercept in Xm
  if (maxf > nX)
    maxf <- nX
  n <- length(y)
  add <- NULL
  maxf <- maxf + nXm
  part <- ceiling(maxp/n)
  parts <- split(ord, ceiling(seq_along(ord)/part))

  loglik <- calculateLogLik(Xm, y, fitFun)
  crit.v <- R.utils::doCall(crit.multif, loglik=loglik, n=n, k=nXm, Xm=Xm, ...)

  for (j in seq_along(parts)) {
    vars <- parts[[j]]
    XX <- as.matrix(X[, vars, drop=FALSE])
    for (i in seq_along(vars)) {
      Xm.new <- cbind(Xm, XX[, i, drop=FALSE])
      loglik <- calculateLogLik(Xm.new, y, fitFun)
      crit.v.new <- R.utils::doCall(crit.multif, loglik=loglik, n=n, k=nXm+1,
                                    Xm=Xm, ...)
      if (crit.v.new < crit.v) {
        if (verbose)
          message(".", appendLF=FALSE)
        add <- c(add, vars[i])
        nXm <- nXm + 1
        crit.v <- crit.v.new
        Xm <- Xm.new
        if (nXm >= maxf)
          return(list(add=add, crit.v=crit.v))
      }
    }
  }

  return(list(add=add, crit.v=crit.v))
}
