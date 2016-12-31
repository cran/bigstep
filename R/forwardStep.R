# Find the best variable to add to the model (according to the selected
# citerion).
forwardStep <- function(X, y, fitFun=fitLinear, crit=mbic, Xm=NULL,
                        ord=1:ncol(X), maxp=1e6, ...) {

  nX <- ncol(X)
  nXm <- ncol(Xm) # there is always the intercept in Xm
  n <- length(y)
  part <- ceiling(maxp/n)
  parts <- split(ord, ceiling(seq_along(ord)/part))
  add <- 0

  loglik <- calculateLogLik(Xm, y, fitFun)
  crit.v <- R.utils::doCall(crit, loglik=loglik, n=n, k=nXm, Xm=Xm, ...)

  for (j in seq_along(parts)) {
    vars <- parts[[j]]
    XX <- as.matrix(X[, vars, drop=FALSE])
    for (i in seq_along(vars)) {
      Xm.new <- cbind(Xm, XX[, i, drop=FALSE])
      loglik <- calculateLogLik(Xm.new, y, fitFun)
      crit.v.new <- R.utils::doCall(crit, loglik=loglik, n=n, k=nXm + 1,
                                    Xm=Xm.new, ...)
      if (crit.v.new < crit.v) {
        crit.v <- crit.v.new
        add <- vars[i]
      }
    }
  }

  list(add=add, crit.v=crit.v)
}
