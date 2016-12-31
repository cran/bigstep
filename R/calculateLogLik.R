# Calculate loglik.
calculateLogLik <- function(X, y, fitFun=fitLinear) {
  compl <- stats::complete.cases(X, y)
  loglik <- fitFun(X=X[compl, , drop=FALSE], y=y[compl])
  return(loglik)
}

