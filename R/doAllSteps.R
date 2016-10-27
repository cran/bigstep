# Do multi-forwad, multi-backward and stepwise.
doAllSteps <-
  function(X, y, Xm=NULL, stay=NULL, ord=1:ncol(X), multif=TRUE,
           maxf=min(ncol(X), 70), minb=0, crit=mbic, crit.multif=bic, maxp=1e7,
           verbose=TRUE, ...) {

  if (is.null(Xm)) {
    model <- NULL
  } else {
    model <- -(1:ncol(Xm))
  }

  if (multif) {
    add <- multiForward(X, y, Xm, ord, maxf, crit.multif, maxp, verbose)$add
    Xm <- cbind(Xm, X[, add])
    model <- c(model, add)
    res.b <- multiBackward(Xm, y, minb, stay, crit, verbose, ...)
    if (!is.null(res.b$drop)) {
      Xm <- Xm[, -res.b$drop, drop=FALSE]
      model <- model[-res.b$drop]
      stay <- res.b$stay
    }
  }
  res.s <- stepwise(X, y, Xm, stay, ord, crit, maxp, verbose, ...)
  model <- c(model[res.s$Xm], res.s$X)

  return(list(Xm=-model[model<0], X=model[model>0]))
}
