# Do single tests, multi-forward, multi-backward and stepwise.
allSteps <-
  function(X, y, fitFun=fitLinear, crit=mbic, Xm=NULL, stay=1, minpv=0.15,
           multif=TRUE, crit.multif=bic, maxf=min(ncol(X), 70), minb=0,
           maxp=1e6, verbose=TRUE, file.out=NULL, ...) {

  # single tests
  if (minpv < 1) {
    ord <- prepareMatrix(X, y, fitFun, minpv, maxp, verbose, file.out)
    nX <- length(ord)
    if (verbose)
      message("The desing matrix has been reduced to ", nX, " columns.")
  } else {
    ord <- 1:ncol(X)
  }

  # multiforward
  if (multif) {
    if (verbose)
      message("Starting the multi-forward step, max = ", maxf, " variables.")
    add <- multiForward(X, y, fitFun, Xm, ord, crit.multif, maxf, maxp,
                        verbose, ...)$add
    if (verbose)
      message("\nDone. ", length(add), " variables selected.")

    if (!is.null(add)) {
      Xm <- cbind(Xm, X[, add, drop=FALSE])

      # multibackward
      if (verbose)
        message("Starting the multi-backward step, min = ", minb, " variables.")
      Xm <- multiBackward(Xm, y, fitFun, crit, stay, minb, verbose, ...)
      if (verbose)
        message("\nDone. ", ncol(Xm) - 1, " variables left.")  # - intercept
    }
  }

  # stepwise
  if (verbose)
    message("Starting stepwise.")
  Xm <- stepwise(X, y, fitFun, crit, Xm, stay, ord, maxp, verbose, ...)
  if (verbose)
    message("\nDone. ", ncol(Xm) - 1, " variables selected.")

  return(Xm)
}
