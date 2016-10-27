# The stepwise procedure using a selected criterion.
stepwise <- function(X, y, Xm=NULL, stay=NULL, ord=1:ncol(X), crit=mbic,
                     maxp=1e7, verbose=TRUE, ...) {
  n <- length(y)
  nXm <- ifelse(is.null(Xm), 0, ncol(Xm))
  if (nXm > 0)
    colnames(Xm) <- 1:nXm
  if (verbose)
    message("Starting stepwise.")
  if (!is.null(stay))
    stay.old <- as.character(stay)

  rss <- ifelse(nXm != 0, fitModel(Xm, y), sum(y^2, na.rm=TRUE))
  crit.v <- crit(rss=rss, n=n, k=nXm, ...)
  repeat {
    model <- colnames(Xm)
    if (!is.null(stay))
      stay <- which(model %in% stay.old)
    mf <- forwardStep(X, y, Xm, ord, crit, maxp, ...)
    mb <- backwardStep(Xm, y, stay, crit, ...)
    crit.v.new <- min(mf$crit.v, mb$crit.v)
    if (crit.v.new < crit.v) {
      if (mf$crit.v < mb$crit.v) {
        Xm <- cbind(Xm, X[, mf$add])
        colnames(Xm)[ncol(Xm)] <- -mf$add
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

  nXm <- ifelse(is.null(Xm), 0, ncol(Xm))
  if (verbose)
    message("\nDone. ", nXm, " variables selected.")
  model <- as.numeric(model)
  model <- list(X=-model[model<0], Xm=model[model>0])

  return(model)
}
