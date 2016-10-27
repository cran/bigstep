# The multi-forward procedure using crit.multif.
multiForward <- function(X, y, Xm=NULL, ord=1:ncol(X), maxf=min(ncol(X), 70),
                         crit.multif=bic, maxp=1e7, verbose=TRUE) {

  nXm <- ifelse(is.null(Xm), 0, ncol(Xm))
  nX <- ncol(X)
  if (maxf > nX)
    maxf <- nX
  n <- length(y)
  add <- NULL
  if (verbose)
    message("Starting the multi-forward step, max = ", maxf, " variables.")
  maxf <- maxf + nXm
  rss <- ifelse(nXm > 0, fitModel(Xm, y), sum(y^2, na.rm=TRUE))
  crit.v <- crit.multif(rss=rss, n=n, k=nXm)

  part <- ceiling(maxp/n)
  parts <- split(ord, ceiling(seq_along(ord)/part))
  for (j in seq_along(parts)) {
    vars <- parts[[j]]
    XX <- as.matrix(X[, vars])
    for (i in seq_along(vars)) {
      Xm.new <- cbind(Xm, XX[, i])
      rss <- fitModel(Xm.new, y)
      crit.v.new <- crit.multif(rss=rss, n=n, k=nXm+1)
      if (crit.v.new < crit.v) {
        if (verbose)
          message(".", appendLF=FALSE)
        add <- c(add, vars[i])
        nXm <- nXm + 1
        crit.v <- crit.v.new
        Xm <- Xm.new
        if (nXm >= maxf) {
          if (verbose)
            message("\nDone. ", length(add), " variables selected.")
          return(list(add=add, crit.v=crit.v))
        }
      }
    }
  }
  if (verbose)
    message("\nDone. ", length(add), " variables selected.")

  return(list(add=add, crit.v=crit.v))
}
