# The multi-backward procedure using a selected criterion.
multiBackward <- function(Xm, y, minb=0, stay=NULL, crit=mbic, verbose=TRUE,
                          ...) {
  nXm <- ncol(Xm)
  colnames(Xm) <- 1:nXm
  n <- length(y)
  rss <- fitModel(Xm, y)
  crit.v <- crit(rss=rss, n=n, k=nXm, ...)
  if (verbose)
    message("Starting the multi-backward step, min = ", minb, " variables.")
  stay.old <- stay
  drop <- NULL
  while (nXm > minb) {
    names <- as.numeric(colnames(Xm))
    if (!is.null(stay.old))
      stay <- which(names %in% stay.old)
    model <- backwardStep(Xm, y, stay, crit, ...)
    crit.v.new <- model$crit.v
    if (crit.v.new < crit.v) {
      if (verbose)
        message(".", appendLF=FALSE)
      crit.v <- crit.v.new
      drop <- c(drop, names[model$drop])
      Xm <- Xm[, -model$drop, drop=FALSE]
      nXm <- nXm - 1
    } else {
      minb <- nXm # there are no more appropriate variables to remove
    }
  }
  if (verbose)
    message("\nDone. ", nXm, " variables left.")
  return(list(drop=drop, stay=stay))
}
