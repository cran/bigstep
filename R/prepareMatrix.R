# Reduce size of the design matrix and order variables according to p-values.
prepareMatrix <- function(X, y, fitFun=fitLinear, minpv=0.15, maxp=1e6,
                          verbose=TRUE, file.out=NULL) {

  pv <- singleTests(X, y, fitFun, maxp, verbose)
  ord <- order(pv)
  pv <- pv[ord]
  ord <- ord[pv < minpv & !is.na(pv)]

  if (!is.null(file.out)) {
    n <- nrow(X)
    M <- ncol(X)
    names <- colnames(X)
    if (is.null(names))
      names <- 1:M
    part <- ceiling(maxp/M)
    parts <- split(1:n, ceiling(1:n/part))
    lp <- length(parts)
    if (verbose) {
      message("Writing to a file.")
      pb <- utils::txtProgressBar(min=0, max=lp, style=3)
    }
    file.out <- paste0(file.out, ".txt")
    utils::write.table(matrix(names[ord], nrow=1), file.out, quote=FALSE,
                col.names=FALSE, row.names=FALSE)
    for (i in seq_along(parts)) {
      XX <- X[parts[[i]], ord]
      utils::write.table(XX, file.out, quote=FALSE, append=TRUE,
                         col.names=FALSE, row.names=FALSE)
      if (verbose)
        utils::setTxtProgressBar(pb, i)
    }
    if (verbose) {
      close(pb)
      message(length(ord), " variables written to ", file.out, ".")
    }
  }

  return(ord)
}

