# Reduce size of design matrix and order variables according to p-values.
prepareMatrix <- function(X, y, minpv=0.15, maxp=1e7, verbose=TRUE,
                          write=FALSE, file.out="Xshort.txt") {

  res <- doSingleTests(X, y, maxp, verbose)
  t <- abs(res$t)
  ord <- order(t, decreasing=TRUE)
  pv <- res$pv[ord]
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
      utils::write.table(XX, file.out, quote=FALSE, append=TRUE, col.names=FALSE,
                  row.names=FALSE)
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

