# Find the best variable to add to a model (according to a selected citerion).
forwardStep <- function(X, y, Xm=NULL, ord=1:ncol(X), crit=mbic, maxp=1e7,
                        ...) {
  nX <- ncol(X)
  nXm <- ifelse(is.null(Xm), 0, ncol(Xm))
  n <- length(y)
  rss <- ifelse(nXm != 0, fitModel(Xm, y), sum(y^2, na.rm=TRUE))
  crit.v <- crit(rss=rss, n=n, k=nX, ...)
  part <- ceiling(maxp/n)
  parts <- split(ord, ceiling(seq_along(ord)/part))
  add <- 0
  for (j in seq_along(parts)) {
    vars <- parts[[j]]
    XX <- as.matrix(X[, vars])
    for (i in seq_along(vars)) {
      Xm.new <- cbind(Xm, XX[, i])
      rss <- fitModel(Xm.new, y)
      crit.v.new <- crit(rss=rss, n=n, k=nXm+1, ...)
      if (crit.v.new < crit.v) {
        crit.v <- crit.v.new
        add <- vars[i]
      }
    }
  }
  list(add=add, crit.v=crit.v)
}
