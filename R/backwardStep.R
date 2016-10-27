# Find the best variable to remove from a model (according to a selected
# citerion)
backwardStep <- function(Xm, y, stay=NULL, crit=mbic, ...) {
  nXm <- ifelse(is.null(Xm), 0, ncol(Xm))
  if (nXm == length(stay))
    return(list(drop=0, crit.v=Inf))
  cand <- 1:nXm
  if (!is.null(stay)) {
    cand <- cand[-stay]
    if (length(cand) == 0)
      return(list(drop=0, crit.v=Inf))
  }
  n <- length(y)

  rss <- fitModel(Xm, y)
  crit.v <- crit(rss=rss, n=n, k=nXm, ...)
  drop <- 0
  for (i in cand) {
    Xm.new <- Xm[, -i, drop=FALSE]
    rss <- ifelse(nXm > 1, fitModel(Xm.new, y), sum(y^2, na.rm=TRUE))
    # if nXm == 1, thera are no variable in Xm.new, even intercept
    crit.v.new <- crit(rss=rss, n=n, k=nXm-1, ...)
    if (crit.v.new < crit.v) {
      crit.v <- crit.v.new
      drop <- i
    }
  }

  return(list(drop=drop, crit.v=crit.v))
}
