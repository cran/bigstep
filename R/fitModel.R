# Fit the linear regression model and calculate residual sum of squares.
fitModel <- function(Xm, y) {
  compl <- stats::complete.cases(Xm, y)
  m <- speedglm::speedlm.fit(y[compl], Xm[compl, , drop=FALSE])
  rss <- m$RSS
  if (rss < 0) {  # sometimes speedlm returns rss<0
    warning("RSS<0; set to 0")
    rss <- 0
  }
  return(rss)
}

