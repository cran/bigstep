test_that("Fit model", {
  set.seed(1)
  n <- 200
  M <- 20
  X <- matrix(sample(0:2, M*n, rep=TRUE), ncol=20)
  y <- X[, 1] + X[, 2] - X[, 6] + rnorm(n)
  rss1 <- bigstep:::calculateLogLik(X, y)
  rss2 <- -n/2*log(sum(lm.fit(X, y)$residuals^2)/n)
  expect_equal(rss1, rss2)
})
