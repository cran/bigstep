test_that("Fit model", {
  set.seed(1)
  n <- 200
  M <- 20
  X <- matrix(sample(0:2, M*n, rep=TRUE), ncol=20)
  y <- X[, 1] + X[, 2] - X[, 6] + rnorm(n, sd=0.1)
  rss1 <- fitModel(X, y)
  rss2 <- sum(lm.fit(X, y)$residuals^2)
  expect_equal(rss1, rss2)
})
