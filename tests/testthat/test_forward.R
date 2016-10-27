test_that("Forward step", {
  set.seed(1)
  n <- 100
  M <- 10
  X <- matrix(sample(0:2, M*n, rep=T), ncol=M)
  y <- X[, 1] + 2*X[, 2] - X[, 6] + rnorm(n, sd=0.1)
  Xm <- X[, 1:3]
  modelf <- forwardStep(X, y, Xm, p=M)
  expect_equal(modelf$add, 6)
  rss <- sum(lm.fit(X[, c(1:3, 6)], y)$residuals^2)
  expect_equal(modelf$crit.v, mbic(rss, n, 4, M))
  expect_equal(forwardStep(X, y, p=M)$add, 2)
  expect_equal(forwardStep(X[, 9:10], y, Xm, p=M, const=1)$add, 0)
  expect_equal(forwardStep(X, y, ord=c(3, 1, 10), p=M)$add, 1)
})

test_that("Multi-forward", {
  set.seed(1)
  n <- 100
  M <- 10
  X <- matrix(rnorm(M*n), ncol=M)
  y <- X[, 2] - X[, 3] + X[, 6] - X[, 10] + rnorm(n, sd=0.1)
  modelf <- multiForward(X, y, verbose=FALSE)
  expect_equal(modelf$add, c(2, 3, 6, 9, 10))
  rss <- sum(lm.fit(X[, c(2, 3, 6, 9, 10)], y)$residuals^2)
  expect_equal(modelf$crit.v, bic(rss, n, 5))
  expect_equal(multiForward(X, y, maxf=2, verbose=FALSE)$add, 2:3)
  expect_true(multiForward(X, y + 1, Xm=matrix(1, nrow=n), maxf=2,
                           verbose=FALSE)$crit.v <
              multiForward(X, y + 1, Xm=NULL, maxf=2, verbose=FALSE)$crit.v)
  expect_equal(multiForward(X, y, ord=c(6, 2, 8, 10), maxp=10,
                            verbose=FALSE)$add, c(6, 2, 10))
})
