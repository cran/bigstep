test_that("Forward step", {
  set.seed(1)
  n <- 100
  M <- 10
  X <- matrix(sample(0:2, M*n, rep=TRUE), ncol=M)
  y <- X[, 2] + 2*X[, 3] - X[, 6] + rnorm(n)
  Xm <- X[, 2:4]
  Xm <- cbind(1, Xm)
  modelf <- bigstep:::forwardStep(X, y, Xm=Xm, p=M)
  expect_equal(modelf$add, 6)
  rss <- sum(lm.fit(cbind(Xm, X[, 6]), y)$residuals^2)
  expect_equal(modelf$crit.v, mbic(-n/2*log(rss/n), n, 5, M))
  expect_equal(bigstep:::forwardStep(X, y, Xm=Xm[, 1, drop=FALSE], p=M)$add, 3)
  expect_equal(bigstep:::forwardStep(X[, 7:10], y, Xm=Xm, p=M, const=1)$add, 0)
  expect_equal(bigstep:::forwardStep(X, y, Xm=Xm[, 1, drop=FALSE],
                                     ord=c(4, 1, 2), p=M)$add, 2)
})

test_that("Multi-forward", {
  set.seed(1)
  n <- 100
  M <- 10
  X <- matrix(rnorm(M*n), ncol=M)
  y <- X[, 2] - X[, 3] + X[, 6] - X[, 10] + rnorm(n)
  Xm <- matrix(1, nrow=n)
  modelf <- bigstep:::multiForward(X, y, Xm=Xm, verbose=FALSE)
  expect_equal(modelf$add, c(2, 3, 6, 9, 10))
  rss <- sum(lm.fit(cbind(Xm, X[, c(2, 3, 6, 9, 10)]), y)$residuals^2)
  expect_equal(modelf$crit.v, bic(-n/2*log(rss/n), n, 6))
  expect_equal(bigstep:::multiForward(X, y, Xm=Xm, maxf=2, verbose=FALSE)$add,
               2:3)
  expect_equal(bigstep:::multiForward(X, y, Xm=Xm, ord=c(6, 2, 8, 10), maxp=10,
               verbose=FALSE)$add, c(6, 2, 10))
})
