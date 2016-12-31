test_that("Backward step", {
  set.seed(1)
  n <- 100
  M <- 10
  X <- cbind(1, matrix(sample(0:2, M*n, rep=TRUE), ncol=M))
  y <- X[, 2] + X[, 3] - X[, 6] + rnorm(n)
  Xm <- matrix(1, nrow=n)
  k <- c(1:4, 6)

  expect_error(bigstep:::backwardStep(X[, k], y, p=length(k) - 1),
               "p/const > 1 is not TRUE")
  modelb <- bigstep:::backwardStep(X[, k], y, p=M)
  expect_equal(modelb$drop, 4)
  rss <- sum(lm.fit(X[, c(1:3, 6)], y)$residuals^2)
  expect_equal(modelb$crit.v, mbic(-n/2*log(rss/n), n, 4, M))

  # stay
  expect_equal(bigstep:::backwardStep(X[, k], y, p=M, stay=4)$drop, 0)
  expect_equal(bigstep:::backwardStep(X, y, p=M, stay=10)$drop, 11)
  expect_equal(bigstep:::backwardStep(X, y, p=M, stay=11)$drop, 0)
})

test_that("Multi-backward", {
  set.seed(1)
  n <- 100
  M <- 10
  X <- matrix(rnorm(M*n), ncol=M)
  colnames(X) <- 1:M
  y <- X[, 3] - X[, 4] + X[, 7] - X[, 10] + rnorm(n)
  X <- cbind(1, X)

  Xm <- bigstep:::multiBackward(X, y, verbose=FALSE, p=M)
  expect_equal(as.numeric(colnames(Xm)[-1]), c(3, 4, 7, 10))
  Xm <- bigstep:::multiBackward(X, y, verbose=FALSE, p=M, minb=7)
  expect_equal(ncol(Xm), 8)
  Xm <- bigstep:::multiBackward(X, y, stay=5, verbose=FALSE, p=M)
  expect_equal(as.numeric(colnames(Xm)[-1]), c(1:4, 7, 10))
})
