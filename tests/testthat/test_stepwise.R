test_that("Stepwise", {
  set.seed(1)
  n <- 200
  M <- 10
  X <- matrix(rnorm(M*n), ncol=M)
  colnames(X) <- 1:M
  y <- X[, 2] - X[, 3] + X[, 6] - X[, 10] + rnorm(n)
  Xm <- matrix(1, nrow=n)
  Xs <- bigstep:::stepwise(X, y, Xm=Xm, verbose=FALSE, p=M)

  expect_equal(sort(as.numeric(colnames(Xs)[-1])), c(2, 3, 6, 10))
  Xm <- cbind(1, rnorm(n), X[, 3:4], rnorm(n), X[, 6])
  colnames(Xm) <- -(1:6)
  Xs <- bigstep:::stepwise(X[, -6], y, Xm=Xm, stay=3, verbose=FALSE, p=M)
  expect_equal(sort(as.numeric(colnames(Xs))), c(-6, -3, -2, -1, 2, 10))

  y <- rbinom(n, 1, 0.5)
  expect_equal(ncol(bigstep:::stepwise(X, y, Xm=Xm,
               verbose=FALSE, p=M)), 1)
  expect_equal(ncol(bigstep:::stepwise(X=Xm, y, Xm=Xm,
               verbose=FALSE, p=M)), 1)
})

