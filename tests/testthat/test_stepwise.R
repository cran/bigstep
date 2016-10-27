test_that("Stepwise", {
  set.seed(1)
  n <- 200
  M <- 10
  X <- matrix(rnorm(M*n), ncol=M)
  y <- X[, 2] - X[, 3] + X[, 6] - X[, 10] + rnorm(n, sd=0.1)
  model <- c(2, 3, 6, 10)
  expect_equal(sort(stepwise(X, y, verbose=FALSE, p=M)$X), model)
  expect_equal(sort(stepwise(X, y, Xm=matrix(1, nrow=n), verbose=FALSE, p=M)$X),
               model)
  results <- stepwise(X, y, Xm=matrix(1, nrow=n), stay=1, verbose=FALSE, p=M)
  expect_equal(sort(results$X), model)
  expect_equal(results$Xm, 1)
  Xm <- cbind(X[, 3:4], rnorm(n), X[, 7], 1, rnorm(n))
  results <- stepwise(X, y + 10, Xm=Xm, stay=c(2, 4), verbose=FALSE, p=M)
  expect_equal(sort(results$X), c(2, 6, 10))
  expect_equal(sort(results$Xm), c(1, 2, 4, 5))
})

