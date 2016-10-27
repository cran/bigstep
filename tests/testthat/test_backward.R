test_that("Backward step", {
  set.seed(1)
  n <- 100
  M <- 10
  X <- matrix(sample(0:2, M*n, rep=T), ncol=M)
  y <- X[, 1] + X[, 2] - X[, 6] + rnorm(n, sd=0.1)
  k <- c(1:3, 6)

  expect_error(backwardStep(X[, k], y, p=length(k)), "p/const > 1 is not TRUE")
  modelb <- backwardStep(X[, k], y, p=M)
  expect_equal(modelb$drop, 3)
  rss <- sum(lm.fit(X[, c(1:2, 6)], y)$residuals^2)
  expect_equal(modelb$crit.v, mbic(rss, n, 3, M))

  # stay
  expect_equal(backwardStep(X[, k], y, p=M, stay=3)$drop, 0)
  expect_equal(backwardStep(X, y, p=M, stay=c(1, 3:9))$drop, 10)
  expect_equal(backwardStep(X, y, p=M, stay=1:10)$drop, 0)

  # intercept
  X2 <- cbind(1, X)
  expect_equal(backwardStep(X2[, c(1:3, 7)], y, p=M)$drop, 1)
  expect_equal(backwardStep(X2[, c(1:3, 7)], y + 10, p=M)$drop, 0)
  expect_equal(backwardStep(X2[, c(1:3, 7)], y + 10, p=M)$crit.v,
               backwardStep(X2[, c(1:3, 7)], y + 20, p=M)$crit.v)
})

test_that("Multi-backward", {
  set.seed(1)
  n <- 100
  M <- 10
  X <- matrix(rnorm(M*n), ncol=M)
  y <- X[, 2] - X[, 3] + X[, 6] - X[, 10] + rnorm(n, sd=0.1)
  expect_equal(setdiff(1:M, multiBackward(X, y, verbose=FALSE, p=M)$drop),
               c(2,3,6,10))
  expect_equal(length(multiBackward(X, y, minb=6, verbose=FALSE, p=M)$drop),
               M-6)
  X <- cbind(1, X)
  M <- M + 1
  y <- rnorm(n) + 10
  expect_equal(length(multiBackward(X, y, verbose=FALSE, p=M)$drop), M - 1)
  y <- y - mean(y)
  expect_equal(length(multiBackward(X, y, stay=1, verbose=FALSE, p=M)$drop),
               M - 1)
  expect_equal(length(multiBackward(X, y, verbose=FALSE, p=M)$drop), M)
  results <- multiBackward(X, y, stay=c(1, 3:(M-2)), verbose=FALSE, p=M)
  expect_equal(sort(results$drop), c(2,10,11))
  expect_equal(results$stay, 1:8)
})
