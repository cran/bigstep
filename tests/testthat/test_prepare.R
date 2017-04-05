test_that("Single tests and prepare matrix", {
  set.seed(1)
  n <- 100
  M <- 9
  X <- matrix(sample(0:2, M*n, rep=TRUE), ncol=M)
  y <- X[, 2] + 0.5*X[, 3] + X[, 6] + 0.1*X[, 8] + rnorm(n, sd=0.1)
  expect_equal(order(singleTests(X, y, fastST=TRUE, verbose=FALSE))[1:3],
               c(6, 2, 3))
  X[c(1, 5:9), 6] <- NA
  expect_equal(order(singleTests(X, y, verbose=FALSE))[1:3], c(6, 2, 3))
  colnames(X) <- 1:M
  expect_equal(colnames(X[, prepareMatrix(X, y, minpv=0.15, verbose=FALSE)]),
               c("6", "2", "3"))
  expect_equal(sort(colnames(X[, prepareMatrix(X, y, minpv=1, verbose=FALSE)])),
               as.character(1:M))

  # bigdata
  library(bigmemory)
  Xbig <- as.big.matrix(X, shared=FALSE)
  expect_equal(colnames(Xbig[, prepareMatrix(Xbig, y, minpv=0.15,
               verbose=FALSE)]), c("6", "2", "3"))
})



