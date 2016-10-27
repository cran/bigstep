test_that("Single tests and prepare matrix", {
  set.seed(1)
  n <- 100
  M <- 9
  X <- matrix(sample(0:2, M*n, rep=TRUE), ncol=M)
  y <- X[, 2] + 0.5*X[, 3] + 2*X[, 6] + 0.1*X[, 8] + rnorm(n, sd=0.1)
  X[c(1, 5:9), 6] <- NA
  r1 <- cor(y, X[, 3])
  r2 <- cor.test(y, X[,6])
  results <- doSingleTests(X, y, verbose=FALSE)
  expect_equal(results$r[3], r1)
  expect_equal(results$t[3], r1*sqrt((n-2)/(1-r1^2)))
  expect_equal(results$r[6], as.numeric(r2$estim))
  expect_equal(results$pv[6], as.numeric(r2$p.v))
  expect_equal(order(results$pv)[1:3], c(6,2,3))
  colnames(X) <- 1:M
  expect_equal(colnames(X[, prepareMatrix(X, y, minpv=0.15, verbose=FALSE)]),
               c("6", "2", "3"))
  expect_equal(sort(colnames(X[, prepareMatrix(X, y, 1, verbose=FALSE)])),
               as.character(1:M))

  # bigdata
  Xbig <- as.big.matrix(X, shared=FALSE)
  expect_equal(doSingleTests(Xbig, y, maxp=2, verbose=FALSE)$r, results$r)
  expect_equal(colnames(Xbig[, prepareMatrix(Xbig, y, minpv=0.15,
               verbose=FALSE)]), c("6", "2", "3"))
})



