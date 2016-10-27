test_that("Model selection", {
  # doAllSteps
  set.seed(1)
  n <- 200
  M <- 10
  X <- matrix(rnorm(M*n), ncol=M)
  Xm <- matrix(rnorm(M*n), ncol=M)
  y <- X[, 2] + X[, 6] - X[, 10] + Xm[, 2] - Xm[, 5] + rnorm(n, sd=0.1)
  res <- doAllSteps(X, y, Xm, stay=c(1,8), verbose=FALSE, p=M)
  expect_equal(res$X, c(2, 6, 10))
  expect_equal(res$Xm, c(1, 2, 5, 8))
  res <- doAllSteps(X, y, Xm, stay=c(1,8), ord=c(5, 8, 10), verbose=FALSE, p=M)
  expect_equal(res$X, 10)
  expect_equal(res$Xm, c(1, 2, 5, 8))

  # selectModel
  expect_equal(names(coef(selectModel(X, y, verbose=FALSE, p=M))),
               c("intercept", "X2", "X6", "X10"))
  expect_equal(names(coef(selectModel(X, y, verbose=FALSE, crit=bic))),
               c("intercept", "X2", "X6", "X10"))
  expect_equal(names(coef(selectModel(X, y, verbose=FALSE, crit=aic))),
               c("intercept", "X2", "X6", "X10", "X5"))
  expect_equal(names(coef(selectModel(X, y, multif=FALSE, verbose=FALSE,
               p=M))), c("intercept", "X2", "X6", "X10"))
  expect_equal(names(coef(selectModel(X, y, minpv=0.5, intercept=FALSE,
               verbose=FALSE, p=M))), c("X2", "X6", "X10"))
  names <- c("Xm1", "Xm2", "Xm5", "Xm8", "X2", "X6", "X10")
  expect_equal(names(coef(selectModel(X, y, Xm, stay=c(1,8), minpv=0.3,
               intercept=FALSE, verbose=FALSE, p=M))), names)
  expect_equal(names(coef(selectModel(X, y, Xm, stay=c(1,8), minpv=0.3,
               intercept=TRUE, verbose=FALSE, p=M))), c("intercept", names))
  colnames(X) <- paste0("Z", 1:M)
  expect_equal(names(coef(selectModel(X, y, minpv=0.5, intercept=FALSE,
               verbose=FALSE, p=M))), c("Z2", "Z6", "Z10"))

  # stupid data
  expect_error(selectModel(y, y, verbose=FALSE, p=M),
               "X has to be a matrix, data.frame or big.matrix.")

  # big.matrix
  set.seed(1)
  n <- 200
  M <- 1000
  X <- matrix(rnorm(M*n), n, M)
  y <- rowSums(X[, c(10, 50, 200, 500, 750)]) + rnorm(n)
  X <- as.big.matrix(X, shared=FALSE)
  Q <- rnorm(n)
  model <- selectModel(X, y, Xm=Q, stay=1, p=M, verbose=FALSE, maxp=2000)
  expect_equal(names(coef(model)),
               c("intercept", "Xm1", "X500", "X10", "X200", "X50", "X750"))
})
