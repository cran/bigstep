test_that("Model selection", {
  # doAllSteps
  set.seed(1)
  n <- 200
  M <- 10
  X <- matrix(rnorm(M*n), ncol=M)
  colnames(X) <- 1:M
  Xm <- cbind(1, matrix(rnorm(M*n), ncol=M))
  colnames(Xm) <- c("inter", -(1:M))
  y <- X[, 2] + X[, 6] - X[, 10] + Xm[, 2] - Xm[, 5] + rnorm(n)
  Xs <- bigstep:::allSteps(X, y, Xm=Xm, stay=2, verbose=FALSE, p=M)
  expect_equal(sort(as.numeric(colnames(Xs)[-1])), c(-4, -1, 2, 6, 10))
  Xs <- bigstep:::allSteps(X, y=rnorm(n), Xm=Xm[, 1, drop=FALSE],
                              verbose=FALSE, p=M)
  expect_equal(ncol(Xs), 1)

  # selectModel
  model <- c("2", "6", "10")
  expect_equal(selectModel(X, y, verbose=FALSE, p=M), model)
  expect_equal(selectModel(X, y, verbose=FALSE, crit=bic), model)
  expect_equal(selectModel(X, y, verbose=FALSE, crit=aic), model)
  expect_equal(selectModel(X, y, multif=FALSE, verbose=FALSE, p=M), model)

  model <- selectModel(X, y, Xm=Xm, stay=c(2, 10), minpv=0.3, verbose=FALSE, p=M)
  expect_equal(sort(as.numeric(model)), c(-9, -4, -1, 2, 6, 10))
  expect_equal(length(selectModel(X, y=rnorm(n), verbose=FALSE, p=M)), 0)

  # big.matrix
  set.seed(1)
  n <- 200
  M <- 1000
  X <- matrix(rnorm(M*n), n, M)
  colnames(X) <- 1:M
  y <- rowSums(X[, c(10, 50, 200, 500, 750)]) + rnorm(n)
  library(bigmemory)
  X <- as.big.matrix(X, shared=FALSE)
  Q <- rnorm(n)
  model <- selectModel(X, y, Xm=Q, stay=1, p=M, verbose=FALSE, maxp=2000)
  expect_equal(model, c("", "500", "10", "200", "50", "750"))

  # NA
  y[20] <- NA
  X[4:6, 5:7] <- NA
  model <- selectModel(X, y, Xm=Q, stay=1, p=M, verbose=FALSE, maxp=2000)
  expect_equal(model, c("", "500", "10", "200", "50", "750"))

  # poisson
  set.seed(1)
  n <- 50
  M <- 10
  X <- matrix(runif(M*n, -1, 1), ncol=M)
  colnames(X) <- 1:M
  mu <- rowSums(X[, c(3, 4, 8)])
  y <- rpois(n, exp(mu))
  expect_equal(sort(selectModel(X, y, p=M, fitFun=fitPoisson, verbose=FALSE)),
               c("3", "4", "8"))
  # logistic
  p <- 1/(1 + exp(-3*mu))
  y <- rbinom(n, 1, p)
  expect_equal(sort(selectModel(X, y, p=M, fitFun=fitLogistic, verbose=FALSE)),
               c("3", "4", "8"))
})
