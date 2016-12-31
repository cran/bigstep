test_that("bic and aic", {
  expect_true(bic(loglik=10, n=50, k=5) < bic(loglik=10, n=50, k=6))
  expect_error(bic(10, 0, 5), "n > 0 is not TRUE")
  expect_true(bic(loglik=10, n=50, k=5) > aic(loglik=10, n=50, k=6))
})

test_that("bic and mbic", {
  expect_true(bic(loglik=10, n=50, k=5) < mbic(loglik=10, n=50, k=5, p=50))
  expect_true(mbic(10, 50, 5, 50) < mbic(10, 50, 5, 60))
  expect_true(mbic2(10, 50, 5, 50) < mbic(10, 50, 5, 50))
  expect_equal(mbic(10, 50, 1, 50), mbic2(10, 50, 1, 50))
  expect_true(mbic2(10, 500, 100, 1e10) < mbic2(10, 500, 200, 1e10))
})

