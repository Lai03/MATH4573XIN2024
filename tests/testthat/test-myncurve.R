test_that("myncurve works", {

  expect_equal(myncurve(mu=10,sigma=5, a=6)$area, 0.2119)
  expect_equal(myncurve(mu=10,sigma=5, a=6)$mu, 10)
  expect_equal(myncurve(mu=10,sigma=5, a=6)$sigma, 5)
})
