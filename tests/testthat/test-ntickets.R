test_that("ntickets works", {

  expect_equal(nticket(N=400,gamma = 0.02, p = 0.95)$nd, 412)
  expect_equal(nticket(N=400,gamma = 0.02, p = 0.95)$nc, 412.0152)
  expect_equal(nticket(N=400,gamma = 0.02, p = 0.95)$N, 400)
  expect_equal(nticket(N=400,gamma = 0.02, p = 0.95)$p, 0.95)
  expect_equal(nticket(N=400,gamma = 0.02, p = 0.95)$gamma, 0.02)
})
