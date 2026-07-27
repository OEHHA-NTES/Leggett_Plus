test_that("trapz_internal returns 0 for fewer than 2 points", {
  expect_equal(trapz_internal(numeric(0), numeric(0)), 0)
  expect_equal(trapz_internal(1, 1), 0)
})

test_that("trapz_internal computes the trapezoidal area under a constant function", {
  expect_equal(trapz_internal(x = 0:10, y = rep(2, 11)), 20)
})

test_that("trapz_internal computes the trapezoidal area under a linear ramp", {
  # Area under y = x from 0 to 10 is 10^2 / 2 = 50
  expect_equal(trapz_internal(x = 0:10, y = 0:10), 50)
})

test_that("trapz_internal is invariant to input order", {
  x <- c(3, 1, 2)
  y <- c(30, 10, 20)
  expect_equal(trapz_internal(x, y), trapz_internal(rev(x), rev(y)))
})
