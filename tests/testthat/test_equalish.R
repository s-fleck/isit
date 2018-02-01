context("equalish")


test_that("equalish works as expected", {
  
  a <- 10
  b <- 11
  
  expect_true(equalish_frac(a, b, tolerance = 0.1))
  expect_true(equalish_frac(-a, -b, tolerance = 0.1))
  expect_false(equalish_frac(a, -b, tolerance = 0.1))
  expect_false(equalish_frac(-a, b, tolerance = 0.1))
  expect_false(equalish_frac(a, b + 1, tolerance = 0.1))
  expect_false(equalish_frac(a -1 , b, tolerance = 0.1))
  expect_true(reldiff(a, b) < 0.1)
  expect_identical(reldiff(b, a), reldiff(a, b))
  expect_identical(reldiff(b, a), reldiff(-a, -b))
  expect_false(reldiff(-a, b) < 0.1)

  expect_true(all(equalish_frac(
    c(0, 2, 3),
    c(0, 2.1, 3.1),
    tolerance = 0.1
  )))
})
