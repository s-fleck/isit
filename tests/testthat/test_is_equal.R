context("is_equal")


test_that("is_equal works as expected", {
  
  a = 0.58 
  b = 0.08
  
  expect_false(a - b == 0.5)
  expect_true(isit::is_equal(a-b, 0.5))
  expect_false(isit::is_equal(a-b, 0.5001))

})
