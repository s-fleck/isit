context("equalish")



test_that("is_equal works as expected", {
  a <- 0.58 
  b <- 0.08
  
  expect_false(a - b == 0.5)
  expect_true(equalish(a - b, 0.5))
  expect_false(equalish(a - b, -0.5))
  expect_true(equalish(-(a - b), -0.5))
  expect_false(equalish(a - b, 0.5001))
})




test_that("equal_or_na works as expected", {
  x <- c(NA,  "a", "b", "c", NA)
  y <- c("a", "a", "c", "c", NA)
  
  expect_identical(
    equal_or_na(x, y),
    c(FALSE, TRUE, FALSE, TRUE, TRUE)
  )
})
