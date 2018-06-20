context("all_are")


test_that("is_candidate_key works as expected", {
  
  td <- data.frame(
    x = 1:10,
    y = 1:2,
    z = 1:5
  )
  
  expect_true(is_candidate_key(td$x))
  expect_false(is_candidate_key(td$y))
  expect_false(is_candidate_key(td$z))
  
  expect_false(is_candidate_key(c(td$x, NA)))
  expect_false(is_candidate_key(c(td$x, Inf)))
  expect_true(is_candidate_key(c(td$x, "a")))
  
  expect_true(is_candidate_key(td[, c("y", "z")]))
  expect_true(is_candidate_key(list(td$x, td$z)))
  expect_true(is_candidate_key(list(td$x)))
  expect_false(is_candidate_key(list(td$y)))
  expect_false(is_candidate_key(list(c(td$y, NA))))
})
