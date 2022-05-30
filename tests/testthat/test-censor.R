test_that("censor_left() works", {
  expect_identical(censor_left(1:4, min = 2), c(2, 2, 3, 4))
  expect_identical(censor_left(c(1:4, NA), min = 2), c(2, 2, 3, 4, NA))
})

test_that("censor_right() works", {
  expect_identical(censor_right(1:4, max = 3), c(1, 2, 3, 3))
  expect_identical(censor_right(c(1:4, NA), max = 3), c(1, 2, 3, 3, NA))
})
