f <- function(a, b, c, d) a * b * c * d
to_f <- function(a, b, c, d) {
  args_to_fun("f")
}

test_that("args_to_fun() works", {
  expect_identical(f(1, 2, 3, 4), to_f(1, 2, 3, 4))
})
