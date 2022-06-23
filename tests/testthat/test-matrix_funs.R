
# Set up ------------------------------------------------------------------

x <- c(0, 0, 1,
       0, 1, 0,
       1, 0, 0)

mat1 <- matrix(x, nrow = 3)
mat9 <- mat1 * 9

x_five <- c(0, 0, 0, 0, 1,
            0, 0, 0, 1, 0,
            0, 0, 1, 0, 0,
            0, 1, 0, 0, 0,
            1, 0, 0, 0, 0)
mat_five <- matrix(x_five, nrow = 5)

# Tests -------------------------------------------------------------------

test_that("rev_diag() works", {
  expect_equal(rev_diag(3), mat)
  expect_equal(rev_diag(5), mat_five)
  expect_equal(rev_diag(3, x = 9), mat9)

  expect_error(rev_diag(-1))
  expect_error(rev_diag(1.4))
  expect_error(rev_diag(0))
})
