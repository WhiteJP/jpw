
# Set up ------------------------------------------------------------------

x <- LETTERS[1:5]
x_start  <- c("NEW", x)
x_end    <- c(x, "NEW")
x_penult <- c(x[1:4], "NEW", x[5])
x_third <- c(x[1:2], "NEW", x[3:5])

news <- c("NEW_A", "NEW_B")
x2_start  <- c(news, x)
x2_end    <- c(x, news)
x2_penult <- c(x[1:4], news, x[5])
x2_third <- c(x[1:2], news, x[3:5])


# Tests --------------------------------------------------------------------


test_that("add_by_position() works", {

  # at start when should be
  expect_equal(add_by_position(x, "NEW", -1), x_start)
  expect_equal(add_by_position(x, "NEW", 0), x_start)
  expect_equal(add_by_position(x, "NEW", 1), x_start)

  # at end when should be
  expect_equal(add_by_position(x, "NEW", 6), x_end)
  expect_equal(add_by_position(x, "NEW", Inf), x_end)

  # at second last when should be
  expect_equal(add_by_position(x, "NEW", 5), x_penult)

  # in middle
  expect_equal(add_by_position(x, "NEW", 3), x_third)

  ## ABOVE WORKS FOR VECS OF LENGTH > 1 TOO
  # at start when should be
  expect_equal(add_by_position(x, news, -1), x2_start)
  expect_equal(add_by_position(x, news, 0), x2_start)
  expect_equal(add_by_position(x, news, 1), x2_start)

  # at end when should be
  expect_equal(add_by_position(x, news, 6), x2_end)
  expect_equal(add_by_position(x, news, Inf), x2_end)

  # at second last when should be
  expect_equal(add_by_position(x, news, 5), x2_penult)

  # in middle
  expect_equal(add_by_position(x, news, 3), x2_third)

})
