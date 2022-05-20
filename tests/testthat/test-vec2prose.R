
# Setup -------------------------------------------------------------------

t0 <- character(0)
t_nc <- rnorm(3)

t1 <- "dogs"
t2 <- c("dogs", "cats")
t3 <- c("dogs", "cats", "pigs")
t3_na <- c("dogs", "cats", "pigs", NA)

# Tests -------------------------------------------------------------------

test_that("vec2prose() works", {

  # errors when not character or of zero length
  expect_error(vec2prose(t0))
  expect_error(vec2prose(t_nc))

  #works with 1 with and without oxfrod comma arg
  expect_identical(vec2prose(t1), t1)
  expect_identical(vec2prose(t1, oxford_comma = FALSE), t1)

  #works with 2 with and without oxfrod comma arg
  expect_identical(vec2prose(t2), "dogs and cats")
  expect_identical(vec2prose(t2, oxford_comma = FALSE), "dogs and cats")

  #works with 3 elements with and wo oxford comma
  expect_identical(vec2prose(t3), "dogs, cats, and pigs")
  expect_identical(vec2prose(t3, oxford_comma = FALSE), "dogs, cats and pigs")

  #na.rm works
  expect_identical(vec2prose(t3_na, warn = FALSE), "dogs, cats, and pigs")
  expect_identical(vec2prose(t3_na, na.rm = FALSE, warn = FALSE),
                   "dogs, cats, pigs, and NA")

  #warn works
  expect_warning(vec2prose(t3_na, warn = TRUE), regexp = "removed")
  expect_warning(vec2prose(t3_na, na.rm = FALSE, warn = TRUE), regexp = "retained")

})
