
# Setup -------------------------------------------------------------------

t0 <- character(0)
t_nc <- rnorm(3)
t1 <- "dogs"
t2 <- c("dogs", "cats")
t3 <- c("dogs", "cats", "pigs")

# Tests -------------------------------------------------------------------

test_that(" list_in_prose() works", {

  # errors when not character or of zero length
  expect_error(list_in_prose(t0))
  expect_error(list_in_prose(t_nc))

  #works with 1 with and without oxfrod comma arg
  expect_identical(list_in_prose(t1), t1)
  expect_identical(list_in_prose(t1, oxford_comma = FALSE), t1)

  #works with 2 with and without oxfrod comma arg
  expect_identical(list_in_prose(t2), "dogs and cats")
  expect_identical(list_in_prose(t2, oxford_comma = FALSE), "dogs and cats")

  #works with 3 elements with and wo oxford comma
  expect_identical(list_in_prose(t3), "dogs, cats, and pigs")
  expect_identical(list_in_prose(t3, oxford_comma = FALSE), "dogs, cats and pigs")

})
