a <- paste("this is a sentence with Multiple Capital LETTERS, Throw some numbers",
            "(24, 19, 19.33) and symbols (%$@^) in there too")
b <- paste("This is a sentence with multiple capital letters, throw some numbers",
            "(24, 19, 19.33) and symbols (%$@^) in there too")
c <- paste("This is a sentence with Multiple Capital LETTERS, Throw some numbers",
            "(24, 19, 19.33) and symbols (%$@^) in there too")

test_that("capitalise_first_letter() works", {
  expect_equal(capitalise_first_letter(a), b)
  expect_equal(capitalise_first_letter(a, rest_down = FALSE), c)
})
