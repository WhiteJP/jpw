test_that("normalise() works ", {
  expect_equal(
    range(normalise(1:100)),
    range(0, 1)
    )
  expect_equal(
    range(normalise(10:100, 0, 100)),
    range(.1, 1)
  )
  expect_equal(
    range(normalise(c(1:100, NA)), na.rm = TRUE),
    range(0, 1)
    )
})

test_that("scale_min_max works ", {
  expect_equal(
    range(scale_minmax(1:100, out_min = -123, out_max = 8889)),
    range(-123, 8889)
    )
  expect_equal(
    range(scale_minmax(c(1:100, NA), out_min = -123, out_max = 8889), na.rm = TRUE),
    range(-123, 8889)
  )
  expect_equal(
    range(scale_minmax(4:10, in_min = 0, in_max = 10, out_min = 0, out_max = 100)),
    range(40, 100)
  )
})

test_that("log10() works ", {
  expect_equal(
    log(0:10, base = 10),
    log10(0:10)
  )
  expect_warning(
    log10(-5)
  )
  expect_error(
    log10(NULL)
  )
  expect_error(
    log10("hello")
  )
})

test_that("as_numeric2 works ", {
  expect_equal(
    as_numeric2(factor(1:4 *100)),
    1:4 * 100
  )
  #warning when trying to convert things that can't be coerced
  expect_warning(
    as_numeric2(factor(LETTERS)),
  )
  expect_equal(
    as_numeric2(c("100", "200", "300", "400")),
    1:4 * 100
  )
  # wont work for logical values stored as a factor
  # this is consistent with trying to convert to double from
  # a character vector of "TRUE", "FALSE"
  expect_warning(
    as_numeric2(factor(c(TRUE, FALSE)))
  )
})


