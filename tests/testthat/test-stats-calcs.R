test_that("normalise() works ", {
  expect_equal(
    range(normalise(1:100)),
    range(0, 1)
    )
  expect_equal(
    range(normalise(c(1:100, NA)), na.rm = TRUE),
    range(0, 1)
    )
})


test_that("scale_min_max works ", {
  expect_equal(
    range(scale_minmax(1:100, mn = -123, mx = 8889)),
    range(-123, 8889)
    )
  expect_equal(
    range(scale_minmax(c(1:100, NA), mn = -123, mx = 8889), na.rm = TRUE),
    range(-123, 8889)
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


