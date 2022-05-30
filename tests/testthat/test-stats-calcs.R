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

