
# Setup ------------------------------------------------------------------

tf     <- c(TRUE, FALSE)
int01 <- c(1   ,     0)
chr01 <- c("1" ,   "0")

df1 <- data.frame(a = tf)
df2 <- data.frame(a = int01)
df3 <- data.frame(a = chr01)
df123 <- bind_rows2(list(df1, df2, df3))


# Test --------------------------------------------------------------------

test_that("bind_rows2() works", {
  expect_error(dplyr::bind_rows(bind_rows(list(df1, df2, df3))))
  expect_true(
    all(is.data.frame(df123), nrow(df123) == 6)
  )
})
