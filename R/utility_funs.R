## Utility funs to use

#wrapper for any( ..., na.rm)
any0 <- function(...) {
  any(..., na.rm = TRUE)
}
