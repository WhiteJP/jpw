## Utility funs to use

#wrapper for any( ..., na.rm)
any_narm <- function(...) {
  any(..., na.rm = TRUE)
}
