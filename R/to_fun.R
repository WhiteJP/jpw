### Helpers -----------------------------------------------------------------

#' Pass to function with named arguments, using dynamic dots
#'
#' @param x first argument of function, normally data
#' @param args list of other named arguments to pass to `fun`
#' @param fun function to pass `x` and `args` to.
#'
to_fun <- function(x, args, fun) {
  args <- rlang::list2(x, !!!args)
  do.call(fun, args)
}

# THere isn't really any beneft to using rlang in this situation
# the following approach does it all with `c()`, `list()`

to_fun2 <- function(x, args, fun) {
  do.call(fun, c(list(x), args))
}

#to_fun(1:10, args = list(na.rm = TRUE, trim = 0.5), "mean")
#to_fun2(1:10, args = list(na.rm = TRUE, trim = 0.5), "mean")
