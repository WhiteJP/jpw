### Helpers -----------------------------------------------------------------

#' Pass to function with named arguments, using dynamic dots
#'
#' @param x first argument of function, normally data
#' @param args list of other named arguments to pass to `fun`
#' @param fun function to pass `x` and `args` to.

to_fun <- function(x, args, fun) {
  fun <- match.fun(fun)
  f <- function(...) {
    rlang::list2(x, ...)
  }
  args <- f(!!!args)
  do.call(fun, args)
}

