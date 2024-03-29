#' Utility functions
#'
#' Set of simple utility functions for common use.
#'
#' @name utility_funs
#' @param a numeric vector
NULL

#' @describeIn utility_funs wrapper for `any()` with na.rm = TRUE
#' @param ... logical expression passed to `any()` or `all()`
#'
#' @export
any0 <- function(...) {
  base::any(..., na.rm = TRUE)
}

#' @describeIn utility_funs wrapper for `all()` with na.rm = TRUE
#' @export
all0 <- function(...) {
  base::all(..., na.rm = TRUE)
}

#' @describeIn utility_funs replace `NULL` with default value
#' @param x vector/list to check for `NULL`
#' @param y value to replace `NULL`s
#' @export
`%||%` <- function(x, y) if (is.null(x)) y else x


#' Evaluate logical expressions, where NAs proliferate (any NA in = NA out)
#'
#' @param x logical expression
#' @returns logical vector evaluating expression x, but where all NAs in input
#'  proliferate in output
#' @export
na2na <- function(x) {

  e <- rlang::caller_env()
  ex <- rlang::enexpr(x)
  syms <- all.vars(ex)

  m <- as.matrix(as.data.frame(lapply(syms, function(x) get(x, envir = e))))
  na_rows <- apply(m, 1, function(x) any(is.na(x)))

  out <- eval(ex, envir = e)
  out[na_rows] <- NA
  out
}

#' @describeIn utility_funs check if values are whole numbers
#' @export
is_wholenum <-function(a) {
  abs(a - round(a)) < 1e-16
  }

#' @describeIn utility_funs enclose string in brackets.
#' @param type string. type of bracket to use. Defaults to "smooth".
#' @export
brackets <- function(a, type = c("smooth", "square", "squiggly")) {
  type <- match.arg(type)
  open <- switch(type,
                 smooth   = "(",
                 square   = "[",
                 squiggly = "{")
  close <- switch(type,
                  smooth   = ")",
                  square   = "]",
                  squiggly = "}")

  paste0(open, as.character(a), close)

}

#

#' As.numeric but also works for factors
#'
#' Wrapper for as.numeric that works the same as as.numeric for everything
#' except factors for which it returns the levels of the factor (if it can),
#' rather than the underlying integers
#'
#' Should work fine as the only (base) methods for as.numeric are
#'  difftime and POSIXlt which don't also have factor as class. Could be problematic
#'  if some class inherits factor, but then provides their own as.numeric method,
#'  which this function would override. could of course get function to check for this
#'  and to avoid, but think it is very unlikely.
#'
#' @param x vector to convert to double.
#' @export
as_numeric2 <- function(x) {
  if(is.factor(x)) {
    as.double(levels(x)[x])
  } else {
    as.double(x)
  }
}
