#' Utility functions
#'
#' Set of simple utility functions for common use.
#'
#' @describeIn utility_funs wrapper for `any()` with na.rm = TRUE
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
is_wholenum <-function(x) {
  abs(x - round(x)) < 1e-16
  }

