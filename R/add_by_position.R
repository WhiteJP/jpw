#' Add element to vector by position
#'
#' @param x vector
#' @param y new element(s) to add to vector. Must be coercible to type of `x`
#' @param i numeric scalar. Index where to add new elements.
#'  Censored to min = 1 and max = length(x) + 1. Thus to add elements at end of
#'  vector use `Inf`
#' @examples
#' add_by_position(LETTERS[1:5], "NEW", 3)
#'
#' @export
add_by_position <- function(x, y, i) {
  n <- length(x)
  i <- censor_both(i, 1, n + 1)
  out <- c(x[seq_len(i - 1)], y, if(i == n + 1) NULL else x[seq(i, n)])
  out

}
