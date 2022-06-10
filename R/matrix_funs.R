#' Get Reverse diagonal of a square matrix
#'
#' @param n number of rows in matrix. Note, this function forces a square matrix
#'  with n = p.
#' @param x value to place in reverse diagonal. defaults to 1.
#' @examples
#' rev_diag(3)
#' rev_diag(5, TRUE)
#'
#' @export
rev_diag <- function(n, x = 1) {
  if(!n | !is_wholenum(n)) {stop("`n` must be wholenumber >= 1")}
  diag(x, n)[n:1, ]
}
