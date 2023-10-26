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


#' Make an array of zeros or ones
#'
#' @rdname zeros-ones
#' @param dim dimensions of array of zeros. If scalar returns a vector. If vector input,
#'  returns `length(dim)`-dimensional array.
#' @examples
#' zeros(7)
#' ones(c(3, 4, 2))
#'
#' @export
zeros <- function(dim) {
  array(0, dim)
}


#' @rdname zeros-ones
#' @export
ones <- function(dim) {
  array(1, dim)
}
