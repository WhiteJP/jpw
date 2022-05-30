
#' Statistical calculations
#'
#' @name stats-calcs
NULL

#' @describeIn stats-calcs normalise vector to max = 1 and min = 0
#' @param x numeric vector
#' @export
normalise <- function(x) {
  y <- x/(max(x, na.rm = TRUE) - min(x, na.rm = TRUE))
  y - min(y, na.rm = TRUE)
}

#' @describeIn stats-calcs normalise vector so that max = `mx` and min = `mn`
#' @param mn numeric scalar. Minimum value of scaled output.
#' @param mx numeric scalar. Maximum value of scaled output.
#' @export
scale_minmax <- function(x, mn, mx){
  x_01 <- normalise(x)
  x_01*(mx - mn) + mn
}
