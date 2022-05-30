
#' Statistical calculations
#'
#' @name stats-calcs
NULL

#' @describeIn stats-calcs normalise vector to max = 1 and min = 0
#' @export
normalise <- function(x) {
  y <- x/(max(x, na.rm = TRUE) - min(x, na.rm = TRUE))
  y - min(y, na.rm = TRUE)
}

#' @describeIn stats-calcs normalise vector so that max = `mx` and min = `mn`
#' @export
scale_minmax <- function(x, mn, mx){
  x_01 <- normalise(x)
  x_01*(mx - mn) + mn
}
