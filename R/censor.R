#' Return censored data
#'
#' @param x numeric vector
#' @param min/max numeric scalar describing the min/max for censoring.
#' @describeIn censor_data left censor data (floor)
#' @export
censor_left <- function(x, min) {
  ifelse(x > min, x, min)
}


#' @describeIn censor_data right censor data (ceiling)
#' @export
censor_right <- function(x, max) {
  ifelse(x < max, x, max)
}
