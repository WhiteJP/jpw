#' Return censored data
#'
#' @param x numeric vector
#' @param min numeric scalar describing the minimum value for censoring.
#' @param max numeric scalar describing the maximum value for censoring.
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

#' @describeIn censor_data left and right censor data (floor and ceiling)
#' @export
censor_both <- function(x, min, max) {
  censor_left(
    censor_right(x, max),
    min
    )
}
