#' Return censored data
#'
#' @param x numeric vector
#' @param min numeric scalar describing the minimum value for censoring.
#' @param max numeric scalar describing the maximum value for censoring.
#' @describeIn censor_data left censor data (floor)
#' @export
censor_left <- function(x, min) {
  x[x < min] <- min
  x
}

#' @describeIn censor_data right censor data (ceiling)
#' @export
censor_right <- function(x, max) {
  x[x > max] <- max
  x
}


#' @describeIn censor_data left and right censor data (floor and ceiling)
#' @export
censor_both <- function(x, min, max) {
  censor_left(
    censor_right(x, max),
    min
  )
}

# Speed Test, comparing c and r funs -----------------------------------------

# x <- rnorm(1e4)
# microbenchmark::microbenchmark(
#   "r, using if_else"  = censor_left(x, min = -1),
#   "r, using indexing" = censor_left2(x, min = -1),
#   "for loop in c" = censor_left_c(x, min = -1)
# )
# microbenchmark::microbenchmark(
#   "r, new"      = censor_both(x, min = -1, max = 1),
#   "c, for loop" = censor_both_c(x, min = -1, max = 1)
# )

