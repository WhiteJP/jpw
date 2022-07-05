
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


#' Get AUC-based effect sizefor wilcoxon rank sum test
#'
#' @param x dataframe output from `rstatix::wilcox_test()`
wilcox_AUC <- function(x) {
  unname(x[['statistic']]/(x$n1*x$n2))
}

#' Get M (SD) from numeric vector
#'
#' @describeIn stats-calc
#' @param x numeric vector
#' @param digits numeric vector of length 2. Digits to round M and SD to
#'  (in that order). Defaults to `c(2, 2)`
#' @param na.rm. logical. whether to remove `NA`s before calculating M and SD.
#' @returns string of form "M (SD)"
#'
#' @export
msd_label <- function(x, digits = c(2, 2), na.rm = TRUE) {
  m <- mean(x, na.rm = na.rm)
  sdev <- sd(x, na.rm = na.rm)

  paste(
    "M (SD) =",
    round(m, digits[1]),
    jpw::brackets(round(sdev, digits[2]))
  )
}
