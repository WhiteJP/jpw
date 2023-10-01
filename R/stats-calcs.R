
#' Statistical calculations
#'
#' Various stastical calculations.
#'
#' @name stats-calcs
NULL

#' @describeIn stats-calcs normalise vector to scale [0, 1]
#' @param x numeric vector
#' @param in_min minimum possible value of input, defaults to min(x, na.rm = TRUE)
#' @param in_max maximum possible value of input, defaults to max(x, na.rm = TRUE)
#' @export
normalise <- function(
    x,
    in_min = min(x, na.rm = TRUE),
    in_max = max(x, na.rm = TRUE)
) {
  x/(in_max - in_min) - (in_min/(in_max - in_min))
}

#' @describeIn stats-calcs normalise vector so that max = `mx` and min = `mn`
#' @param x
#' @param out_min numeric scalar. Minimum value of scaled output.
#' @param out_max numeric scalar. Maximum value of scaled output.
#' @export
scale_minmax <- function(
    x,
    out_min,
    out_max,
    in_min = min(x, na.rm = TRUE),
    in_max = max(x, na.rm = TRUE)
){
  x_01 <- normalise(x, in_min, in_max)
  x_01*(out_max - out_min) + out_min
}


#' Get AUC-based effect sizefor wilcoxon rank sum test
#'
#' @param x dataframe output from `rstatix::wilcox_test()`
wilcox_AUC <- function(x) {
  unname(x[['statistic']]/(x$n1*x$n2))
}

#' Get M (SD) from numeric vector
#'
#' @param x numeric vector
#' @param digits numeric vector of length 2. Digits to round M and SD to
#'  (in that order). Defaults to `c(2, 2)`
#' @param na.rm logical. whether to remove `NA`s before calculating M and SD.
#' @param label logical. whether to prepend "M (SD) = " to string
#'
#' @returns string of form "M (SD)"
#'
#' @export
msd_label <- function(x, digits = c(2, 2), na.rm = TRUE, label = TRUE) {
  m <- mean(x, na.rm = na.rm)
  sdev <- stats::sd(x, na.rm = na.rm)

  paste(
    if(label) "M (SD) =" else "",
    round(m, digits[1]),
    jpw::brackets(round(sdev, digits[2]))
  )
}

#' @describeIn stats-calcs Find Maximum z score given sample size n
#' @param n numeric scalar. sample size.
#' @seealso https://www.tandfonline.com/doi/abs/10.1080/00031305.1988.10475530
#' @export
max_z_given_n <- function(n) {
  stopifnot("n must be numeric scalar" = length(n) == 1 && is.numeric(n))
  (n - 1)/ sqrt(n)
}

#' @describeIn stats-calcs Calculate standard error of mean
#' @export
se <- function(x, ...) {
  sqrt(stats::var(x, ...)/length(x))
}

#' @describeIn stats-calcs get r squared from stats::lm()
#'
#' @param lm_mod model output from stats::lm
#' @param adjusted logical. Whether to return adjusted R (`TRUE`), or non-adjusted
#'  (`FALSE`). Defaults `FALSE`
#' @export
lm_r2 <- function(lm_mod, adjusted = FALSE) {
  stopifnot("`lm_mod` must be of class `lm`" = inherits(lm_mod, "lm"))
  msum <-stats::summary.lm(lm_mod)
  if(adjusted) msum$adj.r.square else msum$r.square
}

#' @describeIn stats-calcs get median absolute deviation
#' @export
mad <- function(x, na.rm = TRUE) {
  abs_dev <- abs(x - stats::median(x, na.rm = na.rm))
  stats::median(abs_dev)
}


#' @describeIn stats-calcs Log(x, base = 10) wrapper
#' @export
log10 <- function(x){
  log(x, base = 10)
}

#' @describeIn stats-calcs symmetrical setdiff()
#' @export
sym_setdiff <- function(a,b) c(setdiff(a,b), setdiff(b,a))


#' @describeIn stats-calcs quantile wrapper for Q1
#' @export
q1 <- function(x) {
  quantile(x,  0.25)
}

#' @describeIn stats-calcs quantile wrapper for Q3
#' @export
q3 <- function(pmf) {
  quantile(pmf,  0.75)
}

#' @describeIn estimate standard error of estimate p value from Fisherian randomization inference
#' @param p_est,k estimated p value from Fisherian randomization inference from K iterations
#' @export
p_se <- function(p_est, k) {
  sqrt(p_est*(1-p_est)/k)
}


