# use S3 for now - for NUMERICAL PMFS - discrete but still at least very least
# ordinal and ideally raito or interval to relaly work

# contructor
new_pmf <- function(x = data.frame(x = numeric(0), p = numeric(0))) {
  x <- as.data.frame(x)
  colnames(x) <- c("x", "p")
  x <- x[order(x[[1]]),]
  structure(x, class = c("pmf", "data.frame"))
}

# Validator
# TODO give better error messages
validate_pmf <- function(x) {
  stopifnot(inherits(x, "data.frame")) # must be df too
  stopifnot(ncol(x) == 2) # must be two cols
  stopifnot(is.numeric(x[[1]])) # first col must be numeric
  stopifnot(is.numeric(x[[2]])) # second col must be numeric
  stopifnot(x[[2]] >= 0 & x[[2]] <= 1) # no prob can be out of 0-1
  stopifnot(sum(x[[2]]) == 1 | !length(x[[2]])) # probs must add to 1 length(x[[2]]))
                        # also allow it to add to zero so we can create empty pmf
  stopifnot(!is.unsorted(x[[1]])) # must be sorted
 x
}


#' Create pmf object
#'
#' @param x either a sample of values from which to estimate a PMF based on frequency
#'   if `p` is missing, or unique `x` values with probability `p`
#' @param the probabilty of each unqiue value of `x` occuring
#'
#' @returns `data.frame` of class `pmf`
#'
#' @export
#' @examples
#' samp <- sample(c(-1000, 100, 800), size = 1e4, replace = TRUE)
#' pmf(samp)
#' pmf(x = c(-1000, 100, 800), p = c(0.25, 0.5, 0.25))

pmf <- function(x, p) {
  if(missing(p)) {
    stopifnot(is.numeric(x))
    ft <- as.data.frame(table(x)/length(x), stringsAsFactors = FALSE)
    ft[[1]] <- as.numeric(ft[[1]])
    validate_pmf(new_pmf(ft))
  } else {
    stopifnot(length(x) == length(p))
    validate_pmf(new_pmf(data.frame(x = x, p = p)))
  }
}

# Methods for common R generics

mean.pmf <- function(pmf) {
  as.numeric(pmf$x %*% pmf$p)
}

median.pmf <- function(pmf) {
  quantile(pmf,  0.5)
}

quantile.pmf <- function(pmf, prob) {
  if(missing(prob)) {
    stop("Must supply `prob` argument to quantile function")
  }
  if(!is.unsorted(pmf$x)) {
    stop("PMF must be sorted. Creating one with pmf() will ensure this.")
  }
  cs <- cumsum(pmf$p)
  res <- min(which(cs > prob))
  pmf$x[[res]]
}

plot.pmf <- function(pmf, title = "", colour = "lightblue") {
  requireNamespace("ggplot2")
  p <- pmf |>
    ggplot2::ggplot(aes(x = x, y = p)) +
    ggplot2::geom_bar(stat = "identity", col = "black", fill = colour) +
    ggplot2::labs(
      title = title,
      x = "Outcome ($)",
      y = "Probability"
    ) +
    theme_minimal()
  return(p)
}

# make it so that when you assign it doesn't return.
# need to properly figure that out.
summary.pmf <- function(pmf, ...) {

  m   <- mean(pmf)
  sd  <- sd.pmf(pmf)
  mad <- mad.pmf(pmf, ...)
  min <- min.pmf(pmf)
  Q1  <- q1(pmf)
  mdn <- median(pmf)
  Q3  <- q3(pmf)
  max <- max.pmf(pmf)

  smry <- data.frame(m, sd, mdn, mad, min, Q1, Q2 = mdn, Q3, max)
  print(smry, row.names = FALSE)

  invisible(smry)
}


# could improve this so that it prints in columns when it is long
# could see how it is done for print.data.frame and improve.
print.pmf <- function(pmf) {
  cat("Probability Mass Function:\n")
  print.data.frame(pmf, row.names = FALSE)
  #cat("\nSome summary statistics for this PMF:\n")
  #summary(pmf)

  invisible(pmf)
}


## Functions
#pop var
var.pmf <- function(pmf) {
  s <- (pmf$x - mean(pmf))^2
  ws <- s %*% pmf$p
  as.numeric(ws)

}

sd.pmf <- function(pmf) {
  sqrt(var.pmf(pmf))
}

# Do i need a scaling factor like in stats::mad
# add it in for now but keep degault as one
mad.pmf <- function(pmf, center = median(pmf), constant = 1) {
  absdev <- abs(pmf$x - center)
  mad <- median(
    new_pmf(data.frame(x = absdev, p = pmf$p))
    )
  mad * constant
}

min.pmf <- function(pmf) {
  min(pmf$x)
}

max.pmf <- function(pmf) {
  max(pmf$x)
}

sample.pmf <- function(pmf, n = 1e6) {
  sample(pmf$x, size = n, replace = TRUE, prob = pmf$p)
}


#examples
# x <- new_pmf(data.frame(x = c(100, 200, 400), p = c(0.5, 0.4, 0.1)))
#
# mean(x)
# var.pmf(x)
# sd.pmf(x)
# quantile(x, prob = .12)
# min.pmf(x)
# q1.pmf(x)
# median(x)
# q3.pmf(x)
# max.pmf(x)
