# use S3 for now - for NUMERICAL PMFS - discrete but still at least very least
# ordinal and ideally raito or interval to relaly work

# contructor
new_pmf <- function(x = data.frame(x = numeric(0), p = numeric(0))) {
  stopifnot(is.data.frame(x))
  x[[1]] <- as.numeric(x[[1]])
  x[[2]] <- as.numeric(x[[2]])
  structure(x, class = c("pmf", "data.frame"))
}

#validator - imp
# should make sure we order too
#make sure this is ok - think more about how to do checks.
validate_pmf <- function(x) {
  stopifnot(inherits(x, "data.frame")) # must be df too
  stopifnot(ncol(x) == 2) # must be two cols
  stopifnot(is.numeric(x[[1]])) # first col must be numeric
  stopifnot(is.numeric(x[[2]])) # second col must be numeric
  stopifnot(x[[2]] >= 0, x[[2]] <= 1) # no prob can be out of 0-1
  stopifnot(sum(x[[2]]) == 1) # probs must add to 1
 x
}

### user facing  wrapper for using
create_pmf <- function(x, p) {
  if(missing(p)) {
    stopifnot(is.numeric(x))
    ft <- as.data.frame(table(x)/length(x), stringsAsFactors = FALSE)
    names(ft) <- c("x", "p")
    new_pmf(ft)
  } else {
    stopifnot(length(x) == length(p))
    new_pmf(data.frame(x = x, p = p))
  }
}

# Methods for common R generics

mean.pmf <- function(pmf) {
  as.numeric(pmf$x %*% pmf$p)
}

median.pmf <- function(pmf) {
  quantile.pmf(pmf,  0.5)
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
  Q1  <- q1.pmf(pmf)
  mdn <- median(pmf)
  Q3  <- q3.pmf(pmf)
  max <- max.pmf(pmf)

  smry <- data.frame(m, sd, mdn, mad, min, Q1, Q2 = mdn, Q3, max)
  print(smry, row.names = FALSE)

  invisible(smry)
}

print.pmf <- function(pmf) {
  cat("Probability Mass Function:\n")
  print.data.frame(pmf, row.names = FALSE)
  cat("\nSome summary statistics for this PMF:\n")
  summary(pmf)

  invisible(pmf)
}




## Functions for pmfs

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

quantile.pmf <- function(pmf, prob) {
  pmf <- pmf[order(pmf$x),]
  cs <- cumsum(pmf$p)
  res <- min(which(cs > prob))

  pmf$x[[res]]
}

q1.pmf <- function(pmf) {
  quantile.pmf(pmf,  0.25)
}

q3.pmf <- function(pmf) {
  quantile.pmf(pmf,  0.75)
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
