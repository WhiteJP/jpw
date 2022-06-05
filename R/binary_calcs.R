#' Convert integer to binary string
#'
#' @param x  integer to convert to binary
#' @returns character vector
#'
int2bin <- function(x) {
  na <- which(is.na(x))
  stopifnot("`x` must be a vector of whole numbers" = is_wholenum(x[-na]))

  out <- rep(NA_character_, length(x))
  strs <- vapply(x[-na], FUN.VALUE = character(1), FUN = function(x) {
    bin_vec <- as.integer(rev(intToBits(x)))
    str0 <- paste(bin_vec,  collapse = "")
    str <- sub("0*", "", str0)
    str
  })
  out[-na] <- strs
  out

}
