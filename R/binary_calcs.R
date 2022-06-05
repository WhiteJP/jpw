#' Convert integer to binary string
#'
#' @param x  integer to convert to binary
#' @returns character vector
#'
#'@export
int2bin <- function(x) {
  na <- is.na(x)
  stopifnot("`x` must be a vector of whole numbers" = is_wholenum(x[!na]))

  out <- rep(NA_character_, length(x))
  strs <- vapply(x[!na], FUN.VALUE = character(1), FUN = function(x) {
    bin_vec <- as.integer(rev(intToBits(x)))
    str0 <- paste(bin_vec,  collapse = "")
    str <- sub("0*", "", str0)
    str[nchar(str) == 0] <- "0"
    str
  })
  out[!na] <- strs
  out

}

#TODO, add in na in na out.
#' Add trailing zeros
#'
#' @param x vector to add trailing zeros
#' @return character vector with traling zeros
#'
#' @export
add_zeros <- function(x, len = max(nchar(x), na.rm = TRUE)) {
  n <- nchar(x)
  n0 <- len - n
  z <- vapply(n0, FUN.VALUE = character(1),
              FUN = function(n0) paste(rep("0",  n0), collapse = ""))

  paste0(z, x)

}
