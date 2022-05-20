
#' List in prose
#'
#' Takes a vector and returns prose listing vector elements. Suitable for use in
#' inline text in Rmarkdown.
#'
#' @param x character vector of length > 0 (after removing `NA`s if na.rm = TRUE)
#' @param oxford_comma logical. If TRUE (default), output includes oxford comma.
#' @param na.rm logical. If TRUE (Default), remove all `NA` from output.
#' @param warn logical. If TRUE (Default), warn about NAs occuring in dataste
#' @returns character vector of length 1.
#' @export
#' @examples
#' x <- c("dogs", "cats", "pigs")
#'
#' list_in_prose(x)
#' list_in_prose(x, oxford_comma = FALSE)
#'
vec2prose <- function(x, oxford_comma = TRUE, na.rm = TRUE, warn = TRUE){

  # setup and checks
  if(!is.character(x)) stop("`x` must be character vector")  # char vec check
  x0 <- if(na.rm) x[!is.na(x)] else x # deal with NAs
  if(!length(x0)) stop("`x` must be of length > 0" ) #length check

  # make warning
  if(warn && anyNA(x)){
    wrn <- if(na.rm) {
      "`NA` values in `x` were removed. see `na.rm` argument. "
    }  else {
      "`x` contains `NA` values retained in the output. see `na.rm`"
    }
    warning(wrn)
  }

  # collapse vector
  t <- paste(x0, collapse = ", ")

  # get appropriate replacement for last ", "
  replacement <- ifelse(oxford_comma & length(x0) > 2, ", and ", " and ")

  #use regex to find last ", " and replace
  sub(",\\s(?!.*,\\s)", replacement, t, perl = TRUE)

}
