
# Copyright 2021 (c) Cogstate Ltd

#' Get dates in prose format
#'
#'@param x character vector in yyyy-mm-dd format.
#'@param full_month logical whether to return full name of month or
#' shortened version (e.g,. "Oct")
#'
#'@returns characater vector of dates in prose format (e.g., 21 May 1991)
#'
#'@export
date_prose <- function(x, full_month = TRUE) {
  fmt <- if(full_month) "%d %B %Y" else "%d %b %Y"
  format(x, fmt)
}

#'Get today's date in prose format
#'
#'@inheritParams date_prose
#'@examples
#'today()
#'
#'@export
today <- function(full_month = TRUE) {
  date_prose(Sys.Date(), full_month = full_month)
}
