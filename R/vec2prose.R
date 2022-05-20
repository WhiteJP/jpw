
#' List in prose
#'
#' Takes a vector and returns in prose. Suitable for use in inline
#' text in Rmarkdown.
#'
#' @param x character vector of length > 0.
#' @param oxford_comma logical. If TRUE (default), output includes oxford comma.
#' @returns character vector of length 1.
#' @export
#' @examples
#' x <- c("dogs", "cats", "pigs")
#'
#' list_in_prose(x)
#' list_in_prose(x, oxford_comma = FALSE)
#'
#'
#'
vec2prose <- function(x, oxford_comma = TRUE){

  if(!length(x) && is.character(x)) {
    stop("x must be character vector of length > 0" )
  }

  # concatenate vector
  t <- paste(x, collapse = ", ")

  # get appropriate replacement for last ", "
  replacement <- ifelse(oxford_comma & length(x) > 2, ", and ", " and ")

  #use regex to find last ", " and replace
  sub(",\\s(?!.*,\\s)", replacement, t, perl = TRUE)

}
