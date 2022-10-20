
#' Capitalise first letter of each element of vector
#'
#' @param x character vector
#'
#' @export
capitalise_first_letter <- function(x, rest_down = TRUE) {
  substr(x, 1, 1) <- toupper(substr(x, 1, 1))
  if(rest_down) {
    n <- nchar(x)
    substr(x, 2, n) <- tolower(substr(x, 2, n))
  }
  x
}

clean_name <- function(x) {
  x <- gsub("[[:punct:]]+|\\s+|\\s$", " ", x)
  x <- gsub("\\s$", "", x)
  jpw::capitalise_first_letter(x)
}
