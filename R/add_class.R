#' Add a new class to object in addition to existing classes
#'
#' @param x object to add class to
#' @param class string. class to add.
#' @param position the position in which to add the class
#' @returns object 'x' but with  new class added
#' @examples
#' x <- add_class(tibble::tibble(), "NEW", 1)
#' class(x)
#'
#' @export
add_class <- function(x, class, position = 1) {
  c0 <- class(x)
  class(x) <- add_by_position(c0, class, position)
  x
}






