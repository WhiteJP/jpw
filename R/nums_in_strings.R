#'Dealing with strings with numbers
#'
#'@param x character vector, or other vector which will be coerced to character
#'@param trimws logical. whether to call `base::trimws()`, trimming all whitespace
#' at start of end of string before returning.
#'@describeIn nums_in_strings remove all the numbers from a string
#'@export
remove_nums <- function(x, trimws = FALSE) {
  out <- gsub("[-0-9.-]+", "", as.character(x))
  if(trimws) trimws(out) else out
}

#'@describeIn nums_in_strings extract the numbers from a string all together
#' (i.e. `12aaa34` -> `1234`)
#'@export
extract_nums_tgthr <- function(x) {
  as.numeric(gsub("[^0-9.-]+", "", as.character(x)))
}

#'@describeIn nums_in_strings extract all the separate numbers from a string.
#' returns list of numeric vectors. (i.e., 12aaa34 -> `[1] 12 [2] 34`)
#'@export
extract_nums_all <- function(x) {
  x <- as.character(x)
  gg <- gregexpr("-?[0-9]+(.[0-9]+)?", x)
  ss <- strsplit(x, "")

  stopifnot(length(ss) == length(gg))
  lapply(seq_along(ss), function(i) f(ss[[i]], gg[[i]]))

}

#' Extract scientific notation exponent
#'
#' @param x string of numbers in scientific notation
#'
extract_sci_exp <- function(x) {
  gsub(".*e", "", as.character(x))
}




# helpers ------------------------------------------------------------------

f <- function(s, g) {
  st <- g
  ml <- attr(g, "match.length")
  vapply(seq_along(st), FUN.VALUE = numeric(1), function(i) {
    as.numeric(paste(s[st[i]:(st[i]+ml[i]-1)], collapse = ""))
  })

}
