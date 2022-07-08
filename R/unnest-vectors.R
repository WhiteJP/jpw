
# Copyright 2021 (c) Cogstate Ltd

#' Unnest vectors to data.frame
#'
#'
#' @param l list of vectors of same length, although obcsured in levels of nesting
#' @returns list of vectors without any nesting.
#'
#' @examples
#' x <- stats::runif(10)
#' y <- LETTERS[1:10]
#' l <- list(x = x, y, list(list(x, y), nums = x, y))
#' unnest_vectors(l)
#'
#' @export
unnest_vectors <- function(l){
  #initialise output list, `out` and counter `k`
  out <- list()
  k <- 0
  e <- environment()
  recursive_unlist_vectors(l, e)
}

recursive_unlist_vectors <- function(l, e){
  for(i in 1:length(l)) {
    if(is.atomic(l[[i]])) {
      e$k <- e$k + 1
      e$out[[e$k]] <- l[[i]]
    }
    else recursive_unlist_vectors(l[[i]], e)
  }
  e$out
}


##another approach to unlist is to find the first vector, and get its length.
## then unlist everything and put into dataframe. although won't have mames
# unnest_vectors2 <- function(l) {
#
#   # assuming all lists have the same length
#   recurse_n <- function(i) {
#     if(is.atomic(i)) length(i) else recurse_n(i[[1]])
#   }
#
#   as.list(as.data.frame(matrix(unlist(l), nrow = recurse_n(l))))
#
# }
