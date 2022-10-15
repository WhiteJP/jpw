
# Copyright 2021 (c) Cogstate Ltd

#' Pass arguments from caller environment to another function
#'
#'This function is useful when you want to pass all named arguments from a function
#'to another function. I've used this in the pass when you have a wrapper for another
#'function with the exact same arguments, but may be preferred to ... as it is
#'more explicit
#'
#' @param fun string referencing function name to pass arguments to.
#' @export
args_to_fun <- function(fun){
  cl <- sys.call(-1)
  cl[[1]] <- as.name(fun)
  eval.parent(cl, 2)
}
