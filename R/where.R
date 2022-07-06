#' Return environment where function is defined
#'
#' This is an adaptation of the function written by Hadley Wickham, see
#' `https://adv-r.hadley.nz/environments.html`. This version allows the user to
#' specify the function as a string, or to use with non-standard evaluation.
#'
#'
#' @param name The name of function to search for. Can be a string, or can be
#'  evaluated with non-standard evaluation.
#' @param env environment from which to start recursive search. Defaults to the
#'  caller environment taken from `rlang::caller_env()`. Note, setting the `env`
#'  argument to anything but the default will nullify the capability for
#'  non-standard evaluation, and string must then be used in `name`.
#' @returns environment where function is defined.
#' @examples
#' where(sum)
#' where("sd")
#'
#' @export
where <- function(name, env = rlang::caller_env()) {

  if(identical(env, rlang::caller_env())){
    # first case - to allow for either string or non-standard eval
    name <- rlang::enexpr(name)
    if(!is.character(name)) {
      name <- as.character(name)
    }
  }

  if (identical(env, rlang::empty_env())) {
    # Base case
    stop("Can't find ", name, call. = FALSE)
  } else if (rlang::env_has(env, name)) {
    # Success case
    env
  } else {
    # Recursive case
    where(name, rlang::env_parent(env))
  }
}
