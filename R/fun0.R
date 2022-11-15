## function factory that creates a fun0() version which automatically removes narm
# must return funcit
# it will assign it in the gloabal environment which is a bit naughty.

# this only works when na.rm is explicity set
# not surehow to deal with it when it is passed through like with mean as a

#https://www.r-bloggers.com/2014/08/hijacking-r-functions-changing-default-arguments/

fun0 <- function(f, env = globalenv()){
  fun0 <- match.fun(f)
  if("na.rm" %in% as.character(formals(f))){
    stop("fun `f` does not have `na.rm as an argument`")
  }
  #get name
  if(is.function(f)) {
    fname <- as.character(substitute(f))
  } else {
    fname <- f
  }
  fname0 <- paste0(fname, "0")

  #assign
  formals(fun0)[["na.rm"]] <- TRUE
  assign(fname0, fun0, envir = env)
}
