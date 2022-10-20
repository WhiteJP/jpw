
# Copyright 2021 (c) Cogstate Ltd

conditionally_require_namespace <- function(pkg, attach = FALSE, ...) {
  is_installed <- requireNamespace(pkg, ..., quietly = TRUE)
  if(!is_installed) {
    stop(
      bt(pkg), "is not installed on this machine, ",
      "but is necessary for this functionality. ",
      "Please install and try again."
    )
  }
  if(attach) {
    library(pkg, character.only = TRUE)
    message("Attaching package: ", bt(pkg))
  }
}


