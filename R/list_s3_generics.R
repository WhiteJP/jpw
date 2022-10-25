
# Copyright 2021 (c) Cogstate Ltd

#' List all S3 generic function in package
#' 
#' @param pkgs character vector with names of packages to 
#' 
#' @export
#' @examples 
#' list_s3_generics("base")
#' 
list_s3_generics <- function(
    pkgs = c("base", "utils", "methods", "stats", "graphics", "datasets")
) {
  fs <- lapply(paste0("package:", pkgs), lsf.str)       
  out <- vapply(unlist(fs), isS3stdGeneric, logical(1))
  names(out[out])
}

