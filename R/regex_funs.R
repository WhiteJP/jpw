
# Copyright 2021 (c) Cogstate Ltd

which_regexp <- function(vals, regexs) {
  m <- lapply(regexs, function(r) grep(r, vals, ignore.case = TRUE))
  as.integer(utils::stack(stats::setNames(m, seq_along(m)))[,1])
}
