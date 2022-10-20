#
# #temporarily assign g in random parent_env
# e <- rlang::caller_env(ceiling(runif(1, 0, 20)))
# assign("g", function() 10, envir = e)
#
# test_that("where() works", {
#   expect_equal(where(g), e)
#   expect_equal(where("g"), e)
#
# })
#
# #remove f fom new env
# rm("g", envir = e)
