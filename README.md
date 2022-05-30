
<!-- README.md is generated from README.Rmd. Please edit that file -->

# jpw

<!-- badges: start -->
<!-- badges: end -->

Personal R package to house miscellaneous tidbits of code.

## Installation

You can install the development version of jpw from
[GitHub](https://github.com/) with:

``` r
install.packages("devtools")
devtools::install_github("WhiteJP/jpw")
```

## Examples

Below are some examples of functions in jpw.

-   put vectors in prose format

``` r
library(jpw)
vec2prose(c("Peanut", "Butter", "Jelly"), oxford_comma = FALSE)
#> [1] "Peanut, Butter and Jelly"
```

-   evaluate logical expressions where NAs proliferate. NA in any part
    of input -\> NA in output.

``` r
x <- c(1,  2, 3, NA,  5, NA, 7,  8, NA, 10)
y <- c(NA, 9, 8,  7,  5, NA, 4,  3,  2,  1)

na2na(x > 8 | x == y)
#>  [1]    NA FALSE FALSE    NA  TRUE    NA FALSE FALSE    NA  TRUE
```

-   right and left censor data

``` r
censor_left(1:10, min = 5)
#>  [1]  5  5  5  5  5  6  7  8  9 10
censor_right(1:10, max = 5)
#>  [1] 1 2 3 4 5 5 5 5 5 5
```

-   Extract or remove numbers from strings

``` r
extract_nums_tgthr(c("sdff234", "12aaa34"))
#> [1]  234 1234
extract_nums_all(c("12aaa34", "-12.4 54.3", "adfsdf.344 -1.2 4"))
#> [[1]]
#> [1] 12 34
#> 
#> [[2]]
#> [1] -12.4  54.3
#> 
#> [[3]]
#> [1] 344.0  -1.2   4.0
remove_nums(c("12aaa34",  "adfsdf.344 -1.2 4"), trimws = TRUE)
#> [1] "aaa"    "adfsdf"
```
