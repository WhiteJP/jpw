
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

## Example

Below are some examples of functions in jpw.

-   put vectors in prose format

``` r
library(jpw)
vec2prose(c("Peanut", "Butter", "Jelly"), oxford_comma = FALSE)
#> [1] "Peanut, Butter and Jelly"
```

-   evaluate logical expressions where NAs proliferate. NA in any part
    of input -\> NA in output

``` r
x <- c(1,  2, 3, NA,  5, NA, 7,  8, NA, 10)
y <- c(NA, 9, 8,  7,  5, NA, 4,  3,  2,  1)

na2na(x > 8 | x == y)
#>  [1]    NA FALSE FALSE    NA  TRUE    NA FALSE FALSE    NA  TRUE
```
