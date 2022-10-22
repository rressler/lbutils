
<!-- README.md is generated from README.Rmd. Please edit that file -->

# lbutils

<!-- badges: start -->
<!-- badges: end -->

The goal of {lbutils} is to facilitate teaching statistics using R.

## Installation

You can install the development version of {lbutils} from GitHub using
{devtools} like so:

``` r
remotes::install_github("rressler/lbutils", build_vignettes = TRUE)
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(lbutils)
lmout <- stats::lm(mpg ~ disp * cyl, data = mtcars)
lb_anovat_lm(object = lmout)
#>       Source Df        SS         MS        F            P
#> 1 Regression  3  927.9461 309.315374 43.71925 1.077824e-10
#> 2      Error 28  198.1011   7.075038       NA           NA
#> 3      Total 31 1126.0472  36.324103       NA           NA

lb_anovat_lm(object = lmout, reg_collapse = FALSE)
#>     Source Df         SS         MS         F            P
#> 1     disp  1  808.88850 808.888498 114.32991 2.150986e-11
#> 2      cyl  1   46.41841  46.418407   6.56087 1.609895e-02
#> 3 disp:cyl  1   72.63922  72.639216  10.26697 3.369023e-03
#> 4    Error 28  198.10107   7.075038        NA           NA
#> 5    Total 31 1126.04719  36.324103        NA           NA
```
