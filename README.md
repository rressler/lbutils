
<!-- README.md is generated from README.Rmd. Please edit that file -->

# lbutils

<!-- badges: start -->
<!-- badges: end -->

The goal of {lbutils} is to facilitate teaching statistics using R.

## Installation

You can install the development version of {lbutils} from GitHub using
{devtools} like so:

``` r
devtools::install_github("https://github.com/rressler/lbutils")
```

## Example

This is a basic example which shows you how to solve a common problem:

``` r
library(lbutils)
lmout <- stats::lm(mpg ~ disp * cyl, data = mtcars)
lb_anovat_lm(object = lmout)
#> Analysis of Variance Table
#> 
#>        Df      SS      MS      F          P
#> Source  3  927.95 309.315 43.719 1.0778e-10
#> Error  28  198.10   7.075                  
#> Total  31 1126.05  36.324

lb_anovat_lm(object = lmout, reg_collapse = FALSE)
#> Analysis of Variance Table
#> 
#>          Df      SS     MS        F        P
#> disp      1  808.89 808.89 114.3299 0.000000
#> cyl       1   46.42  46.42   6.5609 0.016099
#> disp:cyl  1   72.64  72.64  10.2670 0.003369
#> Error    28  198.10   7.08                  
#> Total    31 1126.05  36.32
```
