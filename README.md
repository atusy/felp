<!-- badges: start -->
[![CRAN status](https://www.r-pkg.org/badges/version/felp)](https://cran.r-project.org/package=felp)
![Total downloads](https://cranlogs.r-pkg.org/badges/grand-total/felp)
![Monthly downloads](https://cranlogs.r-pkg.org/badges/felp)
[![R-CMD-check](https://github.com/atusy/felp/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/atusy/felp/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

# felp <img src="man/figures/logo.png" align="right" alt="" width="120" />

`felp` is a short of **f**unctional h**elp**, and provides

- fuzzy search and preview of help with `fuzzyhelp` function or "Fuzzy Search on R Help" RStudio Addin
- the `?.` pseudo-post fix operator to simultaneously display a help document
  and a structure of an object
- the `?p` pseudo-post fix operator to display document of a package

and more in [Syntax](#Syntax) and [Get started](https://felp.atusy.net/articles/felp.html)

## Installation

### From CRAN

``` r
install.packages("felp")
```

### From GitHub

``` r
source("https://install-github.me/atusy/felp")
```

## Syntax

``` r
# ? operator
?help
?utils::help

# ?. pseudo postfix operator for functions and objects
help?.
utils::help?.

# ?p pseudo postfix operator for packages
utils?p

# felp as an extention of utils::help
felp(help)
felp("help")
felp(utils::help)
felp(help, utils)
felp(package = utils)
```
