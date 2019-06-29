<!-- badges: start -->
[![Travis build status](https://travis-ci.org/atusy/felp.svg?branch=master)](https://travis-ci.org/atusy/felp)
[![AppVeyor build status](https://ci.appveyor.com/api/projects/status/github/atusy/felp?branch=master&svg=true)](https://ci.appveyor.com/project/atusy/felp)
[![Codecov test coverage](https://codecov.io/gh/atusy/felp/branch/master/graph/badge.svg)](https://codecov.io/gh/atusy/felp?branch=master)
[![CRAN status](https://www.r-pkg.org/badges/version/felp)](https://cran.r-project.org/package=felp)
![CRAN downloads](https://cranlogs.r-pkg.org/badges/grand-total/felp)
<!-- badges: end -->

# felp <img src="man/figures/logo.png" align="right" alt="" width="120" />

`felp` is a short of **f**unctional h**elp**, and provides

- the `?.` pseudo-post fix operator to simultaneously display a help document
  and a structure of an object
- the `?p` pseudo-post fix operator to display document of a package

and more in [Syntax](#Syntax) and [Get started](https://felp.atusy.net/articles/felp.html)

## Installation

Copy & paste:

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
