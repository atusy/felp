<!-- badges: start -->
[![CRAN status](https://www.r-pkg.org/badges/version/felp)](https://cran.r-project.org/package=felp)
![Total downloads](https://cranlogs.r-pkg.org/badges/grand-total/felp)
![Monthly downloads](https://cranlogs.r-pkg.org/badges/felp)
[![R-CMD-check](https://github.com/atusy/felp/actions/workflows/R-CMD-check.yaml/badge.svg)](https://github.com/atusy/felp/actions/workflows/R-CMD-check.yaml)
<!-- badges: end -->

# felp <img src="man/figures/logo.png" align="right" alt="" width="120" />

`felp` is a short of **f**unctional h**elp**, and provides

- [fuzzy search and preview of help](#fuzzy-search-and-preview-of-help)
    - with `fuzzyhelp` function or "Fuzzy Search on R Help" RStudio Addin
- [Enhanced alternatives of base features](#enhanced-alternatives-of-base-features)
    - `?.` pseudo-postfix operator to simultaneously display a help document
      and a structure of an object
    - `?p` pseudo-postfix operator to display document of a package
    - and more

## Installation

### From CRAN

``` r
install.packages("felp")
```

### From GitHub

``` r
source("https://install-github.me/atusy/felp")
```

## Features

### Fuzzy search and preview of help

with `felp::fuzzyhelp()` or "Fuzzy Search on R Help" RStudio Addin:

![Fuzzy search and preview of help](https://felp.atusy.net/reference/figures/fuzzyhelp.gif)

This feature is a [Shiny](https://shiny.posit.co/) app and is also available online at <https://atusy.shinyapps.io/fuzzyhelp/>.
Note that some features are disabled in the online version (e.g., links between help topics and syntax highlights).
Also, the online version has a limit on the number of available packages (**tidyverse** + **felp**).

There is also a similar project at <https://helpr.atusy.net/> based on [WebR].
The main differences are appearance and the number of available packages.
[WebR]-based project supports installing any package on CRAN.
Once you type `{{ packageName }}::` (e.g., `dplyr::`), [WebR] installs the package and you can find help topics in the package (e.g., `dplyr::mutate`).

[WebR]: https://docs.r-wasm.org/webr/latest/


### Enhanced alternatives of base features

``` r
library(felp)


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
