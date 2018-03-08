# felp

felp is a short of **f**unction h**elp**.
This package provides functions to print source and help of a function simultaneously.

## Installation

You can install the development version from [GitHub](https://github.com/) with:

``` r
# install.packages("devtools")
devtools::install_github("atusy/felp")
```

## Example

Following codes provide same results to print help and source of `help`.

``` r
help
utils::help
felp(help)
felp("help")
felp(utils::help)
felp(help, utils)
```

