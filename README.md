# felp

felp is a short of **f**unction h**elp**.
This package provides functions to print 
source and help of a function simultaneously.

## Installation

Copy & paste:

``` r
source("https://install-github.me/atusy/felp")
```

## Example

These provide same results to print help and source of `help`.

``` r
help?.
utils::help?.
?help
?utils::help
felp(help)
felp("help")
felp(utils::help)
felp(help, utils)
```
