# felp

`felp` is a short of **f**unctional h**elp**

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
