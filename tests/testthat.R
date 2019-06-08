library(testthat)
library(felp)

test_check("felp")

rmarkdown::render("./testthat/ans.Rmd")
