expect_identical_except_bytecode <- function(x, y) {
  x <- readLines(x)
  y <- readLines(y)
  expect_identical(
    x[!grepl("^    ## <bytecode:", x)],
    y[!grepl("^    ## <bytecode:", y)]
  )
}
