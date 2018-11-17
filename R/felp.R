#' Returns source and help of a function simultaneously
#'
#' @param x a name or character string specifying the function for which help and source are sought. Package to be sought can also be defined simultaneously if x is specified in the style of package::name or package:::name
#' @param package a name or character string specifying the package for which help and source are sought. If the package is specified by x, this parameter is neglected, or NULL (default).
#' @param ... other arguments passed to help
#' @importFrom utils help
#' @importFrom prettycode highlight
#' @export
#'
felp <- function(x, package = NULL, ...) {
  x_substituted <- substitute(x)
  # convert package::name to list("name", "package", "`::`)
  # if x = name, input = list("name")
  input <- if(is.character(x)) {
    list(x)
  } else {
    rev(lapply(x_substituted, deparse))
  }

  # Package to look for help of the function
  if (!missing(package) && (p <- substitute(package))) {
    package <- as.character(p)
  }
  package <- c(package, input[2][[1]])

  # Try to find help
  try(print(help(input[[1]], package = package[1], ...)))

  # Print source of the function
  prettycode:::print.function(get(
    input[[1]],
    envir = `if`(is.null(package), parent.frame(), getNamespace(package))
  ))
}
