#' Returns source and help of a function simultaneously
#'
#' @param x a name or character string specifying the function for which help and source are sought. Package to be sought can also be defined simultaneously if x is specified in the style of package::name or package:::name
#' @param package a name or character string specifying the package for which help and source are sought. If the package is specified by x, this parameter is neglected, or NULL (default).
#' @param ... other arguments passed to help
#' @importFrom utils help
#' @export
#'
felp <- function(x, package = NULL, ...) {
  x_substituted <- substitute(x)
  # convert package::name to list("name", "package", "`::`)
  # if x = name, input = list("name")
  input <- if(is.character(x))
      list(x)
    else
      rev(lapply(as.list(substitute(x)), deparse))

  # Package to look for help of the function
  package <- c(package, input[2][[1]])

  # print help
  if(is.null(package))
    print(help(input[[1]], ...))
  else
    print(help(input[[1]], package = package[1], ...))

  # print source of the function
  base::print.function(get(
    input[[1]],
    envir = if(is.null(package)) .GlobalEnv else getNamespace(package)
  ))
}

