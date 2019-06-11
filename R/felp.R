#' Functional help which displays structure of an object in addition to help
#'
#' Structure of object is returned by `str()`.
#' For a function, its source is returned instead of `str()`.
#'
#' @inheritParams utils::help
#' @inheritDotParams utils::help -topic -package
#'
#' @examples
#' # Identical to help(identity); print(identity)
#' felp(identity)
#'
#' # Identical to help(iris); str(iris)
#' felp(iris)
#'
#' # Identical to help(package = MASS)
#' felp(package = MASS)
#'
#'
#' @importFrom utils help str
#' @export
#'
felp <- function(topic, package = NULL, ...) {
  # Display package document
  if (missing(topic)) return(do.call(help, list(package = substitute(package), ...)))

  # Convert `package::name` to c("name", "package", "`::`") or `name` to "name"
  t <- rev(as.character(substitute(topic)))

  p <- c(as.character(substitute(package)), t[2L])[1L]
  if (is.na(p)) p <- NULL

  # Display structure and document of an object
  str(get(t[1L], envir = `if`(is.null(p), parent.frame(), asNamespace(p))))

  try(help(t[1L], p[1L], ...))
}
