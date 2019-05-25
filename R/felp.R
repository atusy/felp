#' Functional help which displays structure an object in addition to help
#'
#' Structure of object is returned by `str()`.
#' For a function, its source is returned instead of `str()`.
#'
#' @inheritParams utils::help
#' @inheritDotParams utils::help -topic -package
#' @importFrom utils help
#' @export
#'
felp <- function(topic, package = NULL, ...) {
  # convert package::name to c("name", "package", "`::`")
  # or name to "name"
  t <- rev(as.character(substitute(topic)))

  p <- c(as.character(substitute(package)), t[2L])[1L]
  if (is.na(p)) p <- NULL

  # Print source of the function
  str(get(t[1L], envir = `if`(is.null(p), parent.frame(), asNamespace(p))))

  try(help(t[1L], p[1L], ...))
}
