#' Returns source and help of a function simultaneously
#'
#' @param topic A name or character string specifying the function for which help and source are sought. Package to be sought can also be defined simultaneously if x is specified in the style of package::name or package:::name
#' @param package A name or character string specifying the package for which help and source are sought. If the package is specified by x, this parameter is neglected, or NULL (default).
#' @param ... Other arguments passed to help
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
  prettycode:::print.function(get(
    t[1L], envir = `if`(is.null(p), parent.frame(), asNamespace(p))
  ))

  try(help(t[1L], p[1L], ...))

}
