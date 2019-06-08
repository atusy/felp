#' str for function
#'
#' @noRd
#'
#' @importFrom utils str getFromNamespace
str.function <- function(object, ...) {
  getFromNamespace("print.function", "prettycode")(object, ...)
}
