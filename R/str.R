#' str for function
#'
#' @noRd
#'
#' @importFrom utils str
str.function <- function(object, ...) {
  get("print.function", envir = asNamespace("prettycode"))(object, ...)
}
