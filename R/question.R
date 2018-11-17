#' S3 version of base::`?`
#'
#' @inheritParams ?
#' @rdname question
#' @aliases ?
#' @export
`?` <- function() UseMethod("?")
#' @rdname question
#' @aliases ?
#' @inheritParams ?
#' @export
`?.function` <- function() {
  f <- deparse(substitute(e1))
  print.function(e1)
  help(f)
}
#' @rdname question
#' @aliases ?
#' @inheritParams ?
#' @importFrom utils ?
#' @export
`?.default` <- utils::"?"

formals(`?`) <- formals(`?.function`) <- formals(`?.default`)
