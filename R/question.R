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
  .felp <- felp
  formals(.felp)$x <- substitute(e1)
  .felp()
}
#' @rdname question
#' @aliases ?
#' @inheritParams ?
#' @importFrom utils ?
#' @export
`?.default` <- utils::"?"

formals(`?`) <- formals(`?.function`) <- formals(`?.default`)
