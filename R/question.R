#' S3 version of [utils::`?`]
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
  if(!missing(e2) && as.character(substitute(e2)) != ".") {
    NextMethod()
  }
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
