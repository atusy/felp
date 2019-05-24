#' S3 version of [utils::`?`]
#'
#' @param e1
#'   A topic ought to be documented. Refer to `topic` argument described in `` utils::`?` ``.
#' @param e2
#'   `.` equals to `missing(e2)`. If else, see `type` argument described in `` utils::`?` ``.
#' @rdname question
#' @aliases ?
#' @export
`?` <- function(e1, e2) UseMethod("?")

#' @rdname question
#' @aliases ?
#' @export
`?.function` <- function(e1, e2) {
  if (!missing(e2) && as.character(substitute(e2)) != ".") NextMethod()
  do.call(felp, list(topic = substitute(e1)))
}

#' @rdname question
#' @aliases ?
#' @importFrom utils ?
#' @export
`?.default` <- function(e1, e2) {
  .arg <- list(e1 = substitute(e1))
  .e2 <- substitute(e2)
  if (!missing(.e2) && as.character(.e2) != ".") .arg$e2 <- .e2
  do.call(utils::"?", .arg)
}
