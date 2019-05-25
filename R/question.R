#' Functional help with `?` operator
#'
#' Displays help and structure of an object.
#' A special syntax `?.` is supported in addition to
#' the one in `utils::?`.
#' `?.` works as if it is a postfix operator.
#' For example, `help?.` is equivalent to `?help`.
#'
#' @param e1
#'   A topic ought to be documented.
#'   Refer to `topic` argument described in `` utils::`?` ``.
#' @param e2
#'   `.` equals to `missing(e2)`.
#'   If else, see `type` argument described in `` utils::`?` ``.
#'
#' @rdname question
#' @aliases ?
#'
#' @importFrom utils ?
#' @export
`?` <-  function(e1, e2) {
  .e1 <- substitute(e1)
  .e2 <- substitute(e2)
  if (!missing(e2) && as.character(.e2) != ".") {
    return(do.call(utils::`?`, list(e1 = .e1, e2 = .e2)))
  }
  do.call(felp, list(topic = .e1))
}
