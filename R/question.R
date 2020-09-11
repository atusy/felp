#' Functional help with `?` operator
#'
#' Displays help and structure of an object, or help of a package.
#' Two syntax sugars are added to enhance `utils::?`.
#' One is `object?.` which works as if `?object`.
#' Another is `package?p` which works as if `help(package = package)`
#'
#' @param e1
#'   A topic of document.
#'   Refer to `topic` argument described in `` utils::`?` ``.
#' @param e2
#'   `.` and `p` have special meanings as documented above.
#'   Otherwise, `e2` is same as `type` argument of `` utils::`?` ``.
#'
#' @rdname question
#' @aliases ?
#'
#' @examples
#' # Identical to help(identity); print(identity)
#' ?identity
#' identity?.  # The same
#'
#' # Identical to help(iris); str(iris)
#' ?iris
#' iris?.  # The same
#'
#' # Identical to help(package = stats)
#' stats?p
#'
#' @importFrom utils ?
#' @export
`?` <-  function(e1, e2) {
  .e1 <- substitute(e1)
  .e2 <- substitute(e2)
  if (missing(e2)) return(do.call(felp, list(topic = .e1)))
  .e2_chr <- as.character(.e2)
  if (.e2_chr == ".") return(do.call(felp, list(topic = .e1)))
  if (.e2_chr == "p") return(do.call(help, list(package = .e1)))
  do.call(utils::`?`, list(e1 = .e1, e2 = .e2))
}
