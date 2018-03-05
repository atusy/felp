#' Display source and help of function
#' @param x function
#' @param ... Other arguments passed to felp. felp is called when print.function is explicitly called or via print.
#' @importFrom utils savehistory
#' @importFrom utils tail
#' @importFrom utils ?
#' @export
#'
print.function <- function(x, ...) {
  tempfile <- tempfile(pattern="rhistory_felp_", fileext=".txt")
  if(.Platform$OS.type == 'windows') savehistory <- get('savehistory')
  savehistory(tempfile)
  cmd <- as.character(parse(text = tail(readLines(tempfile), 1)))
  # cmd <- deparse(rev(histry::histry())[[2]])

  # return result of felp when print.function is explicitly called or via print.
  if(grepl('print(.function)?\\(', cmd)) {
    input <- rev(lapply(substitute(x), deparse))
    if(!is.function(x)) stop("x must be a function")
    return(felp(x = input[[1]], package = input[2][[1]]))
  }

  #print help
  try(print(eval(parse(text = paste0('?', cmd)))))

  #print source
  base::print.function(eval(parse(text = cmd)))
}

