get_help_file <- function(...) {
  get(".getHelpFile", envir = asNamespace("utils"))(...)
}

get_help <- function(topic, package, help_type = "html", ...) {
  conv <- c(html = tools::Rd2HTML, text = tools::Rd2txt)[[help_type]]
  x <- help((topic), package = (package), help_type = help_type)
  paths <- as.character(x)
  file <- paths[1L]
  pkgname <- basename(dirname(dirname(file)))
  content <- paste(
    utils::capture.output(conv(get_help_file(file), package = pkgname)),
    collapse = "\n"
  )
  return(content)
}

get_vignette <- function(topic, package) {
  v <- utils::vignette(topic, package)
  p <- file.path(v$Dir, "doc", v$PDF)
  ext <- tools::file_ext(p)
  if (ext != "html") {
    return(sprintf(
      "<p>The extention of vignette should be html: %s</p>", p
    ))
  }
  paste(readLines(p), collapse = "")
}

# Just to skip a note
#
# > checking dependencies in R code ... NOTE
# > Namespace in Imports field not imported from: ‘prettycode’
# > All declared Imports should be used.
.default_style <- function(...) prettycode::default_style(...)
.curl <- function(...) curl::curl(...)
