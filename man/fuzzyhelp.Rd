% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/fuzzyhelp.R
\name{fuzzyhelp}
\alias{fuzzyhelp}
\title{Fuzzily Search Help and View the Selection}
\usage{
fuzzyhelp(
  query = "",
  method = getOption("fuzzyhelp.method", "fzf"),
  background = getOption("fuzzyhelp.background", TRUE)
)
}
\arguments{
\item{query}{An initial query to search for the help system.}

\item{method}{A fuzzy match method to use. Choices are "fzf" and "lv"
(levenstein). The method "lv" is faster but can be less accurate. The
default value can be tweaked by \code{options(fuzzyhelp.method = "lv")}.}

\item{background}{Whether to run a shiny gadget in a background process.
The default value is \code{TRUE} and can be changed by
\code{option(fuzzyhelp.background = FALSE)}.}
}
\description{
Users no more have to afraid of exact name of the object they need help.
A shiny gadget helps you to find a topic fuzzily.
Click radio buttons to switch preview contents.
Click "Done" or "Cancel" to close the widget.
The "Done" button will also hook \code{help} function on the selection.
}
\note{
The default fuzzy match algorithm is a simplified version of
\url{https://github.com/junegunn/fzf}. The implementation in this package
excludes bonuses from relationship with matched characters and their
previous characters.
}
\examples{
if (FALSE) {
  fuzzyhelp()
}

}
