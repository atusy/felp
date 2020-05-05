#' Translated Help
#'
#' The translated help is available when `topic` belongs to a `package` installed
#' from CRAN and the system is online. Otherwise, normal help will be shown.
#' The source of the help is powered by 'RDocumentation', and
#' the translation is powered by 'Google Translate.
#'
#' @inheritParams utils::help
#' @param translate_to The help document will be translated to the specified
#' language (e.g., `"ja"` for Japanese and `"de"` for German). If `"default"`,
#' the function looks up `getOption("telp_translate_to", "en")`.
#' @param ... Arguments passed to `utils::help`
#'
#' @examples
#' \dontrun{
#' telp(help)
#' }
#'
telp <- function(topic, package = NULL, translate_to = "default", ...) {
  if (missing(topic)) {
    warning("telp requires a topic to be translated")
    return(help(package = substitute(package), ...))
  }


  .help <- do.call(utils::help, list(substitute(topic), substitute(package), ...))
  package <- sub(".*/([^/]+)/help/([^/]+)$", "\\1", .help[1])
  description <- utils::packageDescription(package)

  disconnected <- !curl::has_internet()
  not_cran <- !identical(description$Repository, "CRAN")
  if (disconnected || not_cran) {
    warning(
      "Showing local help ",
      if (disconnected) "because of being offline",
      if (disconnected && not_cran) "and",
      if (not_cran) "because the package is not installed from CRAN"
    )
    return(.help)
  }

  utils::browseURL(url_telp(
    translate_to = spec_translate_to(translate_to),
    package = package,
    version = description$Version,
    topic = sub(".*/", "", .help[1])
  ))

  invisible(.help)
}

spec_translate_to <- function(translate_to = "default") {
  if (identical(translate_to, "default")) {
    return(getOption("telp_translate_to", "en"))
  }
  translate_to
}

url_telp <- function(translate_to, package, version, topic) {
  url_google_translate(
    translate_to, url_rdocumentation(package, version, topic)
  )
}

url_rdocumentation <- function(package, version, topic) {
  sprintf(
    "https://www.rdocumentation.org/packages/%s/versions/%s/topics/%s",
    package, version, topic
  )
}

#' @noRd
#' @param tl translated_to
#' @param u url
url_google_translate <- function(tl = "ja", u = "") {
  sprintf(
    "https://translate.google.com/translate?hl=en&sl=auto&tl=%s&u=%s",
    tl, u
  )
}
