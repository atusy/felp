#' @importFrom rlang .data
#' @importFrom magrittr %>%
NULL

getHelpFile <- function(...) {
  get(".getHelpFile", envir = asNamespace("utils"))(...)
}

get_help <- function(topic, package, help_type = "html", ...) {
  conv <- c(html = tools::Rd2HTML, text = tools::Rd2txt)[[help_type]]
  x <- help((topic), package = (package), help_type = help_type)
  paths <- as.character(x)
  file <- paths[1L]
  pkgname <- basename(dirname(dirname(file)))
  content <- paste(
    utils::capture.output(conv(getHelpFile(file), package = pkgname)),
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


get_content <- function(x, i) {
  if (NROW(x) == 0L || length(i) == 0L) return("")
  if (length(i) > 1L) {
    warning("i should be an integer vector of the length equal to 1.")
    i <- i[[1L]]
  }
  type <- x$Type[i]
  topic <- x$Topic[i]
  package <- x$Package[i]
  if (type == "vignette") return(get_vignette(topic, package))
  get_help(topic, package)
}


create_toc <- function() {
  db <- utils::hsearch_db()
  df <- db$Base[c("Topic", "ID", "Package", "Title", "Type")] %>%
    dplyr::left_join(db$Aliases[c("Package", "Alias", "ID")], by = c("Package", "ID")) %>%
    dplyr::select(!c("ID", "Topic")) %>%
    dplyr::relocate("Package", "Alias", "Title", "Type") %>%
    dplyr::rename(Topic = .data$Alias) %>%
    identity()
  df
}

distmatrix <- function(x, y, case_sensitive) {
  f <- function(x2, ignore.case) {
    adist(x2, y, ignore.case = ignore.case, partial = TRUE, fixed = TRUE)
  }
  res <- matrix(0L, nrow = length(x), ncol = length(y))
  res[case_sensitive, ] <- f(x[case_sensitive], ignore.case = FALSE)
  res[!case_sensitive, ] <- f(x[!case_sensitive], ignore.case = TRUE)
  return(res)
}

score_toc_filtered <- function(toc, queries) {
  unique_queries <- unique(queries)
  case_sensitive <- stringi::stri_detect_regex(unique_queries, '[:upper:]')
  dist_package <- distmatrix(unique_queries, toc$Package, case_sensitive)
  dist_topic <- distmatrix(unique_queries, toc$Topic, case_sensitive)
  dist <- dist_topic
  loc <- dist_package < dist_topic
  dist[loc] <- dist_package[loc]

  score <- matrixStats::colSums2(dist)
  return(score)
}

detect <- function(package, topic, query, case_sensitive) {
  o <- stringi::stri_opts_regex(case_insensitive = !case_sensitive)
  p <- stringi::stri_detect_regex(package, query, opts_regex = o)
  t <- stringi::stri_detect_regex(topic, query, opts_regex = o)
  return(p | t)
}

score_toc <- function(toc, queries) {
  N <- nrow(toc)
  score <- rep(NA_integer_, N)

  # Pre-filtering to drop phrases missing any characters in queries
  # Package and Topic can be united by a space because
  # the current implementation does not support space (` `) as a part of queries
  prefilter <- rep(TRUE, N)
  unique_queries <- unique(queries)
  case_sensitive <- stringi::stri_detect_regex(unique_queries, "[:upper:]")
  prefilter_queries <- unique_queries %>%
    stringi::stri_replace_all_regex("(.)", "\\\\$1.*") %>%
    stringi::stri_replace_all_regex("\\\\(\\w)", "$1")
  package <- toc$Package
  topic <- toc$Topic
  for (i in seq_along(unique_queries)) {
    prefilter[prefilter] = detect(
      package[prefilter],
      topic[prefilter],
      prefilter_queries[i],
      case_sensitive[i]
    )
    if (!any(prefilter)) {
      return(score)
    }
  }

  # Calculate and return score for filtered items
  score[prefilter] <- score_toc_filtered(toc[prefilter, ], queries)
  return(score)
}

arrange <- function(df, queries) {
  if (length(queries) == 0L) return(df)
  df %>%
    dplyr::mutate(SCORE = score_toc(df, queries)) %>%
    dplyr::filter(!is.na(.data$SCORE)) %>%
    dplyr::arrange(
      .data$SCORE,
      stringi::stri_length(paste0(.data$Package, .data$Topic))
    ) %>%
    dplyr::select(!"SCORE")
}

create_ui <- function(query = "") {
  miniUI::miniPage(
    miniUI::gadgetTitleBar("Fuzzy Help Search"),
    miniUI::miniContentPanel(
      shiny::textInput(
        "query",
        label = "Search query",
        value = paste(query, collapse = " "),
        width = "100%"
      ),
      reactable::reactableOutput("tocViewer", width = "100%", height = "200px"),
      htmltools::tags$div(
        id = "bar",
        style = "width: 100%; height: 8px; cursor: row-resize; background-color: transparent;",
        draggable = "true"
      ),
      shiny::uiOutput("helpViewer"),
      style = "display: grid; grid-template-rows: auto auto auto 1fr"
    ),
    htmltools::tags$style("
      #tocViewer {
        overflow: hidden;
      }
    "),
    htmltools::tags$script("
      (function(){
        // Resize tocViewer
        const toc = document.getElementById('tocViewer');
        const bar = document.getElementById('bar');
        let screenY, tocHeight
        bar.addEventListener('dragstart', function() {
          screenY = window.event.screenY;
          tocHeight = toc.getBoundingClientRect().height;
        });
        bar.addEventListener('drag', function() {
          const diff = window.event.screenY - screenY;
          toc.style.height = tocHeight + diff + 'px';
        });
      })();
    "),
    style = "display: grid; grid-template-rows: auto 1fr; height: 100vh"
  )
}

parse_query <- function(string) {
  queries <- stringi::stri_split_fixed(string, " ")[[1L]]
  queries[queries != ""]
}

server <- function(input, output) {
  toc <- create_toc()
  reactiveQueries <- shiny::reactive(parse_query(input$query))
  reactiveToc <- shiny::reactive(arrange(toc, reactiveQueries()))
  reactiveTocViewer <- shiny::reactive(local({
    toc_matched <- reactiveToc()
    reactable::reactable(
      toc_matched,
      pagination = TRUE,
      defaultPageSize = 20,
      selection = "single",
      defaultSelected = if (nrow(toc_matched) != 0) 1L,
      onClick = "select"
    )
  }))
  reactiveSelection <- shiny::reactive({
    reactiveToc()  # avoids noisy refresh
    reactable::getReactableState("tocViewer", "selected")
  })
  reactiveHelp <- shiny::reactive(
    htmltools::tags$iframe(
      srcdoc = get_content(reactiveToc(), reactiveSelection()),
      style = "width: 100%; height: 100%;"
    )
  )

  output$tocViewer <- reactable::renderReactable(reactiveTocViewer())
  output$helpViewer <- shiny::renderUI(reactiveHelp())

  shiny::observeEvent(input$done, {
    shiny::stopApp()
    selection <- reactiveToc()[reactiveSelection(), ]
    type <- selection$Type[1L]
    topic <- selection$Topic[1L]
    package <- selection$Package[1L]
    if (rstudioapi::isAvailable()) {
      rstudioapi::sendToConsole(
        sprintf('%s("%s", package = "%s")', type, topic, package),
        execute = TRUE
      )
    } else {
      getNamespace("utils")[[type]]((topic), (package))
    }
  })
}

#' Fuzzily Search Help and View the Selection
#'
#' Users no more have to afraid of exact name of the object they need help.
#' A shiny gadget helps you to find a topic fuzzily.
#' Click radio buttons to switch preview contents.
#' Click "Done" or "Cancel" to close the widget.
#' The "Done" button will also hook `help` function on the selection.
#'
#' @param query An initial query to search for the help system.
#' @note
#' The fuzzy match algorithm is experimental, and may change in the future.
#'
#' @return NULL
#'
#' @examples
#' if (FALSE) {
#'   fuzzyhelp()
#' }
#'
#' @export
fuzzyhelp <- function(query = "") shiny::runGadget(create_ui(query), server)
