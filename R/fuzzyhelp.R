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


get_content <- function(x) {
  if (NROW(x) == 0L) return("")
  type <- x$Type[1L]
  topic <- x$Topic[1L]
  package <- x$Package[1L]
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

score_toc_filtered <- function(toc, queries) {
  unique_queries <- unique(queries)
  dist_package <- stringdist::stringdistmatrix(toc$Package, unique_queries, method = "lv")
  dist_topic <- stringdist::stringdistmatrix(toc$Topic, unique_queries, method = "lv")
  #afound_title <- stringdist::afind(toc$Title, unique_queries)

  score_df <- data.frame(
    index = seq(NROW(toc)),
    package = matrixStats::rowSums2(dist_package),
    topic = matrixStats::rowSums2(dist_topic),
    #title = matrixStats::rowSums2(afound_title$distance)
    title = 0
  ) %>%
    dplyr::mutate(score = 0.5 * .data$package + .data$topic + 0.1 * .data$title)
    # focus on topic, less on package, and least on title

  if (length(queries) == 1L) return(score_df$score)

  score <- score_df[["score"]]
  idx <- utils::head(dplyr::arrange(score_df, .data$score)$index, 20)
  idx_last <- utils::tail(idx, 1L)
  tbl <- c(table(queries))
  for (i in idx) {
    p <- which.min(dist_package[i, ])
    t <- which.min(dist_topic[i, ])
    if (p == t && tbl[queries[p]] == 1L) {
      score[i] <- score[idx_last]
    }
  }
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
  prefilter_queries <- stringi::stri_replace_all_regex(unique_queries, "(.)", "$1.*")
  package <- toc$Package
  topic <- toc$Topic
  opts <- stringi::stri_opts_regex(case_insensitive = TRUE)
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

  score[prefilter] <- score_toc_filtered(toc[prefilter, ], queries)
  return(score)
}

arrange <- function(df, queries) {
  if (length(queries) == 0L) return(df)
  df %>%
    dplyr::mutate(SCORE = score_toc(df, queries)) %>%
    dplyr::filter(!is.na(.data$SCORE)) %>%
    dplyr::arrange(.data$SCORE) %>%
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
  queries <- stringr::str_split(string, "\\s+")[[1L]]
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
      srcdoc = get_content(reactiveToc()[reactiveSelection(), ]),
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
