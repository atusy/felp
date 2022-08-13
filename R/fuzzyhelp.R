.data <- rlang::.data
`%>%` <- magrittr::`%>%`

getHelpFile <- function(...) {
  get(".getHelpFile", envir = asNamespace("utils"))(...)
}

stringhelp <- function(topic, help_type = "html", ...) {
  if (NROW(topic) == 0L) return("")
  conv <- c(html = tools::Rd2HTML, text = tools::Rd2txt)[[help_type]]
  x <- help(
    (topic$Topic[1L]), package = (topic$Package[1L]), help_type = help_type
  )
  paths <- as.character(x)
  file <- paths[1L]
  pkgname <- basename(dirname(dirname(file)))
  html <- paste(
    utils::capture.output(conv(getHelpFile(file), package = pkgname)),
    collapse = "\n"
  )
  return(html)
}#; stringhelp("mutate", "dplyr")

create_toc <- function() {
  db <- utils::hsearch_db()
  df <- db$Base[c("Topic", "ID", "Package", "Title", "Type")] %>%
    dplyr::left_join(db$Aliases[c("Package", "Alias", "ID")], by = c("Package", "ID")) %>%
    dplyr::select(!c("ID", "Topic")) %>%
    dplyr::relocate("Package", "Alias", "Title", "Type") %>%
    dplyr::filter(.data$Type == "help") %>% dplyr::select(!"Type") %>% # TODO: support vignette
    dplyr::rename(Topic = .data$Alias) %>%
    identity()
  df
}

score_toc <- function(toc, queries) {
  unique_queries <- unique(queries)
  dist_package <- stringdist::stringdistmatrix(toc$Package, unique_queries, method = "lv")
  dist_topic <- stringdist::stringdistmatrix(toc$Topic, unique_queries, method = "lv")
  afound_title <- stringdist::afind(toc$Title, unique_queries)

  score_df <- data.frame(
    index = seq(NROW(toc)),
    package = matrixStats::rowSums2(dist_package),
    topic = matrixStats::rowSums2(dist_topic),
    title = matrixStats::rowSums2(afound_title$distance)
  ) %>%
    dplyr::mutate(score = 0.5 * .data$package + .data$topic + 0.1 * .data$title)
    # focus on topic, less on score, and least on title

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

arrange <- function(df, queries) {
  if (length(queries) == 0L) return(df)
  df %>%
    dplyr::mutate(SCORE = score_toc(df, queries)) %>%
    dplyr::arrange(.data$SCORE) %>%
    dplyr::select(!"SCORE")
}

create_ui <- function() {
  miniUI::miniPage(
    miniUI::gadgetTitleBar("Fuzzy Help Search"),
    miniUI::miniContentPanel(
      shiny::textInput("query", label = "Search query", width = "100%"),
      reactable::reactableOutput("tocViewer", width = "100%", height = "200px"),
      htmltools::tags$div(
        id = "bar",
        style = "width: 100%; height: 8px; cursor: pointer; background-color: transparent;",
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
  reactiveTocViewer <- shiny::reactive(
    reactable::reactable(
      reactiveToc(), pagination = TRUE, defaultPageSize = 20,
      selection = "single", defaultSelected = 1L, onClick = "select"
    )
  )
  reactiveSelection <- shiny::reactive(
    reactable::getReactableState("tocViewer", "selected")
  )
  reactiveHelp <- shiny::reactive(
    htmltools::tags$iframe(
      srcdoc = stringhelp(reactiveToc()[reactiveSelection(), ]),
      style = "width: 100%; height: 100%;"
    )
  )

  output$tocViewer <- reactable::renderReactable(reactiveTocViewer())
  output$helpViewer <- shiny::renderUI(reactiveHelp())

  shiny::observeEvent(input$done, {
    shiny::stopApp()
    selection <- reactiveToc()[reactiveSelection(), ]
    if (rstudioapi::isAvailable()) {
      rstudioapi::sendToConsole(
        sprintf("help(%s, package = %s)", selection$Topic, selection$Package),
        execute = TRUE
      )
    } else {
      print(help((selection$Topic), (selection$Package)))
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
#' @return NULL
#'
#' @export
fuzzyhelp <- function() shiny::runGadget(create_ui(), server)
