.data <- rlang::.data

stringhelp <- function(topic, help_type = "html", ...) {
  if (NROW(topic) == 0L) return("")
  conv <- c(html = tools::Rd2HTML, text = tools::Rd2txt)[[help_type]]
  x <- help(
    (topic$Topic[1L]), package = (topic$Package[1L]), help_type = help_type
  )
  paths <- as.character(x)
  file <- paths[1L]
  pkgname <- basename(dirname(dirname(file)))
  html <- paste(capture.output(conv(utils:::.getHelpFile(file), package = pkgname)), collapse = "\n")
  print(html)
  return(html)
}#; stringhelp("mutate", "dplyr")

create_toc <- function() {
  db <- utils::hsearch_db()
  df <- db$Base[c("Topic", "ID", "Package", "Title", "Type")] |>
    dplyr::left_join(db$Aliases[c("Alias", "ID")], by = "ID") |>
    dplyr::filter(.data$Alias == .data$Topic) |>
    dplyr::select(!c("ID", "Topic")) |>
    dplyr::relocate("Package", "Alias", "Title", "Type") |>
    dplyr::filter(.data$Type == "help") |> dplyr::select(!"Type") |> # TODO: support vignette
    dplyr::rename(Topic = .data$Alias) |>
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
    package = matrixStats::rowMins(dist_package),
    topic = matrixStats::rowMins(dist_topic),
    title = matrixStats::rowSums2(afound_title$distance)
  ) |>
    dplyr::mutate(score = 0.5 * .data$package + .data$topic + 0.1 * .data$title)
    # focus on topic, less on score, and least on title

  if (length(queries) == 1L) return(score_df$score)

  score <- score_df[["score"]]
  idx <- head(dplyr::arrange(score_df, .data$score)$index, 20)
  idx_last <- tail(idx, 1L)
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
  if (identical(queries, "")) return(df)
  df |>
    dplyr::mutate(SCORE = score_toc(df, queries)) |>
    dplyr::arrange(.data$SCORE) |>
    dplyr::select(!"SCORE")
}

create_ui <- function() {
  shiny::fluidPage(
    shiny::titlePanel("Fuzzy Help Search"),
    shiny::textInput("query", label = "Search query", width = "100%", value = "ggplot geom_pint"),
    reactable::reactableOutput("searchResult", width = "100%", height = "200px"),
    shiny::uiOutput("helpHTML")
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
      selection = "single", defaultSelected = 1L
    )
  )
  reactiveSelection <- shiny::reactive(
    reactable::getReactableState("tocViewer", "selected")
  )
  reactiveHelp <- shiny::reactive(
    htmltools::tags$iframe(
      srcdoc = stringhelp(reactiveToc()[reactiveSelection(), ]),
      width = "100%", height = "500px"
    )
  )

  output$tocViewer <- reactable::renderReactable(reactiveTocViewer())
  output$helpViewer <- shiny::renderUI(reactiveHelp())
}

shiny::runGadget(create_ui(), server)
