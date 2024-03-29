#' @importFrom rlang .data
#' @importFrom magrittr %>%
NULL

#' Get preview content for Shiny UI
#' @noRd
get_content <- function(x, i) {
  if (NROW(x) == 0L || length(i) == 0L) {
    return("")
  }
  if (length(i) > 1L) {
    warning("i should be an integer vector of the length equal to 1.")
    i <- i[[1L]]
  }
  type <- x$Type[i]
  topic <- x$Topic[i]
  package <- x$Package[i]
  if (type == "help") {
    return(get_help(topic, package))
  }
  if (type == "vignette") {
    return(get_vignette(topic, package))
  }
  if (type == "demo") {
    return('Press "Done" to see demo.')
  }
  paste("Viewer not available for the type:", type)
}

#' Create ToC of help
#' @noRd
create_toc <- function() {
  db <- utils::hsearch_db()
  db$Base[c("Topic", "ID", "Package", "Title", "Type")] %>%
    dplyr::left_join(
      db$Aliases[c("Package", "Alias", "ID")],
      by = c("Package", "ID")
    ) %>%
    dplyr::select(!c("ID", "Topic")) %>%
    dplyr::relocate("Package", "Alias", "Title", "Type") %>%
    dplyr::rename(Topic = .data$Alias)
}

score_one <- function(query_chars, target_chars, extra_bonus = FALSE) {
  fzf_core(
    target_chars, query_chars,
    must_match = 0L, extra_bonus = extra_bonus
  )$score
}

score_vec <- function(target, query_chars_list, ...) {
  target_chars <- split_chars(target)[[1L]]
  vapply(
    query_chars_list, score_one, NA_integer_,
    target_chars = target_chars, ...
  )
}

score_matrix <- function(targets, query_chars_list, ...) {
  unique_targets <- unique(targets)
  n <- lengths(query_chars_list)
  score <- matrix(
    0L, length(query_chars_list), length(unique_targets),
    dimnames = list(NULL, unique_targets)
  )
  # as score_one returns integer, tiebreak works as tiebreak
  tiebreak <- matrix(
    0.1 / stringi::stri_length(unique_targets),
    length(query_chars_list), length(unique_targets),
    dimnames = list(NULL, unique_targets), byrow = TRUE
  )

  single <- n == 1L
  if (any(single)) {
    score[single, ] <- local({
      queries <- query_chars_list[single]
      s <- matrix(
        substr(unique_targets, 1L, 1L), sum(single), length(unique_targets),
        byrow = TRUE
      )
      16L + 16L * (s == queries)
    })
  }

  double <- n == 2L
  if (any(double)) {
    score[double, ] <- local({
      queries <- query_chars_list[double]
      do.call(
        rbind,
        lapply(queries, function(x) {
          query <- paste(x, collapse = "")
          s <- rep(NA_integer_, length(unique_targets))
          fmatch_abs <- stringi::stri_startswith_fixed(unique_targets, query)
          s[fmatch_abs] <- 52L
          pmatch_abs <- stringi::stri_detect_fixed(
            unique_targets[!fmatch_abs], query
          )
          s[!fmatch_abs][pmatch_abs] <- 36L

          fzy <- is.na(s)
          pattern_base <- paste0(x[[1L]], ".*?", x[[2L]])
          pattern_fmatch_fzy <- paste0("^", pattern_base)
          pattern_pmatch_fzy <- paste0(".", pattern_base)
          phrase_fmatch_fzy <- stringi::stri_extract_first_regex(
            unique_targets[fzy], pattern_fmatch_fzy
          )
          phrase_fmatch_fzy_alt <- stringi::stri_extract_first_regex(
            phrase_fmatch_fzy, pattern_pmatch_fzy
          )
          need_alt <- !is.na(phrase_fmatch_fzy_alt)
          phrase_fmatch_fzy[need_alt] <- phrase_fmatch_fzy_alt[need_alt]
          score_fmatch_fzy <- (
            48L - 16L * need_alt - stringi::stri_length(phrase_fmatch_fzy)
          )
          score_fmatch_fzy[is.na(score_fmatch_fzy)] <- 0L
          score_pmatch_fzy <- 32L - vapply(
            stringi::stri_match_all_regex(
              unique_targets[fzy], pattern_pmatch_fzy
            ),
            function(x) min(stringi::stri_length(x) - 1L),
            NA_integer_
          )
          score_pmatch_fzy[is.na(score_pmatch_fzy)] <- 0L
          s[fzy] <- dplyr::if_else(
            score_fmatch_fzy > score_pmatch_fzy,
            score_fmatch_fzy, score_pmatch_fzy
          )
          s
        })
      )
    })
  }

  long <- !(single | double)
  if (any(long)) {
    score[long, ] <- do.call(
      cbind, lapply(unique_targets, score_vec, query_chars_list[long], ...)
    )
  }

  score[is.na(score)] <- 0L
  score <- score + tiebreak
  score[, targets, drop = FALSE]
}

adist2 <- function(x, y, case_sensitive) {
  f <- function(x2, case_insensitive) {
    utils::adist(
      x2, y,
      ignore.case = case_insensitive, partial = TRUE, fixed = TRUE
    )
  }
  res <- matrix(0L, nrow = length(x), ncol = length(y))
  res[case_sensitive, ] <- f(x[case_sensitive], ignore.case = FALSE)
  res[!case_sensitive, ] <- f(x[!case_sensitive], ignore.case = TRUE)
  return(res)
}

score_toc_filtered <- list(
  fzf = function(toc, queries) {
    query_chars_list <- split_chars(queries)
    score <- score_matrix(toc$Package, query_chars_list, extra_bonus = FALSE)
    topic <- score_matrix(toc$Topic, query_chars_list, extra_bonus = FALSE)
    right <- score < topic
    score[right] <- topic[right]
    return(-colSums(score))
  },
  lv = function(toc, queries) {
    res <- adist2(toc$Package, queries)
    topic <- adist2(toc$Topic, queries)
    right <- res > topic
    res[right] <- topic[right]
    len_package <- stringi::stri_length(toc$Package)
    len_topic <- stringi::stri_length(toc$Topic)
    tiebreak <- 0.1 / dplyr::if_else(right, len_package, len_topic)
    return(res - tiebreak)
  }
)

detect <- function(package, topic, query, case_sensitive) {
  o <- stringi::stri_opts_regex(case_insensitive = !case_sensitive)
  d <- stringi::stri_detect_regex(package, query, opts_regex = o)
  d[!d] <- stringi::stri_detect_regex(topic[!d], query, opts_regex = o)

  return(d)
}

score_toc <- function(toc, queries, method = c("fzf", "lv")) {
  n <- nrow(toc)
  score <- rep(NA_integer_, n)
  method <- match.arg(method)

  # Pre-filtering to drop phrases missing any characters in queries
  # Package and Topic can be united by a space because
  # the current implementation does not support space (` `) as a part of queries
  prefilter <- rep(TRUE, n)
  unique_queries <- unique(queries)
  case_sensitive <- stringi::stri_detect_regex(unique_queries, "[:upper:]")
  prefilter_queries <- unique_queries %>%
    stringi::stri_replace_all_regex("(.)", "\\\\$1.*") %>%
    stringi::stri_replace_all_regex("\\\\(\\w)", "$1")
  package <- toc$Package
  topic <- toc$Topic
  for (i in seq_along(unique_queries)) {
    prefilter[prefilter] <- detect(
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
  score[prefilter] <- score_toc_filtered[[method]](
    toc[prefilter, ], unique_queries
  )
  return(score)
}

search_toc <- function(df, queries, ...) {
  if (length(queries) == 0L) {
    return(df)
  }
  df %>%
    dplyr::mutate(SCORE = score_toc(df, queries, ...)) %>%
    dplyr::filter(!is.na(.data$SCORE)) %>%
    dplyr::arrange(
      .data$SCORE,
      .data$Package,
      .data$Topic,
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
        style = paste(
          "width: 100%; height: 8px; cursor: row-resize;",
          "background-color: transparent;"
        ),
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

create_server <- function(method = c("fzf", "lv")) {
  method <- match.arg(method)
  function(input, output) {
    toc <- create_toc()
    reactiveQueries <- shiny::reactive(parse_query(input$query))
    reactiveToc <- shiny::reactive(search_toc(toc, reactiveQueries(), method = method))
    reactiveTocViewer <- shiny::reactive(local({
      toc_matched <- dplyr::mutate(
        reactiveToc(),
        Title = dplyr::if_else(
          .data$Type == "help",
          .data$Title,
          sprintf("%s (%s)", .data$Title, .data$Type)
        ),
        Type = NULL
      )
      reactable::reactable(
        toc_matched,
        pagination = TRUE,
        showPagination = TRUE,
        defaultPageSize = 20,
        selection = "single",
        defaultSelected = if (nrow(toc_matched) != 0) 1L,
        onClick = "select"
      )
    }))
    reactiveSelection <- shiny::reactive({
      reactiveToc() # avoids noisy refresh
      reactable::getReactableState("tocViewer", "selected")
    })
    reactiveHelp <- shiny::reactive(
      htmltools::tags$iframe(
        srcdoc = get_content(reactiveToc(), reactiveSelection()),
        style = "width: 100%; height: 100%;",
        id = "helpViewer",
        onload = "(function(){
          // replace anchors to avoid nesting shiny widgets
          const pattern = document.baseURI + '#';
          const iframe = document.querySelector('#helpViewer iframe');
          Array.from(iframe.contentDocument.querySelectorAll('a'))
            .filter(a => a.href.startsWith(pattern))
            .map(a => {
              const id = a.href.slice(pattern.length);
              a.href = 'javascript:void(0)';
              a.onclick = function() {
                const top = iframe.contentDocument.getElementById(id).offsetTop;
                iframe.contentWindow.scrollTo({ top: top, behavior: 'smooth' });
              }
            });
        })();"
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
}

.env <- new.env()

#' Fuzzily Search Help and View the Selection
#'
#' Users no more have to afraid of exact name of the object they need help.
#' A shiny gadget helps you to find a topic fuzzily.
#' Click radio buttons to switch preview contents.
#' Click "Done" or "Cancel" to close the widget.
#' The "Done" button will also hook `help` function on the selection.
#'
#' @param query An initial query to search for the help system.
#' @param method A fuzzy match method to use. Choices are "fzf" and "lv"
#'  (levenstein). The method "lv" is faster but can be less accurate. The
#'  default value can be tweaked by `options(fuzzyhelp.method = "lv")`.
#' @param background Whether to run a shiny gadget in a background process.
#'  The default value is `TRUE` and can be changed by
#'  `option(fuzzyhelp.background = FALSE)`.
#' @inheritParams shiny::runGadget
#'
#' @note
#' The default fuzzy match algorithm is a simplified version of
#' <https://github.com/junegunn/fzf>. The implementation in this package
#' excludes bonuses from relationship with matched characters and their
#' previous characters.
#'
#' @return
#'   If the `background` argument is `TRUE`, then the return value inherits from
#'  `callr::r_bg()`. Otherwise, `NULL` is returned.
#'
#' @examples
#' if (FALSE) {
#'   fuzzyhelp()
#' }
#'
#' @export
fuzzyhelp <- function(
    query = "",
    method = getOption("fuzzyhelp.method", "fzf"),
    background = getOption("fuzzyhelp.background", TRUE),
    viewer = shiny::paneViewer()) {
  app <- create_ui(query)
  server <- create_server(method)

  # Create new gadget on foreground
  if (!background) {
    shiny::runGadget(app, server, viewer = viewer)
    return(invisible(NULL))
  }

  # Prepare background execution
  if (is.null(.env$fuzzyhelp_url)) {
    .env$fuzzyhelp_url <- tempfile()
  }

  # View existing gadget
  if (fuzzyhelp_bg_view(viewer)) {
    return(.env$fuzzyhelp)
  }

  # Create new gadget on background
  if (rstudioapi::isAvailable()) {
    # Just start the UI without viewer because RStudio's viewer
    # is not available fro the background process.
    .env$fuzzyhelp <- fuzzyhelp_bg_start(app, server, identity)

    # Wait and view UI in the main process.
    min_seed <- 1L
    for (i in c(rep(min_seed, 10L), seq(min_seed + 1, 10))) {
      if (fuzzyhelp_bg_view(viewer)) {
        return(.env$fuzzyhelp)
      } else {
        # Wait with exponential backoff
        t <- max((i**2) / 10, 0.5)
        if (i > min_seed) {
          # Don't be too noisy
          message("Failed to open fuzzyhelp UI. Will retry in ", t, " seconds")
        }
        Sys.sleep(t)
      }
    }
    stop("Failed to open fuzzyhelp UI. Try using fuzzyhelp(background = FALSE)")
  }

  .env$fuzzyhelp <- fuzzyhelp_bg_start(app, server, viewer)
  return(.env$fuzzyhelp)
}

fuzzyhelp_bg_view <- function(viewer) {
  if (
    !is.null(.env$fuzzyhelp) &&
      is.null(.env$fuzzyhelp$get_exit_status()) &&
      file.exists(.env$fuzzyhelp_url)
  ) {
    url <- readLines(.env$fuzzyhelp_url)[1L]
    if (url != "") {
      viewer(url)
      return(TRUE)
    }
  }
  return(FALSE)
}

fuzzyhelp_bg_start <- function(app, server, viewer) {
  writeLines("", .env$fuzzyhelp_url) # Ensure content is empty
  callr::r_bg(
    function(..., .env, base_viewer, base_options) {
      do.call(options, base_options)
      viewer <- function(url) {
        writeLines(url, .env$fuzzyhelp_url)
        base_viewer(url)
      }
      shiny::runGadget(..., viewer = viewer)
    },
    args = list(
      app = app,
      server = server,
      .env = .env,
      base_viewer = viewer,
      base_options = options()
    ),
    env = Sys.getenv(),
    package = TRUE
  )
}

fuzzyhelp_addin <- function() {
  fuzzyhelp(background = TRUE)
}
