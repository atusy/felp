char_unicode_patterns <- c(
  white = "[:blank:]",
  lower = "[:lower:]",
  upper = "[:upper:]",
  number = "[:number:]",
  delimiter = "[/,:;|]",
  non_word = "."
)

classify_char <- function(x, patterns = char_unicode_patterns) {
  matched <- stringi::stri_detect_regex(x, patterns)
  names(patterns)[matched][1L]
}

classify_chars <- function(x, patterns = char_unicode_patterns) {
  vapply(x, classify_char, NA_character_)
}

#' Calculate available bonuses from 2-grams
#' @noRd
calc_paired_bonus <- function(target_chars) {
  cur <- classify_chars(target_chars)
  pre <- dplyr::lag(cur, default = "_unk_")
  is_white <- cur == "white"
  is_non_word <- cur == "non_word"
  bonus <- (
    (!(is_white | is_non_word)) * (
      10L * (pre == "white") +
        9L * (pre == "delimiter") +
        8L * (pre == "non_word")
    ) +
      (7L * (paste(pre, cur) %in% c("lower upper", "number number"))) +
      (8L * is_non_word) +
      (10L * is_white)
  )
  bonus
}

#' Test if matched
#'
#' If not `case_sensitive`, then return the logical add of tests with
#' `target_chars` in the input cases and lower cases.
#' @noRd
calc_match_matrix <- function(
    target_chars, query_chars, case_sensitive = FALSE
) {
  target_matrix <- matrix(
    target_chars,
    ncol = length(target_chars),
    nrow = length(query_chars),
    byrow = TRUE,
    dimnames = list(query_chars, target_chars)
  )
  match_matrix <- target_matrix == query_chars

  if (case_sensitive) return(match_matrix)
  match_matrix | calc_match_matrix(
    stringi::stri_trans_tolower(target_chars),
    query_chars,
    case_sensitive = TRUE
  )
}

#' Calculate bonus for each matches
#'
#' Unmatched ones yield `NA`.
calc_bonus_matrix <- function(match_matrix, target_chars) {
  base <- 16L
  mult <- match_matrix
  mult[match_matrix[, 1L], 1L] <- 2L
  mult[!match_matrix] <- NA_integer_  # for the efficient calculation
  bonus <- t(t(mult * base) + calc_paired_bonus(target_chars))
  bonus
}

#' Calculate penalty
#'
#' The penalty starts from -3 and decrements by -1.
#'
#' @note
#'
#' Example of internal variables:
#'
#' ```
#' unmatch:   T  F  T  F  T  T  T
#' locals:   -1  0 -3  0 -3 -1 -2
#' cumsum:   -1 -1 -4 -4 -7 -8 -9
#' resetter: NA -1 -1 -4 -4 -4 -4
#' penalty:  NA  0 -3  0 -3 -4 -5
#' ```
#'
#' @noRd
calc_penalty_matrix <- function(match_matrix) {
  unmatch <- !match_matrix
  is_boundary <- unmatch[, -1L] & match_matrix[, -ncol(match_matrix)]

  locals <- -unmatch
  locals[, -1L][is_boundary] <- -3L

  cumsums <- matrixStats::rowCumsums(locals)
  resetter <- cumsums * match_matrix
  resetter[unmatch] <- NA_integer_
  resetter <- t(tidyr::fill(
    as.data.frame(t(unname(resetter))),
    tidyselect::everything())
  )
  penalty <- cumsums - resetter
  penalty
}

#' Fill missing values with last observations
#' @noRd
fillna <- function(x) {
  data.table::nafill(x, "locf")
}

#' Calculate score
#'
#' The core does following steps for each rows:
#' (1) cells with matches add itself and the score from the upper-left cell,
#' (2) cells without matches inherit scores from the left,
#' and
#' (3) cells without matches gains penalty.
#' @noRd
calc_score_matrix <- function(bonus, penalty) {
  # prep
  nc <- ncol(bonus)
  nr <- nrow(bonus)
  s <- rbind(0L, bonus)  # zeros as initial scores

  # core
  for (i in seq(nr)) {
    s[i + 1L, ] <- fillna(s[i + 1L, ] + c(0L, s[i, -nc])) + penalty[i, ]
  }

  # normalization
  s[s < 0L | is.na(s)] <- 0L  # should not be missing or negative
  s[-1L, , drop = FALSE]  # remove initial scores while keeping the matrix type
}

#' Evaluate score
#'
#' @return
#' A named integer vector
#'
#' score: maximum value of the input score
#' length: length of the target string
#' begin: index of the first match
#' end: index of the last match
#'
#' @noRd
eval_score <- function(score) {
  # Ignore some cells by filling zeros:
  # 00 16 13 12 11 10 09    00 16 13 12 00 00 00
  # 00 00 00 29 26 25 24 -> 00 00 00 29 26 25 00
  # 00 00 00 00 00 42 39    00 00 00 00 00 42 39
  backtrace <- score
  backtrace[-nrow(score), -1L][score[-1L, -ncol(score)] > 0] = 0L

  # Intermediate evaluation
  row_maxs <- matrixStats::rowMaxs(backtrace)
  selected <- backtrace == row_maxs
  begin <- which(selected[1L, ])

  # Evaluation
  c(
    score = max(row_maxs),
    length = ncol(score),
    begin = begin[[length(begin)]],
    end = which(selected[nrow(score), ])[[1L]]
  )
}

#' Evaluate target without matches
#'
#' The return is structurally same as that of `eval_score`, but values are
#' missing except for the length. This function is used when fuzzy match does
#' not allow omission or mismatch, which is the default behavior of `fzf`.
eval_nomatch <- function(target_chars) {
  c(
    score = NA_integer_,
    length = length(target_chars),
    begin = NA_integer_,
    end = NA_integer_
  )
}

#' The core algorithm of fuzzy match
#' @noRd
fzf_core <- function(
    target_chars, query_chars, must_match = TRUE, case_sensitive = FALSE
) {
  # Early return for blank target or query
  if (0L %in% c(length(target_chars), length(query_chars))) {
    return(eval_nomatch(target_chars))
  }

  # Test match
  matched <- calc_match_matrix(target_chars, query_chars, case_sensitive)

  # Early return when omission or mismatch happens despite of `must_match`
  if (must_match && (0L %in% rowSums(matched))) {
    return(eval_nomatch(target_chars))
  }

  # Calculate and evaluate score
  bonus <- calc_bonus_matrix(matched, target_chars)
  penalty <- calc_penalty_matrix(matched)
  score <- calc_score_matrix(bonus, penalty)
  eval_score(score)
}

#' Summarize
#'
#' Wraps `sum`, `min`, or `max` to return `NA` when all the elements of `x` are
#' `NA` and `na.rm = TRUE`.
#'
#' @noRd
summarize <- function(x, fun, remove_na) {
  fun(x, na.rm = remove_na && !all(is.na(x)))[[1L]]  # [[1L]] ensures un-named
}

#' Fuzzy match on one target string
#'
#' @noRd
fzf_one <- function(
    target,
    query_chars_list,
    must_match = TRUE,
    ...
) {
  # prep
  target_chars <- stringi::stri_split_boundaries(
    target, type = "character"
  )[[1L]]

  # early return if target is blank
  if (length(target_chars) == 0L) {
    return(eval_nomatch(target_chars))
  }

  # calc
  results <- purrr::map_dfr(
    query_chars_list, function(x) fzf_core(target_chars, x, must_match)
  )

  # summarize
  remove_na <- !must_match
  list(
    score = summarize(results[["score"]], sum, remove_na),
    length = results[1L, "length"][[1L]],
    begin = summarize(results[["begin"]], min, remove_na),
    end = summarize(results[["end"]], max, remove_na),
    order = list(results[["score"]])
  )
}

#' Fuzzy match
#'
#' @param targets,queries Character vectors.
#' @param must_match
#'  The default, `TRUE`, denies omission and mismatch of queries in targets.
#' @param case_sensitive
#'  The default, `FALSE`, allows lower-case characters of queries to match with
#'  their upper-case characters in the targets.
#'
#' @return A data frame whose rows correspond to `targets` and whose columns
#'  constitutes of followings.
#'
#'  - score: The sum of the scores from the each queries
#'  - length: The length of the target
#'  - begin: The minimum index that begins the match from the each queries
#'  - end: The maximum index that ends the match from the each queries
#'  - order: The order of scores from the each queries
#'
#'  The missing values in columns except "length" occur when `must_match` is
#'  `TRUE` (default) and targets have omission or mismatch of queries.
#'
#' @noRd
fzf <- function(
    targets, queries, must_match = TRUE, case_sensitive = FALSE, ...
) {
  queries_chars_list <- stringi::stri_split_boundaries(
    queries, type = "character"
  )
  purrr::map_dfr(
    targets,
    fzf_one,
    query_chars_list = queries_chars_list,
    must_match = must_match,
    case_sensitive = case_sensitive,
    ...
  )
}