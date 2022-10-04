classify_chars <- function(x) {
  patterns <- "([:blank:]?)([:lower:]?)([:upper:]?)([:number:]?)([/,:;]?)(.?)"
  res <- stringi::stri_match_first_regex(x, patterns)[, -1L, drop = FALSE] != ""
  colnames(res) <- c(
    "white", "lower", "upper", "number", "delimiter", "non_word"
  )
  return(res)
}

#' Calculate available bonuses from 2-grams
#' @noRd
calc_paired_bonus <- function(target_chars) {
  cur <- classify_chars(target_chars)
  n <- length(target_chars)
  pre <- rbind(FALSE, cur[-length(target_chars), , drop = FALSE])
  bonus <- integer(n)

  # these 3 should be bonused if current is not white nor non_word
  # but will ensure it later
  bonus[pre[, "white"]] <- 10L
  bonus[pre[, "delimiter"]] <- 9L
  bonus[pre[, "non_word"]] <- 8L

  # these 2 should happen after the above 3
  bonus[cur[, "white"]] <- 10L
  bonus[cur[, "non_word"]] <- 8L

  # rest
  bonus[pre[, "lower"] & cur[, "upper"]] <- 7L
  bonus[pre[, "number"] & cur[, "number"]] <- 7L

  # return
  return(bonus)
}

filter_match_matrix <- function(x) {
  nc <- ncol(x)
  nr <- nrow(x)
  if (nc == 1L || nr == 1L) {
    return(x)
  }
  for (i in seq(nr - 1L)) {
    idx <- which(x[i, ])[1L]
    if (!is.na(idx)) {
      x[(i + 1L):nr, 1L:idx] <- FALSE
    }
  }
  for (i in seq(nr, 2L)) {
    idx <- rev(which(x[i, ]))[1L]
    if (!is.na(idx)) {
      x[1L:(i - 1L), idx:nc] <- FALSE
    }
  }
  return(x)
}

calc_match_matrix_core <- function(target_chars, query_chars) {
  target_matrix <- matrix(
    target_chars,
    ncol = length(target_chars),
    nrow = length(query_chars),
    byrow = TRUE,
    dimnames = list(query_chars, target_chars)
  )
  match_matrix <- target_matrix == query_chars
  return(match_matrix)
}

#' Test if matched
#'
#' If not `case_sensitive`, then return the logical add of tests with
#' `target_chars` in the input cases and lower cases.
#' @noRd
calc_match_matrix <- function(
    target_chars, query_chars, case_sensitive = FALSE, partial = TRUE
) {
  match_matrix <- filter_match_matrix(calc_match_matrix_core(
    target_chars, query_chars
  ))

  # if case insensitive, compare with lower cases of target_chars
  if (!case_sensitive) {
    lower <- stringi::stri_trans_tolower(target_chars)
    insensitive <- lower != target_chars
    if (any(insensitive)) {
      match_matrix[, insensitive] <- match_matrix[, insensitive] |
        calc_match_matrix_core(lower[insensitive], query_chars)
    }
  }

  # if possible, reduce the size of match_matrix for efficiency
  # 1 2 3 4 5     2 3 4 5
  # F F T F F     F T F F
  # F F F T F ->  F F T F
  # F F F F F     F F F F
  if (!partial) return(match_matrix)

  colanys <- matrixStats::colAnys(match_matrix)
  if (!any(colanys)) return(match_matrix)

  idx <- which(colanys)
  left <- max(1L, idx[1L] - 1L)
  right <- min(ncol(match_matrix), idx[length(idx)] + 1L)
  return(match_matrix[, left:right, drop = FALSE])
}

#' Calculate bonus for each matches
#'
#' Unmatched ones yield `NA`.
#' @noRd
calc_bonus_matrix <- function(match_matrix,
                              target_chars = colnames(match_matrix),
                              extra = TRUE) {
  base <- 16L

  # matches gain the base bonus
  bonus <- match_matrix * base

  # head matches double the base bonus
  bonus[, 1L] <- bonus[, 1L] * 2L

  # continuous matches gain +4 as a bonus
  prev <- match_matrix[-1L, -1L]
  curr <- match_matrix[-nrow(match_matrix), -ncol(match_matrix)]
  cont <- prev & curr
  bonus[-1L, -1L][cont] <- bonus[-1L, -1L][cont] + 4L

  # for the efficient calculation
  bonus[bonus == 0L] <- NA_integer_

  # extra bonuses from the relationships with the matches and their preceding
  if (extra) {
    bonus <- t(t(bonus) + calc_paired_bonus(target_chars))
  }

  # return
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
  resetter <- matrix(
    fillna(rbind(1L, t(resetter))), ncol = ncol(resetter) + 1, byrow = TRUE
  )[, -1]
  resetter[resetter == 1L] <- NA_integer_
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

last_non_zero <- function(x) {
  non_zero <- x != 0
  y <- c(0L, x[non_zero])
  return(y[length(y)])
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
eval_score <- function(score, n) {
  # Ignore some cells by filling zeros:
  # 00 16 13 12 11 10 09    00 16 13 12 00 00 00
  # 00 00 00 29 26 25 24 -> 00 00 00 29 26 25 00
  # 00 00 00 00 00 42 39    00 00 00 00 00 42 39
  backtrace <- score
  backtrace[-nrow(score), -1L][score[-1L, -ncol(score)] > 0] <- 0L

  # Intermediate evaluation
  row_maxs <- matrixStats::rowMaxs(backtrace)
  selected <- backtrace == row_maxs
  begin <- which(selected[1L, ])

  # Evaluation
  list(
    score = last_non_zero(row_maxs),
    length = n,
    begin = begin[[length(begin)]],
    end = which(selected[nrow(score), ])[[1L]]
  )
}

#' Evaluate target without matches
#'
#' The return is structurally same as that of `eval_score`, but values are
#' missing except for the length. This function is used when fuzzy match does
#' not allow omission or mismatch, which is the default behavior of `fzf`.
#' @noRd
eval_nomatch <- function(target_chars, score = NA_integer_) {
  list(
    score = score,
    length = length(target_chars),
    begin = NA_integer_,
    end = NA_integer_
  )
}

#' The core algorithm of fuzzy match
#' @noRd
fzf_core <- function(
  target_chars, query_chars, must_match = TRUE, case_sensitive = FALSE,
  extra_bonus = TRUE
) {
  noscore <- if (is.logical(must_match)) NA_integer_ else must_match
  n_target_chars <- length(target_chars)
  n_query_chars <- length(query_chars)

  # Early return for blank target or query
  if (n_target_chars == 0L || n_query_chars == 0L) {
    return(eval_nomatch(target_chars, noscore))
  }

  # Test match
  matched <- calc_match_matrix(target_chars, query_chars, case_sensitive)

  # Early return when omission or mismatch happens despite of `must_match`
  if (!identical(must_match, FALSE) && (0L %in% rowSums(matched))) {
    return(eval_nomatch(target_chars, noscore))
  }

  # Calculate and evaluate score
  bonus <- calc_bonus_matrix(matched, extra = extra_bonus)
  penalty <- calc_penalty_matrix(matched)
  score <- calc_score_matrix(bonus, penalty)
  eval_score(score, n = n_target_chars)
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

split_chars <- function(x) {
  stringi::stri_split_boundaries(x, type = "character")
}

#' Fuzzy match on one target string
#'
#' @noRd
fzf_one_raw <- function(
    target,
    query_chars_list,
    must_match = TRUE,
    ...
) {
  # prep
  target_chars <- split_chars(target)[[1L]]

  # early return if target is blank
  if (length(target_chars) == 0L) {
    return(eval_nomatch(target_chars))
  }

  # calc
  results <- data.table::rbindlist(lapply(
    query_chars_list, function(x) fzf_core(target_chars, x, must_match)
  ), use.names = FALSE)

  return(results)
}

fzf_one <- function(target, query_chars_list, must_match = TRUE, ...) {
  results <- fzf_one_raw(target, query_chars_list, must_match, ...)
  remove_na <- identical(must_match, FALSE)
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
  queries_chars_list <- split_chars(queries)
  data.table::rbindlist(lapply(
    targets,
    fzf_one,
    query_chars_list = queries_chars_list,
    must_match = must_match,
    case_sensitive = case_sensitive,
    ...
  ), use.names = FALSE)
}
