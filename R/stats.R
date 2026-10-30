# Descriptive statistics on draws. Every function is pure (data frames in, data frame out).
# Inputs: `draws` = wide draw table (see etl.R), `numbers` = long table (see db.R::to_long).

library(dplyr)

# bonus_since: first date of the current bonus range (Euromillions stars were 1-9, 1-11, then 1-12)
game_rules <- list(
  loto = list(max_ball = 49L, max_bonus = 10L, n_bonus = 1L, bonus_since = as.Date(NA)),
  euromillions = list(max_ball = 50L, max_bonus = 12L, n_bonus = 2L, bonus_since = as.Date("2016-09-27"))
)

load_draws <- function(con, game) {
  DBI::dbGetQuery(con, paste0("select * from draws_", game, " order by date, draw_id")) |>
    dplyr::mutate(date = as.Date(date))
}

load_numbers <- function(con, game) {
  DBI::dbGetQuery(
    con, "select * from drawn_numbers where game = ? order by date, draw_id",
    params = list(game)
  ) |>
    dplyr::mutate(date = as.Date(date))
}

#' Frequency of each number vs. the uniform expectation.
#'
#' Balls: each draw picks `n_per_draw` distinct numbers out of `max_number`, so a number
#' appears with probability n_per_draw / max_number in each draw. `z` is the standardised
#' deviation under a binomial model (|z| > 3 on one number out of ~50 would be surprising).
number_frequency <- function(numbers, kind = "ball", max_number, since = NULL) {
  x <- numbers |> filter(.data$kind == !!kind)
  if (!is.null(since)) x <- filter(x, date >= since)
  n_draws <- n_distinct(x$draw_id)
  p <- n_distinct(x$position) / max_number
  tibble::tibble(number = seq_len(max_number)) |>
    left_join(count(x, number, name = "n"), by = "number") |>
    mutate(
      n = coalesce(n, 0L),
      expected = n_draws * p,
      pct = n / n_draws,
      z = (n - expected) / sqrt(n_draws * p * (1 - p))
    )
}

#' Chi-square goodness-of-fit of the observed frequencies against uniform.
#' Draws pick distinct numbers (weak negative dependence), so the test is approximate.
uniformity_test <- function(freq) {
  t <- suppressWarnings(chisq.test(freq$n, p = rep(1 / nrow(freq), nrow(freq))))
  tibble::tibble(statistic = unname(t$statistic), df = unname(t$parameter), p_value = t$p.value)
}

#' Current gap (draws since last appearance) vs. the average gap of each number.
number_gaps <- function(numbers, kind = "ball", max_number) {
  x <- numbers |> filter(.data$kind == !!kind)
  draw_index <- x |>
    distinct(draw_id, date) |>
    arrange(date, draw_id) |>
    mutate(idx = row_number())
  n_draws <- nrow(draw_index)
  x |>
    inner_join(draw_index, by = c("draw_id", "date")) |>
    distinct(number, idx) |>
    group_by(number) |>
    arrange(idx, .by_group = TRUE) |>
    summarise(
      times = n(),
      current_gap = n_draws - max(idx),
      avg_gap = n_draws / n(),
      max_gap = max(diff(c(0L, idx, n_draws + 1L))) - 1L,
      .groups = "drop"
    ) |>
    right_join(tibble::tibble(number = seq_len(max_number)), by = "number") |>
    arrange(number)
}

#' Most frequent pairs of balls vs. the expected count under independence.
#' A given pair appears in a draw with probability C(N-2, k-2) / C(N, k) = k(k-1) / (N(N-1)).
pair_counts <- function(draws, max_ball, top = 20) {
  balls <- as.matrix(draws[paste0("n", 1:5)])
  pairs <- t(apply(balls, 1, function(r) {
    r <- sort(r)
    combn(r, 2, FUN = function(p) p, simplify = TRUE) |> as.vector()
  }))
  long <- data.frame(
    a = as.vector(pairs[, seq(1, ncol(pairs), 2)]),
    b = as.vector(pairs[, seq(2, ncol(pairs), 2)])
  )
  expected <- nrow(draws) * 5 * 4 / (max_ball * (max_ball - 1))
  long |>
    count(a, b, name = "n") |>
    mutate(expected = expected, ratio = n / expected) |>
    arrange(desc(n)) |>
    head(top)
}

#' Per-draw features: sum, odd count, low count (<= half of the range), consecutive pairs, spread.
draw_features <- function(draws, max_ball) {
  balls <- as.matrix(draws[paste0("n", 1:5)])
  draws |>
    mutate(
      sum = rowSums(balls),
      n_odd = rowSums(balls %% 2 == 1),
      n_low = rowSums(balls <= max_ball %/% 2),
      n_consecutive = apply(balls, 1, function(r) sum(diff(sort(r)) == 1)),
      spread = apply(balls, 1, function(r) max(r) - min(r))
    ) |>
    select(game, draw_id, date, sum, n_odd, n_low, n_consecutive, spread)
}

#' Observed vs. theoretical (hypergeometric) distribution of a count feature (n_odd or n_low).
feature_vs_expected <- function(features, feature = c("n_odd", "n_low"), max_ball) {
  feature <- match.arg(feature)
  n_special <- if (feature == "n_odd") (max_ball + 1L) %/% 2L else max_ball %/% 2L
  obs <- count(features, value = .data[[feature]], name = "n")
  tibble::tibble(value = 0:5) |>
    left_join(obs, by = "value") |>
    mutate(
      n = coalesce(n, 0L),
      expected = nrow(features) * dhyper(value, n_special, max_ball - n_special, 5)
    )
}

#' Chi-square test of a feature_vs_expected() table (categories with expected < 5 are pooled).
feature_test <- function(tab) {
  small <- tab$expected < 5
  obs <- c(tab$n[!small], sum(tab$n[small]))
  exp_p <- c(tab$expected[!small], sum(tab$expected[small])) / sum(tab$expected)
  keep <- exp_p > 0
  t <- suppressWarnings(chisq.test(obs[keep], p = exp_p[keep]))
  tibble::tibble(statistic = unname(t$statistic), df = unname(t$parameter), p_value = t$p.value)
}
