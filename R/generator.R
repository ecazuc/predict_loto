# Grid generator.
#
# Every combination has exactly the same probability of winning, and the backtest shows no
# strategy beats chance. The only lever that matters is the payout: avoiding combinations many
# people play (birthdays, runs, arithmetic sequences) lowers the chance of sharing a win.
# FDJ publishes no data on what players pick, so "popular" below is a heuristic.

strategy_names <- c("random", "hot", "cold", "overdue", "avoid_popular")

#' Heuristic weights: many players use dates, so 1-31 (and 1-12 even more) are down-weighted.
popular_weights <- function(max_number) {
  w <- rep(1, max_number)
  w[seq_len(min(31L, max_number))] <- 0.7
  w[seq_len(min(12L, max_number))] <- 0.7 * 0.85
  w
}

#' TRUE for grids following patterns that are over-played.
is_popular_pattern <- function(balls) {
  b <- sort(balls)
  d <- diff(b)
  all(b <= 31) || any(d[-length(d)] == 1 & d[-1] == 1) || length(unique(d)) == 1
}

#' Sampling weights of each number for a strategy, from past draws.
strategy_weights <- function(strategy, numbers, kind, max_number, since = as.Date(NA)) {
  strategy <- match.arg(strategy, strategy_names)
  if (!is.na(since)) numbers <- numbers[numbers$date >= since, ]
  switch(strategy,
    random = rep(1, max_number),
    avoid_popular = if (kind == "ball") popular_weights(max_number) else rep(1, max_number),
    hot = number_frequency(numbers, kind, max_number)$n + 1,
    cold = {
      n <- number_frequency(numbers, kind, max_number)$n
      max(n) + 1 - n
    },
    overdue = {
      gap <- number_gaps(numbers, kind, max_number)$current_gap
      gap[is.na(gap)] <- dplyr::n_distinct(numbers$draw_id[numbers$kind == kind])
      gap + 1
    }
  )
}

grid_ok <- function(balls, constraints, avoid_patterns, max_ball) {
  in_range <- function(v, r) is.null(r) || (v >= r[1] && v <= r[2])
  in_range(sum(balls), constraints$sum_range) &&
    in_range(sum(balls %% 2 == 1), constraints$odd_range) &&
    in_range(sum(balls <= max_ball %/% 2), constraints$low_range) &&
    !(avoid_patterns && is_popular_pattern(balls))
}

#' Generate `n` distinct grids.
#'
#' @param numbers long table from load_numbers().
#' @param constraints optional list of ranges: sum_range, odd_range, low_range (each c(min, max)).
#' @param avoid_patterns reject over-played patterns (default: only for "avoid_popular").
generate_grids <- function(game, n = 5, strategy = "random", numbers = NULL, constraints = list(),
                           avoid_patterns = identical(strategy, "avoid_popular"), seed = NULL,
                           max_tries = 20000) {
  strategy <- match.arg(strategy, strategy_names)
  rules <- game_rules[[game]]
  if (!is.null(seed)) set.seed(seed)
  if (strategy %in% c("hot", "cold", "overdue") && is.null(numbers)) {
    stop("strategy '", strategy, "' needs the `numbers` history")
  }
  w_ball <- strategy_weights(strategy, numbers, "ball", rules$max_ball)
  w_bonus <- strategy_weights(strategy, numbers, "bonus", rules$max_bonus, rules$bonus_since)

  grids <- list()
  tries <- 0
  while (length(grids) < n) {
    tries <- tries + 1
    if (tries > max_tries) stop("could not satisfy the constraints in ", max_tries, " tries")
    balls <- sort(sample.int(rules$max_ball, 5, prob = w_ball))
    if (!grid_ok(balls, constraints, avoid_patterns, rules$max_ball)) next
    bonus <- sort(sample.int(rules$max_bonus, rules$n_bonus, prob = w_bonus))
    key <- paste(c(balls, bonus), collapse = "-")
    if (key %in% names(grids)) next
    grids[[key]] <- c(balls, bonus, rep(NA_integer_, 2 - rules$n_bonus))
  }
  out <- as.data.frame(do.call(rbind, unname(grids)))
  names(out) <- c(paste0("n", 1:5), "bonus1", "bonus2")
  tibble::as_tibble(out) |>
    dplyr::mutate(
      strategy = strategy,
      sum = rowSums(dplyr::across(n1:n5)),
      n_odd = rowSums(dplyr::across(n1:n5) %% 2 == 1),
      n_low = rowSums(dplyr::across(n1:n5) <= rules$max_ball %/% 2),
      .before = 1
    )
}
