# Walk-forward backtest of "prediction" strategies against the chance baseline.
#
# For each draw t after a burn-in, every strategy picks k numbers using ONLY draws before t,
# and we count how many of them are in draw t. Under pure chance the hits of any strategy are
# hypergeometric(N, k, k): mean k^2/N. Because the pick at t is fixed before draw t happens,
# per-draw hits are a martingale-difference sequence under the null, so a z-test on the mean is valid.

#' 0/1 matrix (draws x numbers) of which numbers were drawn.
build_presence <- function(draws, kind = c("ball", "bonus"), max_number) {
  kind <- match.arg(kind)
  cols <- if (kind == "ball") paste0("n", 1:5) else c("bonus1", "bonus2")
  m <- as.matrix(draws[cols])
  x <- matrix(0L, nrow(m), max_number)
  for (j in seq_len(ncol(m))) {
    ok <- !is.na(m[, j])
    x[cbind(which(ok), m[ok, j])] <- 1L
  }
  x
}

# highest scores first; ties broken at random so low numbers are not favoured
top_k <- function(score, k) order(-score, sample.int(length(score)))[seq_len(k)]

#' Strategies receive the state `s` (counts, recent counts, gaps) built from past draws only.
backtest_strategies <- list(
  random = function(s) sample.int(s$n_numbers, s$k),
  hot_all = function(s) top_k(s$counts, s$k),
  cold_all = function(s) top_k(-s$counts, s$k),
  hot_recent = function(s) top_k(s$recent, s$k),
  overdue = function(s) top_k(s$gap, s$k)
)

#' Hits of each strategy at each draw (NA during burn-in).
run_backtest <- function(x, strategies = backtest_strategies, burn_in = 200, recent_window = 100, seed = 1) {
  set.seed(seed)
  n_draws <- nrow(x)
  n_numbers <- ncol(x)
  k <- unique(rowSums(x))
  stopifnot(length(k) == 1, n_draws > burn_in)
  cum <- rbind(0L, apply(x, 2, cumsum)) # cum[t, ] = counts over draws 1..t-1
  last_seen <- rep(0L, n_numbers)
  hits <- matrix(NA_integer_, n_draws, length(strategies), dimnames = list(NULL, names(strategies)))
  for (t in seq_len(n_draws)) {
    if (t > burn_in) {
      s <- list(
        n_numbers = n_numbers, k = k, counts = cum[t, ],
        recent = cum[t, ] - cum[max(1L, t - recent_window), ],
        gap = (t - 1L) - last_seen
      )
      for (j in seq_along(strategies)) hits[t, j] <- sum(x[t, strategies[[j]](s)])
    }
    last_seen[x[t, ] == 1L] <- t
  }
  attr(hits, "n_numbers") <- n_numbers
  attr(hits, "k") <- k
  hits
}

#' Mean hits vs. the hypergeometric expectation, z-test, Bonferroni-adjusted p-value.
summarise_backtest <- function(hits) {
  n_numbers <- attr(hits, "n_numbers")
  k <- attr(hits, "k")
  hits <- hits[stats::complete.cases(hits), , drop = FALSE]
  n <- nrow(hits)
  mu <- k * k / n_numbers
  var1 <- k * (k / n_numbers) * (1 - k / n_numbers) * (n_numbers - k) / (n_numbers - 1)
  m_tests <- sum(colnames(hits) != "random")
  tibble::tibble(
    strategy = colnames(hits),
    n_draws = n,
    mean_hits = colMeans(hits),
    expected = mu,
    z = (mean_hits - mu) / sqrt(var1 / n),
    p_value = 2 * stats::pnorm(-abs(z)),
    p_adjusted = pmin(1, p_value * m_tests)
  )
}
