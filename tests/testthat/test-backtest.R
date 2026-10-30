here_root <- normalizePath("../..")
source(file.path(here_root, "R/backtest.R"))

simulate_x <- function(n_draws, n_numbers, k, weights = rep(1, n_numbers), seed = 42) {
  set.seed(seed)
  x <- matrix(0L, n_draws, n_numbers)
  for (t in seq_len(n_draws)) x[t, sample.int(n_numbers, k, prob = weights)] <- 1L
  x
}

test_that("build_presence marks drawn balls and bonus numbers", {
  d <- data.frame(
    n1 = 1L, n2 = 2L, n3 = 3L, n4 = 4L, n5 = 10L, bonus1 = 2L, bonus2 = NA_integer_
  )
  expect_equal(which(build_presence(d, "ball", 10)[1, ] == 1), c(1, 2, 3, 4, 10))
  expect_equal(which(build_presence(d, "bonus", 5)[1, ] == 1), 2)
})

test_that("strategies only use past draws (no look-ahead)", {
  x <- simulate_x(300, 20, 5)
  h1 <- run_backtest(x, burn_in = 100)
  x2 <- x
  x2[300, ] <- 0L # change the last draw only
  x2[300, 1:5] <- 1L
  h2 <- run_backtest(x2, burn_in = 100)
  # picks for draws 101..299 depend on past only, so hits there must be identical
  expect_equal(h1[101:299, ], h2[101:299, ])
})

test_that("on uniform random draws no strategy beats chance", {
  x <- simulate_x(3000, 49, 5)
  s <- summarise_backtest(run_backtest(x))
  expect_true(all(s$p_adjusted > 0.01))
  expect_equal(s$expected[1], 25 / 49)
})

test_that("the backtest detects a real bias when one exists", {
  w <- rep(1, 49)
  w[1:5] <- 4 # five numbers are 4x more likely
  x <- simulate_x(3000, 49, 5, weights = w)
  s <- summarise_backtest(run_backtest(x))
  expect_lt(s$p_adjusted[s$strategy == "hot_all"], 1e-6)
  expect_gt(s$mean_hits[s$strategy == "hot_all"], s$expected[1])
  expect_lt(s$mean_hits[s$strategy == "cold_all"], s$expected[1])
})
