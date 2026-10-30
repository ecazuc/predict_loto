here_root <- normalizePath("../..")
source(file.path(here_root, "R/db.R"))
source(file.path(here_root, "R/stats.R"))
source(file.path(here_root, "R/generator.R"))

toy_numbers <- function() {
  set.seed(7)
  m <- t(replicate(400, sample.int(49, 5)))
  draws <- tibble::tibble(
    game = "loto", draw_id = as.character(1:400), date = as.Date("2018-01-01") + 3 * (1:400),
    n1 = m[, 1], n2 = m[, 2], n3 = m[, 3], n4 = m[, 4], n5 = m[, 5],
    bonus1 = sample.int(10, 400, replace = TRUE), bonus2 = NA_integer_
  )
  to_long(draws)
}

test_that("grids are valid for both games", {
  for (game in c("loto", "euromillions")) {
    g <- generate_grids(game, n = 20, seed = 1)
    rules <- game_rules[[game]]
    balls <- as.matrix(g[paste0("n", 1:5)])
    expect_equal(nrow(g), 20)
    expect_true(all(balls >= 1 & balls <= rules$max_ball))
    expect_true(all(apply(balls, 1, function(r) length(unique(r)) == 5 && !is.unsorted(r))))
    expect_true(all(g$bonus1 >= 1 & g$bonus1 <= rules$max_bonus))
    expect_equal(anyNA(g$bonus2), game == "loto")
    expect_equal(nrow(dplyr::distinct(g)), 20)
  }
})

test_that("a seed makes generation reproducible", {
  expect_equal(generate_grids("loto", 5, seed = 3), generate_grids("loto", 5, seed = 3))
})

test_that("constraints are honoured", {
  g <- generate_grids("loto", 30, constraints = list(sum_range = c(100, 150), odd_range = c(2, 3)), seed = 2)
  expect_true(all(g$sum >= 100 & g$sum <= 150))
  expect_true(all(g$n_odd %in% 2:3))
  expect_error(generate_grids("loto", 1, constraints = list(sum_range = c(1, 5)), max_tries = 200))
})

test_that("avoid_popular never returns over-played patterns and favours numbers above 31", {
  g <- generate_grids("loto", 300, strategy = "avoid_popular", seed = 4)
  balls <- as.matrix(g[paste0("n", 1:5)])
  expect_false(any(apply(balls, 1, is_popular_pattern)))
  r <- generate_grids("loto", 300, strategy = "random", seed = 4)
  expect_gt(mean(as.matrix(g[paste0("n", 1:5)]) > 31), mean(as.matrix(r[paste0("n", 1:5)]) > 31))
})

test_that("is_popular_pattern detects birthdays, runs and sequences", {
  expect_true(is_popular_pattern(c(2, 9, 14, 23, 30)))
  expect_true(is_popular_pattern(c(5, 6, 7, 40, 49)))
  expect_true(is_popular_pattern(c(10, 20, 30, 40, 50)))
  expect_false(is_popular_pattern(c(3, 17, 32, 41, 48)))
})

test_that("history-based strategies need the history and shift the weights", {
  expect_error(generate_grids("loto", 1, strategy = "hot"))
  nums <- toy_numbers()
  w_hot <- strategy_weights("hot", nums, "ball", 49)
  w_cold <- strategy_weights("cold", nums, "ball", 49)
  expect_gt(cor(w_hot, -w_cold), 0.99)
  g <- generate_grids("loto", 5, strategy = "overdue", numbers = nums, seed = 1)
  expect_equal(nrow(g), 5)
})

test_that("Euromillions stars are two distinct values", {
  g <- generate_grids("euromillions", 50, seed = 5)
  expect_true(all(g$bonus1 != g$bonus2))
})
