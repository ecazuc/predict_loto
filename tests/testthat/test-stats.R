here_root <- normalizePath("../..")
source(file.path(here_root, "R/db.R"))
source(file.path(here_root, "R/stats.R"))

toy <- tibble::tibble(
  game = "loto", draw_id = c("1", "2", "3", "4"),
  date = as.Date("2020-01-01") + 0:3,
  n1 = c(1L, 1L, 2L, 3L), n2 = c(2L, 3L, 3L, 4L), n3 = c(3L, 5L, 4L, 5L),
  n4 = c(4L, 6L, 6L, 7L), n5 = c(5L, 7L, 8L, 9L),
  bonus1 = c(1L, 2L, 2L, 3L), bonus2 = NA_integer_
)
toy_long <- to_long(toy)

test_that("number_frequency counts appearances and expectations", {
  f <- number_frequency(toy_long, "ball", max_number = 10)
  expect_equal(nrow(f), 10)
  expect_equal(f$n[f$number == 3], 4L)
  expect_equal(f$n[f$number == 10], 0L)
  expect_equal(sum(f$n), 20L)
  expect_equal(f$expected[1], 4 * 5 / 10)
  expect_equal(sum(f$expected), 20)
})

test_that("number_gaps gives current and maximum gaps", {
  g <- number_gaps(toy_long, "ball", max_number = 10)
  expect_equal(g$current_gap[g$number == 3], 0L) # in last draw
  expect_equal(g$current_gap[g$number == 1], 2L) # last seen in draw 2 of 4
  expect_equal(g$max_gap[g$number == 1], 2L)
  expect_true(is.na(g$times[g$number == 10]))
})

test_that("draw_features computes sum, parity and consecutive pairs", {
  ft <- draw_features(toy, max_ball = 10)
  expect_equal(ft$sum, c(15, 22, 23, 28))
  expect_equal(ft$n_odd, c(3, 4, 1, 4))
  expect_equal(ft$n_consecutive[1], 4L) # 1-2-3-4-5
  expect_equal(ft$spread[1], 4)
})

test_that("pair_counts finds the most common pair", {
  p <- pair_counts(toy, max_ball = 10, top = 3)
  expect_equal(p$n[1], 3L) # (3,4) and (3,5) each appear in 3 draws
  expect_equal(p$expected[1], 4 * 20 / 90)
})

test_that("feature_vs_expected sums to the number of draws", {
  ft <- draw_features(toy, max_ball = 49)
  tab <- feature_vs_expected(ft, "n_odd", max_ball = 49)
  expect_equal(sum(tab$n), 4L)
  expect_equal(sum(tab$expected), 4)
})

test_that("uniform data is not rejected, biased data is", {
  set.seed(1)
  draws <- replicate(3000, sample(49, 5), simplify = FALSE)
  unif <- tibble::tibble(number = 1:49, n = tabulate(unlist(draws), 49))
  expect_gt(uniformity_test(unif)$p_value, 0.01)
  biased <- unif
  biased$n[1] <- biased$n[1] + 400
  expect_lt(uniformity_test(biased)$p_value, 0.001)
})

test_that("real data: balls look uniform", {
  db <- file.path(here_root, "data/loto.sqlite")
  skip_if_not(file.exists(db), "database not built")
  con <- DBI::dbConnect(RSQLite::SQLite(), db)
  on.exit(DBI::dbDisconnect(con))
  nums <- load_numbers(con, "loto")
  f <- number_frequency(nums, "ball", max_number = 49)
  expect_equal(sum(f$n), 5L * n_distinct(nums$draw_id))
  expect_gt(uniformity_test(f)$p_value, 0.001)
})
