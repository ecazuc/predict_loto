here_root <- normalizePath("../..")
source(file.path(here_root, "R/etl.R"))
source(file.path(here_root, "R/db.R"))

test_that("parse_fdj_date handles the three FDJ date formats", {
  expect_equal(
    parse_fdj_date(c("04/03/2017", "23/09/16", "20081004")),
    as.Date(c("2017-03-04", "2016-09-23", "2008-10-04"))
  )
})

test_that("validate_draws flags rule violations", {
  ok <- tibble::tibble(
    game = "loto", draw_id = "1", date = as.Date("2020-01-01"),
    n1 = 1L, n2 = 2L, n3 = 3L, n4 = 4L, n5 = 49L, bonus1 = 10L, bonus2 = NA_integer_
  )
  expect_equal(nrow(validate_draws(ok)), 0)
  expect_equal(nrow(validate_draws(dplyr::mutate(ok, n5 = 50L))), 1)
  expect_equal(nrow(validate_draws(dplyr::mutate(ok, n5 = 4L))), 1) # duplicate ball
  expect_equal(nrow(validate_draws(dplyr::mutate(ok, bonus1 = 11L))), 1)
})

test_that("Euromillions star range depends on the era", {
  e <- tibble::tibble(
    game = "euromillions", draw_id = "1", date = as.Date("2005-01-01"),
    n1 = 1L, n2 = 2L, n3 = 3L, n4 = 4L, n5 = 50L, bonus1 = 1L, bonus2 = 10L
  )
  expect_equal(nrow(validate_draws(e)), 1) # star 10 not allowed in 2005
  expect_equal(nrow(validate_draws(dplyr::mutate(e, date = as.Date("2012-01-01")))), 0)
  expect_equal(nrow(validate_draws(dplyr::mutate(e, date = as.Date("2012-01-01"), bonus2 = 12L))), 1)
  expect_equal(nrow(validate_draws(dplyr::mutate(e, date = as.Date("2020-01-01"), bonus2 = 12L))), 0)
})

test_that("to_long gives 5 balls + bonus rows per draw", {
  d <- tibble::tibble(
    game = "euromillions", draw_id = "1", date = as.Date("2020-01-01"),
    n1 = 1L, n2 = 2L, n3 = 3L, n4 = 4L, n5 = 5L, bonus1 = 6L, bonus2 = 7L
  )
  expect_equal(nrow(to_long(d)), 7)
  expect_equal(nrow(to_long(dplyr::mutate(d, game = "loto", bonus2 = NA_integer_))), 6)
})

test_that("real FDJ data loads and is valid", {
  skip_if_not(dir.exists(file.path(here_root, "data/raw/loto")), "raw data not downloaded")
  old <- setwd(here_root)
  on.exit(setwd(old))
  for (d in list(etl_loto(), etl_euromillions())) {
    expect_equal(nrow(validate_draws(d)), 0)
    expect_false(anyDuplicated(d$draw_id) > 0)
    expect_false(is.unsorted(d$date))
    expect_gt(nrow(d), 1500)
  }
})
