here_root <- normalizePath("../..")
db_path <- file.path(here_root, "data/loto.sqlite")
skip_if_not(file.exists(db_path), "database not built")

suppressPackageStartupMessages({
  library(shiny)
  library(bslib)
  library(DT)
  library(ggplot2)
  library(dplyr)
})
for (f in c("etl", "db", "stats", "generator", "download", "pipeline", "format")) {
  source(file.path(here_root, "R", paste0(f, ".R")))
}
for (f in list.files(file.path(here_root, "app/modules"), pattern = "\\.R$", full.names = TRUE)) source(f)

test_that("format_draws gives most recent first, in French", {
  d <- with_db(db_path, function(con) load_draws(con, "euromillions"))
  out <- format_draws(d, "euromillions", 3)
  expect_equal(names(out), c("Date", "Numéros", "Étoiles"))
  expect_equal(nrow(out), 3)
  expect_equal(out$Date[1], format(max(d$date), "%d/%m/%Y"))
})

test_that("home module renders", {
  testServer(mod_home_server, args = list(db_path = db_path, version = reactiveVal(0)), {
    expect_no_error(output$cards)
    expect_no_error(output$last_loto)
    expect_no_error(output$last_euromillions)
  })
})

test_that("stats module renders for every game / kind / period", {
  testServer(mod_stats_server, args = list(db_path = db_path, version = reactiveVal(0)), {
    for (game in c("loto", "euromillions")) {
      for (kind in c("ball", "bonus")) {
        for (period in c("all", "5")) {
          session$setInputs(game = game, kind = kind, period = period, feature = "n_odd")
          expect_no_error(output$freq_plot)
          expect_match(output$chi2, "p = ")
          expect_no_error(output$gaps)
          expect_no_error(output$pairs_note)
          if (kind == "ball") expect_no_error(output$pairs)
          expect_no_error(output$feature_plot)
          expect_match(output$feature_chi2, "p = ")
        }
      }
    }
    session$setInputs(feature = "n_low")
    expect_match(output$feature_chi2, "p = ")
  })
})

test_that("backtest module renders", {
  testServer(mod_backtest_server, args = list(db_path = db_path, version = reactiveVal(0)), {
    expect_no_error(output$verdict)
    expect_no_error(output$plot)
    expect_no_error(output$table)
  })
})

test_that("generator module produces grids and reports impossible constraints", {
  testServer(mod_generator_server, args = list(db_path = db_path, version = reactiveVal(0)), {
    session$setInputs(
      game = "euromillions", strategy = "overdue", n = 4, use_sum = FALSE, use_odd = FALSE,
      sum_range = c(100, 150), odd_range = c(2, 3), seed = 1, go = 1
    )
    expect_equal(nrow(display()), 4)
    expect_equal(names(display())[2], "Étoiles")
  })
})

test_that("data module lists the loaded games", {
  testServer(mod_data_server, args = list(db_path = db_path, root = here_root, version = reactiveVal(0)), {
    expect_no_error(output$meta)
  })
})
