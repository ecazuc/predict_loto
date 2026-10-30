source("R/db.R")
source("R/stats.R")
source("R/backtest.R")
options(width = 120)

con <- DBI::dbConnect(RSQLite::SQLite(), "data/loto.sqlite")
star_era_start <- as.Date("2016-09-27") # first draw with stars 1-12

targets <- list(
  list(game = "loto", kind = "ball", label = "Loto balls", since = NULL),
  list(game = "loto", kind = "bonus", label = "Loto chance number", since = NULL),
  list(game = "euromillions", kind = "ball", label = "Euromillions balls", since = NULL),
  list(
    game = "euromillions", kind = "bonus", label = "Euromillions stars (12-star era)",
    since = star_era_start
  )
)

results <- lapply(targets, function(tg) {
  draws <- load_draws(con, tg$game)
  if (!is.null(tg$since)) draws <- draws[draws$date >= tg$since, ]
  rules <- game_rules[[tg$game]]
  max_number <- if (tg$kind == "ball") rules$max_ball else rules$max_bonus
  x <- build_presence(draws, tg$kind, max_number)
  out <- summarise_backtest(run_backtest(x))
  out$label <- tg$label
  out$game <- tg$game
  out$kind <- tg$kind
  out
})
results <- dplyr::bind_rows(results)

for (lab in unique(results$label)) {
  cat("\n====", lab, "====\n")
  print(as.data.frame(results[results$label == lab, c(
    "strategy", "n_draws", "mean_hits", "expected", "z", "p_value", "p_adjusted"
  )]), digits = 3)
}

DBI::dbWriteTable(con, "backtest_results", results, overwrite = TRUE)
DBI::dbDisconnect(con)
