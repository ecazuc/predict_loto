source("R/db.R")
source("R/stats.R")
options(width = 120)

con <- DBI::dbConnect(RSQLite::SQLite(), "data/loto.sqlite")

for (game in names(game_rules)) {
  rules <- game_rules[[game]]
  draws <- load_draws(con, game)
  nums <- load_numbers(con, game)
  cat("\n==========", toupper(game), "-", nrow(draws), "draws ==========\n")

  freq <- number_frequency(nums, "ball", rules$max_ball)
  cat("\nBalls - chi-square vs uniform:\n")
  print(as.data.frame(uniformity_test(freq)))
  cat("Most / least drawn balls (z = deviation from expected):\n")
  print(as.data.frame(rbind(head(arrange(freq, desc(n)), 3), tail(arrange(freq, desc(n)), 3))), digits = 3)

  # stars changed range over time: only the era with the current range is comparable
  since <- if (game == "euromillions") as.Date("2016-09-27") else NULL
  bonus <- number_frequency(nums, "bonus", rules$max_bonus, since = since)
  cat("\nBonus numbers", if (!is.null(since)) paste("since", since), "- chi-square vs uniform:\n")
  print(as.data.frame(uniformity_test(bonus)))

  cat("\nLongest current gaps (draws since last seen):\n")
  gaps <- number_gaps(nums, "ball", rules$max_ball)
  print(as.data.frame(head(arrange(gaps, desc(current_gap)), 5)), digits = 3)

  cat("\nMost frequent pairs:\n")
  print(as.data.frame(pair_counts(draws, rules$max_ball, top = 5)), digits = 3)

  ft <- draw_features(draws, rules$max_ball)
  for (feat in c("n_odd", "n_low")) {
    tab <- feature_vs_expected(ft, feat, rules$max_ball)
    cat("\n", feat, "observed vs hypergeometric expectation:\n")
    print(as.data.frame(mutate(tab, expected = round(expected, 1))))
    print(as.data.frame(feature_test(tab)))
  }
  cat("\nSum of balls: mean", round(mean(ft$sum), 1), "sd", round(sd(ft$sum), 1),
      "| draws with >=1 consecutive pair:", round(100 * mean(ft$n_consecutive > 0), 1), "%\n")
}
DBI::dbDisconnect(con)
