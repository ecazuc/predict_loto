# French labels and display helpers shared by the Shiny app.

game_labels <- c(loto = "Loto", euromillions = "Euromillions")
bonus_labels <- c(loto = "Numéro chance", euromillions = "Étoiles")

strategy_labels <- c(
  random = "Aléatoire",
  hot = "Numéros chauds (les plus fréquents)",
  cold = "Numéros froids (les moins fréquents)",
  overdue = "Numéros en retard",
  avoid_popular = "Éviter les grilles populaires"
)

backtest_labels <- c(
  random = "Aléatoire (témoin)",
  hot_all = "Les plus fréquents (tout l'historique)",
  cold_all = "Les moins fréquents (tout l'historique)",
  hot_recent = "Les plus fréquents (100 derniers tirages)",
  overdue = "Les plus en retard"
)

#' Draws as a display table: most recent first, balls and bonus as text.
format_draws <- function(draws, game, n = 10) {
  d <- utils::tail(draws[order(draws$date), ], n)
  d <- d[order(d$date, decreasing = TRUE), ]
  balls <- apply(d[paste0("n", 1:5)], 1, function(r) paste(sort(r), collapse = " - "))
  bonus <- if (game == "loto") as.character(d$bonus1) else paste(d$bonus1, d$bonus2, sep = " - ")
  out <- data.frame(format(d$date, "%d/%m/%Y"), balls, bonus)
  names(out) <- c("Date", "Numéros", bonus_labels[[game]])
  out
}
