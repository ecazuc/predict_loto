# Usage: Rscript scripts/generate_grids.R <loto|euromillions> [strategy] [n]
# Strategies: see `strategy_names` in R/generator.R
source("R/db.R")
source("R/stats.R")
source("R/generator.R")

args <- commandArgs(trailingOnly = TRUE)
game <- if (length(args) >= 1) args[1] else "loto"
strategy <- if (length(args) >= 2) args[2] else "avoid_popular"
n <- if (length(args) >= 3) as.integer(args[3]) else 5L

con <- DBI::dbConnect(RSQLite::SQLite(), "data/loto.sqlite")
numbers <- load_numbers(con, game)
DBI::dbDisconnect(con)

cat("Reminder: every combination is equally likely to win.\n")
print(as.data.frame(generate_grids(game, n, strategy, numbers)), row.names = FALSE)
