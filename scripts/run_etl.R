source("R/etl.R")
source("R/db.R")

draws <- list(etl_loto(), etl_euromillions())

for (d in draws) {
  bad <- validate_draws(d)
  if (nrow(bad) > 0) {
    print(bad)
    stop("Invalid draws found for ", d$game[1])
  }
}

con <- dbConnect(RSQLite::SQLite(), "data/loto.sqlite")
meta <- write_draws_db(con, draws)
dbDisconnect(con)
print(as.data.frame(meta))
