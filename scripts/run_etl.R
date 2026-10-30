source("R/etl.R")
library(DBI)

loto <- etl_loto()
euro <- etl_euromillions()

for (x in list(list(loto, 49, 10), list(euro, 50, 12))) {
  bad <- validate_draws(x[[1]], x[[2]], x[[3]])
  if (nrow(bad) > 0) {
    print(bad)
    stop("Invalid draws found for ", x[[1]]$game[1])
  }
}

con <- dbConnect(RSQLite::SQLite(), "data/loto.sqlite")
on.exit(dbDisconnect(con))
# dates stored as ISO text so SQL date functions work
dbWriteTable(con, "draws_loto", dplyr::mutate(loto, date = as.character(date)), overwrite = TRUE)
dbWriteTable(con, "draws_euromillions", dplyr::mutate(euro, date = as.character(date)), overwrite = TRUE)
message("loto: ", nrow(loto), " draws (", min(loto$date), " -> ", max(loto$date), ")")
message("euromillions: ", nrow(euro), " draws (", min(euro$date), " -> ", max(euro$date), ")")
