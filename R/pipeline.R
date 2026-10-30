# ETL pipeline: raw FDJ files -> validated draws -> SQLite. Requires etl.R and db.R.

#' Rebuild the draw tables of the SQLite database. Returns the `meta` table.
build_database <- function(root = ".") {
  draws <- list(
    etl_loto(file.path(root, "data/raw/loto")),
    etl_euromillions(file.path(root, "data/raw/euromillions"))
  )
  for (d in draws) {
    bad <- validate_draws(d)
    if (nrow(bad) > 0) {
      print(bad)
      stop("Invalid draws found for ", d$game[1])
    }
  }
  con <- DBI::dbConnect(RSQLite::SQLite(), file.path(root, "data/loto.sqlite"))
  on.exit(DBI::dbDisconnect(con))
  write_draws_db(con, draws)
}
