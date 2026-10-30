# SQLite persistence: wide draw tables, a long `drawn_numbers` table and `meta`.

library(DBI)

#' Long format: one row per drawn ball / bonus number (easier for frequency stats).
to_long <- function(draws) {
  balls <- tidyr::pivot_longer(draws,
    cols = dplyr::all_of(paste0("n", 1:5)),
    names_to = "position", values_to = "number"
  ) |>
    dplyr::mutate(kind = "ball", position = as.integer(sub("n", "", position)))
  bonus <- tidyr::pivot_longer(draws,
    cols = dplyr::all_of(c("bonus1", "bonus2")),
    names_to = "position", values_to = "number", values_drop_na = TRUE
  ) |>
    dplyr::mutate(kind = "bonus", position = as.integer(sub("bonus", "", position)))
  dplyr::bind_rows(balls, bonus) |>
    dplyr::select(game, draw_id, date, kind, position, number) |>
    dplyr::arrange(date, draw_id, kind, position)
}

#' Rewrite all tables from the given draw tables (safe to re-run).
write_draws_db <- function(con, draws_list) {
  stamp <- function(d) dplyr::mutate(d, date = as.character(date)) # ISO text for SQL date functions
  for (d in draws_list) {
    dbWriteTable(con, paste0("draws_", d$game[1]), stamp(d), overwrite = TRUE)
  }
  all_draws <- dplyr::bind_rows(draws_list)
  dbWriteTable(con, "drawn_numbers", stamp(to_long(all_draws)), overwrite = TRUE)
  meta <- all_draws |>
    dplyr::group_by(game) |>
    dplyr::summarise(
      first_date = as.character(min(date)), last_date = as.character(max(date)),
      n_draws = dplyr::n(), .groups = "drop"
    ) |>
    dplyr::mutate(loaded_at = format(Sys.time(), "%Y-%m-%d %H:%M:%S"))
  dbWriteTable(con, "meta", meta, overwrite = TRUE)
  invisible(meta)
}
