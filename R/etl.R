# Read and normalise FDJ archives into a tidy draw table.
#
# FDJ publishes each game as several ZIP/CSV archives (one per format era,
# ";"-separated, dates as dd/mm/yyyy, dd/mm/yy or yyyymmdd). All format-era
# handling lives in this file.

library(dplyr)
library(readr)

# Euromillions star range changed over time (first draw date of each range)
euro_star_eras <- tibble::tibble(
  from = as.Date(c("2004-01-01", "2011-05-10", "2016-09-27")),
  max_star = c(9L, 11L, 12L)
)

#' Parse FDJ dates written as dd/mm/yyyy, dd/mm/yy or yyyymmdd.
parse_fdj_date <- function(x) {
  x <- trimws(x)
  fmt <- case_when(
    grepl("^\\d{2}/\\d{2}/\\d{4}$", x) ~ "%d/%m/%Y",
    grepl("^\\d{2}/\\d{2}/\\d{2}$", x) ~ "%d/%m/%y",
    TRUE ~ "%Y%m%d"
  )
  as.Date(mapply(function(v, f) as.character(as.Date(v, format = f)), x, fmt, USE.NAMES = FALSE))
}

#' Read one FDJ CSV as all-character columns.
#'
#' One Euromillions archive has a trailing empty column on every data row;
#' readr reports it as a parsing issue. That case is accepted, anything else fails.
read_fdj_csv <- function(path) {
  d <- withCallingHandlers(
    read_delim(path,
      delim = ";", col_types = cols(.default = col_character()),
      locale = locale(encoding = "latin1"), progress = FALSE, name_repair = "minimal"
    ),
    warning = function(w) {
      if (grepl("parsing issue", conditionMessage(w))) invokeRestart("muffleWarning")
    }
  )
  p <- problems(d)
  if (nrow(p) > 0) {
    n_expected <- as.integer(sub(" .*", "", p$expected))
    n_actual <- as.integer(sub(" .*", "", p$actual))
    if (any(is.na(n_actual) | n_actual != n_expected + 1L)) {
      stop("Unexpected parsing problems in ", path)
    }
  }
  d[, names(d) != ""]
}

#' Keep one row per draw_id; fail if reused ids hide genuinely different draws.
dedupe_draws <- function(draws) {
  out <- distinct(draws, draw_id, .keep_all = TRUE)
  if (nrow(out) != nrow(distinct(draws))) {
    stop("draw_id collision between different draws in ", draws$game[1])
  }
  arrange(out, date)
}

#' Loto: only the 5 numbers (1-49) + chance (1-10) format, i.e. files from
#' Oct 2008 onwards. The 1976-2008 archive is 6 balls + complementary: not comparable.
etl_loto <- function(raw_dir = "data/raw/loto") {
  files <- list.files(raw_dir, pattern = "\\.csv$", recursive = TRUE, full.names = TRUE)
  files <- files[!grepl("loto_l6", files)]
  lapply(files, read_fdj_csv) |>
    bind_rows() |>
    transmute(
      game = "loto",
      draw_id = annee_numero_de_tirage,
      date = parse_fdj_date(date_de_tirage),
      n1 = as.integer(boule_1), n2 = as.integer(boule_2), n3 = as.integer(boule_3),
      n4 = as.integer(boule_4), n5 = as.integer(boule_5),
      bonus1 = as.integer(numero_chance), bonus2 = NA_integer_
    ) |>
    dedupe_draws()
}

#' Euromillions: 5 numbers (1-50) + 2 stars (range depends on the era).
etl_euromillions <- function(raw_dir = "data/raw/euromillions") {
  files <- list.files(raw_dir, pattern = "\\.csv$", recursive = TRUE, full.names = TRUE)
  lapply(files, read_fdj_csv) |>
    bind_rows() |>
    transmute(
      game = "euromillions",
      draw_id = annee_numero_de_tirage,
      date = parse_fdj_date(date_de_tirage),
      n1 = as.integer(boule_1), n2 = as.integer(boule_2), n3 = as.integer(boule_3),
      n4 = as.integer(boule_4), n5 = as.integer(boule_5),
      bonus1 = as.integer(etoile_1), bonus2 = as.integer(etoile_2)
    ) |>
    dedupe_draws()
}

#' Max bonus number allowed on each date (Loto: constant; Euromillions: by era).
max_bonus_for <- function(game, date) {
  if (game == "loto") {
    return(rep(10L, length(date)))
  }
  euro_star_eras$max_star[findInterval(date, euro_star_eras$from)]
}

#' Returns the rows violating the game rules (empty tibble = OK).
validate_draws <- function(draws) {
  game <- draws$game[1]
  max_ball <- c(loto = 49L, euromillions = 50L)[[game]]
  max_bonus <- max_bonus_for(game, draws$date)
  balls <- as.matrix(draws[paste0("n", 1:5)])
  bad_balls <- apply(balls, 1, function(r) anyNA(r) || any(r < 1 | r > max_ball) || anyDuplicated(r) > 0)
  bad_bonus <- is.na(draws$bonus1) | draws$bonus1 < 1 | draws$bonus1 > max_bonus
  if (game == "euromillions") {
    bad_bonus <- bad_bonus | is.na(draws$bonus2) | draws$bonus2 < 1 | draws$bonus2 > max_bonus |
      draws$bonus1 == draws$bonus2
  }
  bad_date <- is.na(draws$date) | draws$date < as.Date("2000-01-01") | draws$date > Sys.Date()
  draws[bad_balls | bad_bonus | bad_date, ]
}
