#' Read and normalise FDJ archives into a tidy draw table.
#'
#' FDJ publishes each game as several ZIP/CSV archives (one per format era,
#' ";"-separated, dates as dd/mm/yyyy or yyyymmdd). All format-era handling
#' lives in this file.

library(dplyr)
library(readr)

parse_fdj_date <- function(x) {
  x <- trimws(x)
  # dd/mm/yyyy, dd/mm/yy or yyyymmdd depending on the archive
  fmt <- dplyr::case_when(
    grepl("^\\d{2}/\\d{2}/\\d{4}$", x) ~ "%d/%m/%Y",
    grepl("^\\d{2}/\\d{2}/\\d{2}$", x) ~ "%d/%m/%y",
    TRUE ~ "%Y%m%d"
  )
  as.Date(mapply(function(v, f) as.character(as.Date(v, format = f)), x, fmt, USE.NAMES = FALSE))
}

read_fdj_csv <- function(path) {
  read_delim(path,
    delim = ";", col_types = cols(.default = col_character()),
    locale = locale(encoding = "latin1"), progress = FALSE
  ) |>
    # some archives repeat the header line inside the data
    filter(annee_numero_de_tirage != "annee_numero_de_tirage")
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
    distinct(draw_id, .keep_all = TRUE) |>
    arrange(date)
}

#' Euromillions: 5 numbers + 2 stars. The star range changed over time, so it is
#' not enforced here (see validate_draws()).
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
    distinct(draw_id, .keep_all = TRUE) |>
    arrange(date)
}

#' Returns rows violating basic range rules (empty tibble = OK).
validate_draws <- function(draws, max_ball, max_bonus) {
  balls <- as.matrix(draws[paste0("n", 1:5)])
  bad <- is.na(draws$date) | draws$date < as.Date("2000-01-01") | draws$date > Sys.Date() |
    apply(balls, 1, function(r) anyNA(r) || any(r < 1 | r > max_ball) || anyDuplicated(r) > 0) |
    is.na(draws$bonus1) | draws$bonus1 < 1 | draws$bonus1 > max_bonus
  draws[bad, ]
}
