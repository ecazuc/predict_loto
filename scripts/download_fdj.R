# Download the official FDJ history archives into data/raw/ (one folder per archive).
# Re-run to refresh: the two most recent archives of each game contain the latest draws.

base <- paste0(
  "https://www.sto.api.fdj.fr/anonymous/service-draw-info/v3/documentations/",
  "1a2b3c4d-9876-4562-b3fc-2c963f66af"
)

archives <- data.frame(
  game = c(rep("loto", 5), rep("euromillions", 6)),
  suffix = c("l6", "m6", "n6", "o6", "p6", "e6", "d6", "c6", "b6", "a9", "a8"),
  period = c(
    "1976-05..2008-10", "2008-10..2017-03", "2017-03..2019-02", "2019-02..2019-11", "2019-11..now",
    "2020-02..now", "2019-03..2020-02", "2016-09..2019-02", "2014-02..2016-09", "2011-05..2014-02",
    "2004-02..2011-05"
  )
)

only_latest <- "--latest" %in% commandArgs(trailingOnly = TRUE)
if (only_latest) archives <- archives[archives$suffix %in% c("p6", "e6"), ]

for (i in seq_len(nrow(archives))) {
  a <- archives[i, ]
  dir <- file.path("data/raw", a$game, paste0(if (a$game == "loto") "loto_" else "euro_", a$suffix))
  dir.create(dir, recursive = TRUE, showWarnings = FALSE)
  zip <- tempfile(fileext = ".zip")
  download.file(paste0(base, a$suffix), zip, mode = "wb", quiet = TRUE)
  unzip(zip, exdir = dir, overwrite = TRUE)
  unlink(zip)
  message(a$game, " ", a$period, " -> ", dir)
}
