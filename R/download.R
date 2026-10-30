# Download the official FDJ history archives into <root>/data/raw/ (one folder per archive).

fdj_base_url <- paste0(
  "https://www.sto.api.fdj.fr/anonymous/service-draw-info/v3/documentations/",
  "1a2b3c4d-9876-4562-b3fc-2c963f66af"
)

fdj_archives <- data.frame(
  game = c(rep("loto", 5), rep("euromillions", 6)),
  suffix = c("l6", "m6", "n6", "o6", "p6", "e6", "d6", "c6", "b6", "a9", "a8"),
  period = c(
    "1976-05..2008-10", "2008-10..2017-03", "2017-03..2019-02", "2019-02..2019-11", "2019-11..now",
    "2020-02..now", "2019-03..2020-02", "2016-09..2019-02", "2014-02..2016-09", "2011-05..2014-02",
    "2004-02..2011-05"
  )
)

#' @param latest only fetch the two archives that contain the most recent draws.
download_fdj <- function(latest = FALSE, root = ".", quiet = TRUE) {
  archives <- fdj_archives
  if (latest) archives <- archives[archives$suffix %in% c("p6", "e6"), ]
  for (i in seq_len(nrow(archives))) {
    a <- archives[i, ]
    prefix <- if (a$game == "loto") "loto_" else "euro_"
    dir <- file.path(root, "data/raw", a$game, paste0(prefix, a$suffix))
    dir.create(dir, recursive = TRUE, showWarnings = FALSE)
    zip <- tempfile(fileext = ".zip")
    utils::download.file(paste0(fdj_base_url, a$suffix), zip, mode = "wb", quiet = TRUE)
    utils::unzip(zip, exdir = dir, overwrite = TRUE)
    unlink(zip)
    if (!quiet) message(a$game, " ", a$period, " -> ", dir)
  }
  invisible(archives)
}
