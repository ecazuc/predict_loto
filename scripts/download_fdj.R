# Usage: Rscript scripts/download_fdj.R [--latest]
# Re-run to refresh: the two most recent archives of each game contain the latest draws.
source("R/download.R")
download_fdj(latest = "--latest" %in% commandArgs(trailingOnly = TRUE), quiet = FALSE)
