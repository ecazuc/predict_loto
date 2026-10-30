library(shiny)
library(bslib)
library(DT)
library(ggplot2)
library(dplyr)

# the app lives in <root>/app; project code is in <root>/R
app_root <- normalizePath(file.path(getwd(), ".."), winslash = "/")
for (f in c("etl", "db", "stats", "generator", "download", "pipeline", "format")) {
  source(file.path(app_root, "R", paste0(f, ".R")), encoding = "UTF-8")
}
for (f in list.files("modules", pattern = "\\.R$", full.names = TRUE)) source(f, encoding = "UTF-8")

db_path <- file.path(app_root, "data", "loto.sqlite")
if (!file.exists(db_path)) {
  stop("Base de données absente : lancez d'abord `Rscript scripts/download_fdj.R` puis `Rscript scripts/run_etl.R`.")
}
