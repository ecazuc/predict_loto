# predict-loto

R project: French Loto & Euromillions history, statistics, backtests, number generator, Shiny UI.

## Important
- Draws are random and independent. Never claim predictive power; every model must be backtested against a random baseline and the app must show a disclaimer.

## Stack
R, tidyverse, DBI + RSQLite (data/loto.sqlite), Shiny + bslib, testthat, renv.

## Layout
- data/raw/        downloaded FDJ files (never edited by hand)
- data/loto.sqlite built by ETL
- R/               pure functions (etl_*.R, stats_*.R, models_*.R, generator.R)
- scripts/         run_etl.R, update_draws.R
- app/             Shiny app (app.R, modules/)
- tests/testthat/

## Conventions
- tidyverse style, snake_case, `styler` formatting, roxygen comments on functions in R/
- Draw schema: game, draw_id, date, n1..n5, bonus (lucky | star1, star2)
- ETL must be idempotent; keep format-era handling in one place
- Run: `renv::restore()`, `source("scripts/run_etl.R")`, `shiny::runApp("app")`
- Tests: `testthat::test_dir("tests/testthat")`

## Game rules reference
- Loto: 5 numbers 1–49 + chance number 1–10, from 2008-10-06 (the 1976–2008 archive is 6 balls + complementary and is excluded). ~156 draws/year in the data.
- Euromillions: 5 numbers 1–50 + 2 stars, from 2004-02-13. Weekly until May 2011, then twice weekly. Star max: 9 (2004), 11 (2011), 12 (Sep 2016).
- Source: FDJ ZIP archives (https://www.fdj.fr/jeux-de-tirage/loto/historique and .../euromillions-my-million/historique), downloaded into data/raw/ (git-ignored).
- R on this machine: `C:\Program Files\R\R-4.6.1\bin\Rscript.exe`; set `R_LIBS_USER=%LOCALAPPDATA%\R\win-library\4.6` (system library is not writable).
