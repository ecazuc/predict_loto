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
- Loto: 5 numbers 1–49 + lucky number 1–10 (since 2008; 3 draws/week since 2019)
- Euromillions: 5 numbers 1–50 + 2 stars 1–12 (since 2016; earlier eras differ)
