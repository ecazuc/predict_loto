# predict-loto

French **Loto** and **Euromillions** draw history, statistics and a Shiny app, built in R.

> **Disclaimer:** lottery draws are random and independent. No statistic or model can predict
> them better than chance. This project is for data analysis and learning; every "strategy" is
> backtested against a random baseline.

## Setup

1. Install R (4.6+). On Windows, if the system library is not writable, set
   `R_LIBS_USER=%LOCALAPPDATA%\R\win-library\4.6` (create the folder first).
2. `renv::restore()`
3. `Rscript scripts/download_fdj.R` (official FDJ archives into `data/raw/`)
4. `Rscript scripts/run_etl.R` (builds `data/loto.sqlite`)
5. Tests: `testthat::test_dir("tests/testthat")`

## Data

Source: FDJ history archives (<https://www.fdj.fr/jeux-de-tirage/loto/historique>,
<https://www.fdj.fr/jeux-de-tirage/euromillions-my-million/historique>).

| Game | Rules | Coverage |
|---|---|---|
| Loto | 5 numbers 1-49 + chance 1-10 | 2008-10-06 to now |
| Euromillions | 5 numbers 1-50 + 2 stars (max 9, then 11 from 2011-05, 12 from 2016-09) | 2004-02-13 to now |

The 1976-2008 Loto archive (6 balls + complementary) is not loaded.
