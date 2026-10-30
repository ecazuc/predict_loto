# predict-loto

Historique des tirages du **Loto** et d'**Euromillions** (FDJ), statistiques, backtest de stratégies,
générateur de grilles et application Shiny (interface en français), en R.

> **Avertissement :** les tirages sont aléatoires et indépendants. Aucune statistique ni aucun modèle
> ne peut les prédire mieux que le hasard. Le projet sert à l'analyse de données et à l'apprentissage ;
> chaque « stratégie » est testée contre un tirage aléatoire (onglet Backtest).

## Installation

1. Installer R (4.6+). Sous Windows, si la bibliothèque système n'est pas modifiable, créer un dossier
   et définir `R_LIBS_USER` (ex. `%LOCALAPPDATA%\R\win-library\4.6`).
2. Dans le dossier du projet : `renv::restore()`
3. `Rscript scripts/download_fdj.R` : archives officielles FDJ dans `data/raw/`
4. `Rscript scripts/run_etl.R` : construit `data/loto.sqlite`
5. `Rscript scripts/run_backtest.R` : calcule les résultats du backtest (onglet Backtest)
6. Lancer l'application : `shiny::runApp("app")`

Tests : `testthat::test_dir("tests/testthat")` · Format/lint : `styler::style_dir("R")`, `lintr::lint_dir("R")`

## Structure

| Dossier | Contenu |
|---|---|
| `R/` | Fonctions pures : `etl.R`, `db.R`, `pipeline.R`, `download.R`, `stats.R`, `backtest.R`, `generator.R`, `format.R` |
| `scripts/` | Points d'entrée en ligne de commande (téléchargement, ETL, rapport, backtest, génération) |
| `app/` | Application Shiny (`app.R`, `global.R`, `modules/`) |
| `tests/testthat/` | Tests unitaires et tests des modules Shiny |
| `data/` | `raw/` (archives FDJ) et `loto.sqlite` : générés, non versionnés |

## Données

Source : archives d'historique FDJ ([Loto](https://www.fdj.fr/jeux-de-tirage/loto/historique),
[Euromillions](https://www.fdj.fr/jeux-de-tirage/euromillions-my-million/historique)).

| Jeu | Règles | Couverture |
|---|---|---|
| Loto | 5 numéros 1-49 + numéro chance 1-10 | 06/10/2008 → aujourd'hui |
| Euromillions | 5 numéros 1-50 + 2 étoiles (max 9, puis 11 dès 05/2011, 12 dès 09/2016) | 13/02/2004 → aujourd'hui |

L'archive Loto 1976-2008 (6 boules + complémentaire) n'est pas chargée.

## Résultats principaux

- Les fréquences des numéros ne s'écartent pas de l'uniforme (test du χ², p > 0,05 partout).
- Aucune stratégie (numéros chauds, froids, en retard) ne dépasse le hasard après correction des
  tests multiples ; le backtest détecte bien un biais artificiel injecté (test automatisé).
- Le générateur ne change pas la probabilité de gagner. L'option « éviter les grilles populaires »
  vise seulement à limiter le partage d'un gain ; le caractère « populaire » est une hypothèse.

## Déploiement (optionnel)

L'application a besoin de `data/loto.sqlite` : exécuter les étapes 3 à 5 avant de déployer
(par ex. shinyapps.io avec `rsconnect::deployApp("app")` après avoir copié `R/` et `data/` dans le dossier
déployé, car `app/global.R` lit `../R` et `../data`). Non testé.
