source("global.R", encoding = "UTF-8")

ui <- page_navbar(
  title = "Loto & Euromillions",
  theme = bs_theme(version = 5, bootswatch = "flatly"),
  nav_panel("Accueil", mod_home_ui("home")),
  nav_panel("Statistiques", mod_stats_ui("stats")),
  nav_panel("Backtest", mod_backtest_ui("backtest")),
  nav_panel("Générateur", mod_generator_ui("generator")),
  nav_panel("Données", mod_data_ui("data")),
  footer = div(
    class = "text-center text-muted small p-2",
    "Les tirages sont aléatoires : aucune méthode ne permet de prédire les numéros. Jouer comporte des risques : ",
    "pour être aidé, appelez le 09 74 75 13 13 (appel non surtaxé)."
  )
)

server <- function(input, output, session) {
  version <- reactiveVal(0) # bumped after a data update to refresh every tab
  mod_home_server("home", db_path, version)
  mod_stats_server("stats", db_path, version)
  mod_backtest_server("backtest", db_path, version)
  mod_generator_server("generator", db_path, version)
  mod_data_server("data", db_path, app_root, version)
}

shinyApp(ui, server)
