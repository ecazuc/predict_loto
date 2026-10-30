mod_home_ui <- function(id) {
  ns <- NS(id)
  tagList(
    card(
      class = "border-warning",
      card_body(
        tags$strong("À savoir : "),
        "les tirages du Loto et d'Euromillions sont aléatoires et indépendants. Aucune statistique ni aucun modèle ",
        "ne peut prédire les numéros mieux que le hasard : l'onglet « Backtest » le démontre sur l'historique réel. ",
        "Cette application sert à explorer les données, pas à gagner."
      )
    ),
    uiOutput(ns("cards")),
    navset_card_tab(
      title = "Derniers tirages",
      nav_panel("Loto", DTOutput(ns("last_loto"))),
      nav_panel("Euromillions", DTOutput(ns("last_euromillions")))
    )
  )
}

mod_home_server <- function(id, db_path, version) {
  moduleServer(id, function(input, output, session) {
    meta <- reactive({
      version()
      with_db(db_path, function(con) DBI::dbReadTable(con, "meta"))
    })

    output$cards <- renderUI({
      m <- meta()
      cards <- lapply(seq_len(nrow(m)), function(i) {
        value_box(
          title = game_labels[[m$game[i]]],
          value = format(m$n_draws[i], big.mark = " "),
          p("tirages du ", format(as.Date(m$first_date[i]), "%d/%m/%Y"), " au ", format(as.Date(m$last_date[i]), "%d/%m/%Y"))
        )
      })
      layout_columns(!!!cards)
    })

    last_table <- function(game) {
      renderDT({
        version()
        d <- with_db(db_path, function(con) load_draws(con, game))
        datatable(format_draws(d, game, 10), rownames = FALSE, options = list(dom = "t", ordering = FALSE))
      })
    }
    output$last_loto <- last_table("loto")
    output$last_euromillions <- last_table("euromillions")
  })
}
