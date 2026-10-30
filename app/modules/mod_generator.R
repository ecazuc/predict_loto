mod_generator_ui <- function(id) {
  ns <- NS(id)
  layout_sidebar(
    sidebar = sidebar(
      radioButtons(ns("game"), "Jeu", choices = setNames(names(game_labels), game_labels)),
      selectInput(ns("strategy"), "Stratégie", choices = setNames(names(strategy_labels), strategy_labels), selected = "avoid_popular"),
      sliderInput(ns("n"), "Nombre de grilles", min = 1, max = 10, value = 5, step = 1),
      checkboxInput(ns("use_sum"), "Limiter la somme des 5 numéros", FALSE),
      conditionalPanel(
        sprintf("input['%s']", ns("use_sum")),
        sliderInput(ns("sum_range"), NULL, min = 15, max = 240, value = c(100, 150))
      ),
      checkboxInput(ns("use_odd"), "Limiter le nombre d'impairs", FALSE),
      conditionalPanel(
        sprintf("input['%s']", ns("use_odd")),
        sliderInput(ns("odd_range"), NULL, min = 0, max = 5, value = c(2, 3), step = 1)
      ),
      numericInput(ns("seed"), "Graine (optionnel, pour reproduire)", value = NA),
      actionButton(ns("go"), "Générer", class = "btn-primary")
    ),
    card(
      card_body(
        div(
          class = "alert alert-info",
          "Toutes les combinaisons ont exactement la même probabilité de sortir, et le backtest montre qu'aucune stratégie ",
          "n'améliore vos chances. « Éviter les grilles populaires » ne change pas la probabilité de gagner : elle vise seulement ",
          "à réduire le risque de partager un gain (dates de naissance, suites de numéros et progressions sont très jouées). ",
          "Ce caractère « populaire » est une hypothèse : la FDJ ne publie pas les grilles jouées."
        ),
        DTOutput(ns("grids")),
        downloadButton(ns("download"), "Télécharger (CSV)", class = "mt-3")
      )
    )
  )
}

mod_generator_server <- function(id, db_path, version) {
  moduleServer(id, function(input, output, session) {
    grids <- eventReactive(input$go, {
      numbers <- with_db(db_path, function(con) load_numbers(con, input$game))
      constraints <- list(
        sum_range = if (input$use_sum) input$sum_range,
        odd_range = if (input$use_odd) input$odd_range
      )
      seed <- if (is.na(input$seed)) NULL else input$seed
      tryCatch(
        generate_grids(input$game, input$n, input$strategy, numbers, constraints, seed = seed),
        error = function(e) {
          showNotification(paste("Impossible de générer les grilles :", conditionMessage(e)), type = "error")
          NULL
        }
      )
    })

    display <- reactive({
      g <- req(grids())
      bonus <- if (input$game == "loto") as.character(g$bonus1) else paste(g$bonus1, g$bonus2, sep = " - ")
      out <- data.frame(
        apply(g[paste0("n", 1:5)], 1, paste, collapse = " - "), bonus, g$sum, g$n_odd, g$n_low
      )
      names(out) <- c("Numéros", bonus_labels[[input$game]], "Somme", "Impairs", "Petits")
      out
    })

    output$grids <- renderDT(datatable(display(), rownames = FALSE, options = list(dom = "t", ordering = FALSE)))
    output$download <- downloadHandler(
      filename = function() paste0("grilles_", input$game, ".csv"),
      content = function(file) utils::write.csv(display(), file, row.names = FALSE, fileEncoding = "UTF-8")
    )
  })
}
