mod_backtest_ui <- function(id) {
  ns <- NS(id)
  tagList(
    card(
      card_body(
        tags$strong("Principe : "),
        "pour chaque tirage (après 200 tirages d'échauffement), chaque stratégie choisit ses numéros ",
        "en n'utilisant que les tirages précédents. On compte les numéros trouvés et on compare la moyenne à ce que ",
        "donnerait le hasard (loi hypergéométrique). Le p ajusté tient compte du nombre de stratégies testées."
      )
    ),
    uiOutput(ns("verdict")),
    plotOutput(ns("plot"), height = "420px"),
    DTOutput(ns("table"))
  )
}

mod_backtest_server <- function(id, db_path, version) {
  moduleServer(id, function(input, output, session) {
    results <- reactive({
      version()
      with_db(db_path, function(con) {
        if (!DBI::dbExistsTable(con, "backtest_results")) {
          return(NULL)
        }
        DBI::dbReadTable(con, "backtest_results")
      })
    })

    output$verdict <- renderUI({
      r <- results()
      if (is.null(r)) {
        return(card(card_body("Résultats absents : lancez `Rscript scripts/run_backtest.R`.")))
      }
      r <- r[r$strategy != "random", ]
      if (all(r$p_adjusted > 0.05)) {
        div(class = "alert alert-success", "Aucune stratégie ne fait significativement mieux que le hasard (p ajusté > 0,05 partout).")
      } else {
        div(class = "alert alert-warning", "Au moins une stratégie dépasse le seuil de 5 % après correction : à vérifier.")
      }
    })

    output$plot <- renderPlot({
      r <- req(results())
      r$stratégie <- backtest_labels[r$strategy]
      ggplot(r, aes(z, stratégie)) +
        annotate("rect", xmin = -1.96, xmax = 1.96, ymin = -Inf, ymax = Inf, fill = "#BBBBBB", alpha = 0.35) +
        geom_vline(xintercept = 0) +
        geom_point(size = 3, colour = "#4C78A8") +
        facet_wrap(~label, ncol = 1) +
        labs(x = "z (écart à la moyenne attendue, en écarts-types) — bande grise : zone compatible avec le hasard", y = NULL) +
        theme_minimal(base_size = 13)
    })

    output$table <- renderDT({
      r <- req(results())
      out <- data.frame(
        r$label, backtest_labels[r$strategy], r$n_draws, round(r$mean_hits, 4), round(r$expected, 4),
        round(r$z, 2), signif(r$p_value, 3), signif(r$p_adjusted, 3)
      )
      names(out) <- c("Jeu", "Stratégie", "Tirages testés", "Réussites moyennes", "Attendu (hasard)", "z", "p brut", "p ajusté")
      datatable(out, rownames = FALSE, options = list(pageLength = 20, dom = "tp"))
    })
  })
}
