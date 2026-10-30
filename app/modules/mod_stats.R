mod_stats_ui <- function(id) {
  ns <- NS(id)
  layout_sidebar(
    sidebar = sidebar(
      radioButtons(ns("game"), "Jeu", choices = setNames(names(game_labels), game_labels)),
      radioButtons(ns("kind"), "Numéros", choices = c("Boules" = "ball", "Bonus (chance / étoiles)" = "bonus")),
      selectInput(ns("period"), "Période",
        choices = c("Tout l'historique" = "all", "10 dernières années" = "10", "5 dernières années" = "5", "Dernière année" = "1")
      ),
      helpText(
        "Pour les étoiles d'Euromillions, seule la période où les étoiles vont de 1 à 12 ",
        "(depuis le 27/09/2016) est comparable."
      )
    ),
    navset_card_tab(
      nav_panel(
        "Fréquences",
        plotOutput(ns("freq_plot"), height = "380px"),
        textOutput(ns("chi2")),
        helpText(
          "Ligne pointillée : fréquence attendue si le tirage est parfaitement aléatoire. ",
          "Barres rouges : écart supérieur à 2 écarts-types (sur ~50 numéros, 2 à 3 barres rouges sont normales)."
        )
      ),
      nav_panel(
        "Écarts (retard)",
        DTOutput(ns("gaps")),
        helpText("Un numéro « en retard » n'a pas plus de chances de sortir : les tirages n'ont pas de mémoire.")
      ),
      nav_panel("Paires", DTOutput(ns("pairs")), uiOutput(ns("pairs_note"))),
      nav_panel(
        "Parité et répartition",
        radioButtons(ns("feature"), NULL,
          choices = c("Nombre de numéros impairs" = "n_odd", "Nombre de numéros petits (≤ moitié)" = "n_low"),
          inline = TRUE
        ),
        plotOutput(ns("feature_plot"), height = "340px"),
        textOutput(ns("feature_chi2"))
      )
    )
  )
}

mod_stats_server <- function(id, db_path, version) {
  moduleServer(id, function(input, output, session) {
    rules <- reactive(game_rules[[input$game]])
    draws <- reactive({
      version()
      with_db(db_path, function(con) load_draws(con, input$game))
    })
    numbers <- reactive({
      version()
      with_db(db_path, function(con) load_numbers(con, input$game))
    })
    period_since <- reactive({
      if (input$period == "all") as.Date(NA) else seq(Sys.Date(), by = paste0("-", input$period, " years"), length.out = 2)[2]
    })
    # bonus numbers: also limited to the current range of the bonus numbers
    since <- reactive({
      s <- c(period_since(), if (input$kind == "bonus") rules()$bonus_since)
      if (all(is.na(s))) as.Date(NA) else max(s, na.rm = TRUE)
    })
    max_number <- reactive(if (input$kind == "ball") rules()$max_ball else rules()$max_bonus)
    nums_f <- reactive({
      n <- numbers()
      if (is.na(since())) n else n[n$date >= since(), ]
    })
    draws_f <- reactive({
      d <- draws()
      if (is.na(period_since())) d else d[d$date >= period_since(), ]
    })
    freq <- reactive({
      req(nrow(nums_f()) > 0)
      number_frequency(nums_f(), input$kind, max_number())
    })

    output$freq_plot <- renderPlot({
      f <- freq()
      ggplot(f, aes(number, n, fill = abs(z) > 2)) +
        geom_col() +
        geom_hline(yintercept = f$expected[1], linetype = "dashed") +
        scale_fill_manual(values = c(`FALSE` = "#4C78A8", `TRUE` = "#E4572E"), guide = "none") +
        labs(x = "Numéro", y = "Nombre de sorties") +
        theme_minimal(base_size = 14)
    })

    output$chi2 <- renderText({
      t <- uniformity_test(freq())
      sprintf(
        "Test du χ² d'uniformité : χ² = %.1f, ddl = %d, p = %.3f. %s",
        t$statistic, as.integer(t$df), t$p_value,
        if (t$p_value > 0.05) "Aucune différence significative avec un tirage parfaitement aléatoire." else
          "Écart significatif (à interpréter avec prudence)."
      )
    })

    output$gaps <- renderDT({
      req(nrow(nums_f()) > 0)
      g <- number_gaps(nums_f(), input$kind, max_number())
      g <- g[order(-g$current_gap), ]
      g$avg_gap <- round(g$avg_gap, 1)
      names(g) <- c("Numéro", "Sorties", "Tirages depuis la dernière sortie", "Écart moyen", "Écart maximal")
      datatable(g, rownames = FALSE, options = list(pageLength = 10))
    })

    output$pairs <- renderDT({
      req(input$kind == "ball", nrow(draws_f()) > 0)
      p <- pair_counts(draws_f(), rules()$max_ball, top = 20)
      p$expected <- round(p$expected, 1)
      p$ratio <- round(p$ratio, 2)
      names(p) <- c("Numéro A", "Numéro B", "Sorties ensemble", "Attendu (hasard)", "Rapport")
      datatable(p, rownames = FALSE, options = list(dom = "tp", pageLength = 10))
    })
    output$pairs_note <- renderUI({
      if (input$kind != "ball") {
        helpText("Les paires ne concernent que les boules.")
      } else {
        helpText(
          "Parmi plus de 1 100 paires possibles, les plus fréquentes dépassent toujours le nombre attendu ",
          "(effet du maximum parmi beaucoup de valeurs) : ce n'est pas un signal."
        )
      }
    })

    feature_tab <- reactive({
      req(nrow(draws_f()) > 0)
      ft <- draw_features(draws_f(), rules()$max_ball)
      feature_vs_expected(ft, input$feature, rules()$max_ball)
    })
    output$feature_plot <- renderPlot({
      t <- feature_tab()
      long <- rbind(
        data.frame(value = t$value, n = t$n, type = "Observé"),
        data.frame(value = t$value, n = t$expected, type = "Attendu (hasard)")
      )
      ggplot(long, aes(factor(value), n, fill = type)) +
        geom_col(position = "dodge") +
        scale_fill_manual(values = c("Observé" = "#4C78A8", "Attendu (hasard)" = "#BBBBBB"), name = NULL) +
        labs(x = "Valeur", y = "Nombre de tirages") +
        theme_minimal(base_size = 14) +
        theme(legend.position = "top")
    })
    output$feature_chi2 <- renderText({
      t <- feature_test(feature_tab())
      sprintf("Test du χ² : χ² = %.1f, ddl = %d, p = %.3f.", t$statistic, as.integer(t$df), t$p_value)
    })
  })
}
