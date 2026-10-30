mod_data_ui <- function(id) {
  ns <- NS(id)
  tagList(
    card(
      card_header("Données chargées"),
      DTOutput(ns("meta"))
    ),
    card(
      card_header("Mise à jour"),
      card_body(
        p(
          "Télécharge les dernières archives officielles de la FDJ (accès Internet requis) ",
          "puis reconstruit la base de données locale."
        ),
        actionButton(ns("update"), "Mettre à jour depuis la FDJ", class = "btn-primary"),
        p(
          class = "text-muted small mt-3",
          "Source : archives de tirages fdj.fr (Loto depuis octobre 2008, Euromillions depuis février 2004)."
        )
      )
    )
  )
}

mod_data_server <- function(id, db_path, root, version) {
  moduleServer(id, function(input, output, session) {
    output$meta <- renderDT({
      version()
      m <- with_db(db_path, function(con) DBI::dbReadTable(con, "meta"))
      out <- data.frame(game_labels[m$game], m$n_draws, m$first_date, m$last_date, m$loaded_at)
      names(out) <- c("Jeu", "Tirages", "Premier tirage", "Dernier tirage", "Base construite le")
      datatable(out, rownames = FALSE, options = list(dom = "t"))
    })

    observeEvent(input$update, {
      withProgress(message = "Mise à jour en cours…", value = 0.2, {
        ok <- tryCatch(
          {
            download_fdj(latest = TRUE, root = root)
            setProgress(0.7, detail = "Reconstruction de la base")
            build_database(root)
            TRUE
          },
          error = function(e) {
            showNotification(paste("Échec de la mise à jour :", conditionMessage(e)), type = "error", duration = 10)
            FALSE
          }
        )
      })
      if (ok) {
        version(version() + 1)
        showNotification("Base de données mise à jour.", type = "message")
      }
    })
  })
}
