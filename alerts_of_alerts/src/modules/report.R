#-------------------------------------------------
# Rnssp-shiny-apps: alerts_of_alerts App
#
# Authors:
#   Joshua Kimrey
#   Michael Sheppard
#   Raghav Ramachandran
#   Howard Burkom
#   Roseric Azondekon
#-------------------------------------------------

reportModuleOutput <- function(id) {
  ns <- NS(id)
  tagList(
    downloadButton(outputId = ns("report"), "Download Report")
  )
}



reportModule <- function(input, output, session, sideBarInput, widgets) {
  master_empty <- reactive({
    is.null(sideBarInput()$Reactive_dfs$df_1)
  })

  observe({
    message(names(widgets))
    disable <- master_empty()
    shinyjs::toggleState("report", !disable)
  })

  inc_progress <- function(msg = "", amnt = 0) {
    shiny::incProgress(detail = msg, amount = amnt)
  }

  report_envir <- reactive({
    new_envir <- rlang::new_environment(
      list(
        # Progress
        inc_progress = inc_progress,

        # Selected inputs
        selected_state = sideBarInput()$selected$state,
        selected_ccdd = sideBarInput()$selected$CCDD,
        selected_startDate = sideBarInput()$selected$startDate,
        selected_endDate = sideBarInput()$selected$endDate,
        maps_date = sideBarInput()$selected$maps_date,

        # Dimensions
        plotly_width = sideBarInput()$plotly_dims$width,
        plotly_height = sideBarInput()$plotly_dims$height,

        # Widgets objects
        widgets = widgets,

        # Statistics
        globalMoran = sideBarInput()$stat_test$globalMoran,
        joincount_alert = sideBarInput()$stat_test$JoinCount_alert,
        joincount_warningoralert = sideBarInput()$stat_test$JoinCount_warning_or_alert,
        joincount_inc = sideBarInput()$stat_test$JoinCount_increasing
      )
    )
    parent.env(new_envir) <- globalenv()
    new_envir
  })

  # Generate PDF report
  output$report <- downloadHandler(
    filename = paste0("Alerts_of_Alerts_report_", Sys.Date(), ".html"),
    content = function(file) {
      # Render the R Markdown report
      withProgress(
        message = "Generating report...",
        detail = "Please be patient - This may take a while...",
        value = 0,
        {
          temp_dir <- tempdir()
          tempReport <- file.path(temp_dir, "AoA_report.Rmd")
          file.copy("src/report/AoA_report.Rmd", tempReport, overwrite = TRUE)
          logo_file <- file.path(temp_dir, "logo.png")
          file.copy("src/report/logo.png", logo_file, overwrite = TRUE)
          suppressMessages(
            suppressWarnings(
              rmarkdown::render(tempReport, output_file = file, envir = report_envir())
            )
          )
        }
      )
    }
  )
}