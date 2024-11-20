# © 2024 The Johns Hopkins University Applied Physics Laboratory LLC
# Development of this software was sponsored by the U.S. Government under 
# contracts no. 75D30120C07643, 75D30122C15442, 75D30124C19958

#----------------------------------------------------
# Scenario Detection App - Phase 1
# Authors:
#   Joshua Kimrey
#   Catherine Schwartz
#   Roseric Azondekon
#   Michael Sheppard
#----------------------------------------------------

reportModuleOutput <- function(id) {
  ns <- NS(id)
  tagList(
    downloadButton(outputId = ns("report"), "Download Report")
  )
}

reportModule <- function(input, output, session, sideBarInput, widgets, master, filters) {
  # Reactive function to store the binary empty df status
  master_empty <- reactive({
    is.null(master$df)
  })
  
  observe({
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
        
        # All user-selections and filters
        selected_state = sideBarInput()$selected$state,
        selected_date = sideBarInput()$selected$date,
        selected_normalize = sideBarInput()$selected$normalize,
        selected_method = sideBarInput()$selected$method,
        selected_region = filters$region,
        selected_sex = filters$sex,
        selected_agegroup = filters$age,
        selected_icd10 = filters$dd,
        selected_ccsr = filters$ccsr,
        selected_subc = filters$subc,
        selected_ccdd = filters$ccdd,
        selected_min_baseline_records = filters$min_records_baseline,
        selected_min_testdate_records = filters$min_records_testdate,
        
        # Widgets objects (map and tables)
        widgets = widgets
      )
    )
    parent.env(new_envir) <- globalenv()
    new_envir
  })
  
  # Generate PDF report
  output$report <- downloadHandler(
    filename = paste0("ScenarioDetection_report_", Sys.Date(), ".html"),
    content = function(file) {
      # Render the R Markdown report
      withProgress(
        message = "Generating report...",
        detail = "Please be patient - This may take a while...",
        value = 0,
        {
          temp_dir <- tempdir()
          tempReport <- file.path(temp_dir, "ScenarioDetection_report.Rmd")
          file.copy("src/report/ScenarioDetection_report.Rmd", tempReport, overwrite = TRUE)
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