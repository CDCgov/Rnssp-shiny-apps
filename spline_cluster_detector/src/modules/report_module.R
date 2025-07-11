# © 2025 The Johns Hopkins University Applied Physics Laboratory LLC
# Development of this software was sponsored by the U.S. Government under
# contract no. 75D30124C19958


REPORT_NAME <- paste0("spatiotemporalclustering_report_", Sys.Date(), ".html")


# Report UI
report_ui <- function(id) {
  ns <- NS(id)
  tagList(
    hidden(input_task_button(
      id = ns("generate_report"),
      label = "Generate Report",
      icon = bsicons::bs_icon(name = "filetype-html"),
      label_busy = "Generating... please wait",
      class = "btn-secondary"
    )),
    downloadButton(ns("download_report"), "Download Report", style = "visibility:hidden; position:absolute; top:-9999px;")
  )
}

mr <- function(k) {
  nm <- names(k)

  setNames(lapply(nm, \(n) {
    ty <- typeof(k[[n]])
    if (ty != "closure") k[[n]]
    else "closure not returned"
  }), nm)
}

# Report Server (Handles report generation and download)
report_server <- function(id, results, data_config, cluster_config) {
  moduleServer(id, function(input, output, session) {

    ns <- NS(id)

    # observe for changes in records. Only show the generate report button
    # when there are records

    observe({
      toggleElement("generate_report", condition = !is.null(results$records))
    })

    report_path <- reactive({

      toggle_task_button_color(ns("generate_report"), busy = TRUE)

      # Create a new environment to store rv objects
      report_env <- new.env()
      report_env[["results"]] <- mr(isolate(reactiveValuesToList(results)))
      report_env[["data_config"]] <- data_config
      report_env[["cluster_config"]] <- mr(isolate(reactiveValuesToList(cluster_config)))

      tempdir <- tempdir()
      tempReport <- file.path(tempdir, "stc_report.Rmd")
      file.copy("src/report/stc_report.Rmd", tempReport, overwrite = TRUE)
      logo_file <- file.path(tempdir, "logo.png")
      file.copy("src/report/logo.png", logo_file, overwrite = TRUE)

      output_file <- file.path(tempdir(), "stc_report.html")

      # Render the RMarkdown report, passing the environment directly
      rmarkdown::render(
        input = tempReport,
        output_file = output_file,
        envir = report_env
      )

      toggle_task_button_color(ns("generate_report"), busy = FALSE)
      output_file

    }) |> bindEvent(input$generate_report)


    # Observe for updates to the report file and programatically
    # click on the hidden download button
    observe({
      shinyjs::click("download_report")
    }) |> bindEvent(report_path())


    # download handler; note that this is only triggered by
    # changes in the reactive report_path()
    output$download_report <- downloadHandler(
      filename = REPORT_NAME,
      content = function(file) {
        file.copy(report_path(), file)
      }
    )
  })
}
