
data_loader_ui <- function(id) {

  btn_class <- "btn-primary btn-sm"
  ns <- NS(id)

  nav_panel(
    title = "",
    layout_sidebar(
      sidebar = sidebar(
        id = ns("config_sidebar"),
        width = 450,
        dl_sidebar_ui(ns("data_config")),
        input_task_button(
          id = ns("ingest_btn"),
          label = "Ingest Data",
          class = btn_class,
          label_busy = "Ingesting Data"
        ),
        uiOutput(ns("next_stp_msg"))
      ),
      ingested_data_ui(ns("ingest"))
    )
  )
}


data_loader_server <- function(id, results, data_config, cluster_config, profile, valid_profile) {
  moduleServer(
    id,
    function(input, output, session) {

      ns <- session$ns
      # ---------------------------------------------
      #   Module Calls: Sidebar and Ingested Data
      # ---------------------------------------------
      dl_sidebar_server(id = "data_config", data_config, cluster_config, profile, valid_profile)
      ingested_data_server("ingest", profile, results, data_config, cluster_config, ingest_btn_click, session)

      # ---------------------------------------------
      #   Button Click Reactive
      # ---------------------------------------------
      ingest_btn_click <- reactive(input$ingest_btn)
      
      # Show the next step message, if there is data
      output$next_stp_msg <- renderUI({
        req(results$records)
        tags$span(
          actionLink(
            inputId = ns("go_cluster"),
            label   = "Next Step: Clustering",
            style="color:red;"
          )
        )
      })
      
      # Monitor for step_to_cluster link click
      observe(data_config$step_to_cluster <- isolate(data_config$step_to_cluster) + 1) |> 
        bindEvent(input$go_cluster, ignoreInit = TRUE)
        
      
      # ---------------------------------------------
      # Clear critical objects if data_config$USE_NSSP changes
      # ---------------------------------------------
      observe({
        data_config$source_data <- NULL
        data_config$step_to_cluster <- 0
        results$records <- NULL
        results$records_description <- NULL
        results$map <- NULL
        results$cluster_data <- NULL
        results$cluster_table_display <- NULL
        results$time_series_plot <- NULL
        results$heatmap <- NULL

      }) |> bindEvent(data_config$USE_NSSP)

    }
  )
}
