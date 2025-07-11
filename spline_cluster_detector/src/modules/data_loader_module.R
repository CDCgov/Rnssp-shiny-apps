
data_loader_ui <- function(id) {

  btn_class <- "btn-primary btn-sm"
  ns <- NS(id)

  nav_panel(
    title = "Data Loader",
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
        )
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

      # ---------------------------------------------
      #   Sidebar Toggle (currently toggle capability is turned off)
      # ---------------------------------------------
      # observe({
      #   toggle_sidebar(
      #     id = "config_sidebar",
      #     open = !is.data.frame(results$records)
      #   )
      # }) |> bindEvent(ingest_btn_click(),results$records)
      #

      # ---------------------------------------------
      # Clear critical objects if data_config$USE_NSSP changes
      # ---------------------------------------------
      observe({
        data_config$source_data <- NULL
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
