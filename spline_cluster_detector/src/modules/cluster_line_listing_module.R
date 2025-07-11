# © 2025 The Johns Hopkins University Applied Physics Laboratory LLC
# Development of this software was sponsored by the U.S. Government under
# contract no. 75D30124C19958


cluster_line_listing_ui <- function(id) {
  ns <- NS(id)
  nav_panel(
    id = ns("cluster_ll_panel"),
    title = "Cluster Line Listing",
    layout_columns(
      tags$div(
        tags$h4("Note: limited line level can be generated under certain conditions:"),
        tags$ul(
          tags$li("At least one cluster must be found"),
          tags$li("Original data source must be patient level", tags$span(style = "color:red", "(working to expand to facility)")),
          tags$li("Data must have been loaded using the API URL builder (not custom url)")
        ),
        hidden(
          input_task_button(
            id = ns("generate_ll_task"),
            label = "Generate Line Level Data",
            class = "btn-secondary btn-sm"
          )
        )
      ),
      card(
        card_body(
          hidden(selectizeInput(
            ns("ll_cluster_selector"),
            "Choose Cluster Center(s)",
            choices = NULL
          )),
          hidden(input_switch(ns("unmask"), "Show Actual Values", value = TRUE)),
          style = "overflow: visible"
        ),
        style = "overflow: visible"
      ),
      col_widths = c(8, 4), min_height = "200px"
    ),
    hidden(card(
      id = ns("cluster_ll_card"),
      full_screen = TRUE,
      card_header("Line-Level Listing of Encounters by Cluster", class = "bg-primary"),
      withSpinner(
        dataTableOutput(ns("cluster_ll"))
      ),
      card_footer(downloadButton(
        ns("ll_download_btn"),
        "Download Line Listing",
        class = "btn-primary btn-sm"
      ))
    )
  ))
}

cluster_line_listing_server <- function(id, results, dc, cc, profile, recompute) {
  moduleServer(
    id,
    function(input, output, session) {

      clusters_found <- reactive({
        "data.frame" %in% class(results$cluster_data[[1]])
      })

      conditions_met <- reactive({
        clusters_found() &&
          dc$USE_NSSP == TRUE &&
          dc$data_source == "patient" &&
          dc$ad_hoc == FALSE
      })

      ll_data <- reactiveVal(NULL)

      observe({
        ll_data(NULL)
      }) |> bindEvent(recompute())

      observe({
        if (!conditions_met()) ll_data(NULL)
      }) |> bindEvent(conditions_met())


      observe({
        if (clusters_found()) {
          ll <- tryCatch(
            get_line_listing_from_clusters(
              cluster_table = results$cluster_data_extended,
              state_abbreviation = dc$state2,
              end_date = cc$end_date,
              res = dc$res,
              synd_drop_menu = dc$synd_drop_menu,
              synd_cat = dc$synd,
              data_source = dc$data_source,
              profile = profile(),
              # dedup value is FALSE IF not details; if details, then we take the
              # dc$dedup value
              dedup = dc$dedup && dc$data_type == "details",
              reference_details = results$filtered_records
            ),
            error = function(e) {
              print(e)
              return("Error getting line_level data")
            },
            warning = function(w) {
              print(w)
              return("Warning/possible problem getting line_level data")
            }
          )
          ll_data(ll)
        }
      }) |> bindEvent(input$generate_ll_task)

      observe({
        req(ll_data)
        updateSelectizeInput(
          inputId = "ll_cluster_selector",
          choices = c("All", ll_data()[, unique(Center)])
        )
      }) |> bindEvent(ll_data())

      data_to_display <- reactive({
        req(ll_data())
        if (input$unmask) ll_data()
        else obscure_pii(
          ll_data(),
          names(ll_data())[which(names(ll_data()) != "Center")]
        )

      }) |> bindEvent(ll_data(), input$unmask)

      # render the line_level_data
      output$cluster_ll <- renderDataTable({
        req(ll_data())
        c_filter <- input$ll_cluster_selector
        DT::datatable(
          # obscure_pii(
          #   ll_data()[{if(c_filter=="All") TRUE else Center %in% c_filter}],
          #   cols = NULL
          # ),
          data_to_display()[{
            if (c_filter == "All") TRUE else Center %in% c_filter
          }],
          colnames = c(
            "Center", "Cluster Date", "Biosense ID",
            "Hospital Name", "Visit Date", "Visit Time",
            "Region", "State", "Zip Code", "Sex", "Age",
            "Race/Ethnicity Combined", "Chief Complaint(s)",
            "Discharge Diagnosis(es)",
            "Diagnosis Combination", "Triage Notes (Orig)",
            "Disposition Category", "Visit ID", "Hospital",
            "Hospital Region", "Facility Type"
          ),
          rownames = FALSE,
          filter = "top",
          options = list(pageLength = 10, dom = "tip")
        )
      })

      # hide the ll data card if no ll_data()
      observe(toggleElement("cluster_ll_card", condition = !is.null(nrow(ll_data()))))
      # hide the ll center selector if no ll_data()
      observe(toggleElement("ll_cluster_selector", condition = !is.null(nrow(ll_data()))))
      # observe(toggleElement("unmask", condition=!is.null(nrow(ll_data()))))

      # hide the generate LL button if no clusters
      observe(toggleElement("generate_ll_task", condition = conditions_met()))

      # download handler
      output$ll_download_btn <- downloadHandler(
        filename = "line_listing.csv",
        content = function(file) {
          data.table::fwrite(ll_data(), file)
        }
      )

    }
  )
}
