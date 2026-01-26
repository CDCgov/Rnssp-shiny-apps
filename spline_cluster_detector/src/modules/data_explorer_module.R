# © 2025 The Johns Hopkins University Applied Physics Laboratory LLC
# Development of this software was sponsored by the U.S. Government under
# contract no. 75D30124C19958

ds_ll <- list(
  scale_heatmap = list(
    l = "Count Transform",
    m = "
    A non-linear transformation (sqrt, log) can aid in visualization when 
    otherwise small volume regions would be obscured by higher-volume regions"
  ),
  zero_handle = list(
    l = "Include Locations with Zero Counts",
    m = "
    By default, locations with no counts across the entire time period are not
    shown. Slide switch to add those locations
    "
  )

)

data_explorer_ui <- function(id) {
  ns <- NS(id)

  # create some controls for the heatmap
  option_controls <- accordion(
    open = TRUE,
    accordion_panel(
      title = "Heatmap Options",
      radioButtons(
        ns("scale_heatmap"),
        label = labeltt(ds_ll[["scale_heatmap"]], placement = "bottom"),
        choices = c("None", "Sqrt", "Log"),
        selected = "None",
        inline = T,
      ),
      input_switch(id = ns("zero_handle"), labeltt(ds_ll[["zero_handle"]]), value = FALSE)
    )
  )
  
  nav_panel(
    title = "Data Explorer",
    navset_card_pill(
      id = ns("explorer_navset"),
      selected = "Data Summary",
      nav_panel(
        title = "Data Summary",
        card(
          min_height = 300,
          withSpinner(dataTableOutput(ns("summarytable"))),
          class = "bg-transparent border-0"
        )
      ),
      nav_panel(
        "Heatmap",
        card(
          full_screen = TRUE,
          min_height = "500px",
          withSpinner(plotlyOutput(ns("heatmap")), caption="Heatmap Loading"),
          option_controls,
          class = "bg-transparent border-0"
        )
      ),
      nav_panel(
        "Time Series",
        card(
          full_screen = TRUE,
          min_height = "500px",
          withSpinner(
            plotlyOutput(ns("tSeries")),
            caption="Time Series Loading"
          ),
          class = 'bg-transparent border-0',
          selectInput(inputId = ns("ts_region_dropdown"),label="Region",choices="All")
        )
      )
    )
  )
}

data_explorer_server <- function(id, results, dc, cc) {
  moduleServer(
    id,
    function(input, output, session) {

      observe(results$summary_stats <- summary_stats())
      observe(results$heatmap <- heatmap())
      observe(results$time_series_plot <- time_series_plot())


      # Hide the tabs if the global reactive use nssp changes
      observe(hideElement("explorer_div")) |> bindEvent(dc$USE_NSSP)

      summary_stats <- reactive({

        req(results$filtered_records_count)
        d <- results$filtered_records_count[count > 0]
        
        if (d[location %in% cc$list_of_locations, .N] == 0) {
          validate("No Cases at these locations; check filters and/or state selection")
        }
        # splineClusterDetector::generate_summary_table(
        generate_summary_table(
          data = d,
          end_date = cc$end_date,
          locations = cc$list_of_locations,
          baseline_length = cc$baseline_length,
          test_length = cc$test_length,
          guard = 0
        ) |>
          datatable(
            rowname = FALSE,
            options = list(dom = "t")
          )
      })

      # Show the Summary Table
      output$summarytable <- renderDataTable(summary_stats())

      observe({

        req(results$filtered_records_count)
        toggleElement(
          id = "zero_handle",
          condition = results$filtered_records_count[count == 0, .N] > 0
        )
      }) |> bindEvent(results$filtered_records_count)

      hm_data <- reactive({
        req(results$filtered_records_count)

        d <- results$filtered_records_count

        if (d[location %in% cc$list_of_locations, .N] == 0) {
          validate("No Cases at these Locations; check State Selection")
        }


        # splineClusterDetector::generate_heatmap_data(
        generate_heatmap_data(
          data = results$filtered_records_count,
          end_date = cc$end_date,
          locations = isolate(cc$list_of_locations),
          baseline_length = cc$baseline_length,
          test_length = cc$test_length,
          guard = 0
        )
      })

      heatmap <- reactive({

        d <- data.table::copy(hm_data())

        if (input$zero_handle == FALSE) {
          locs <- d[, .(v1 = sum(count) > 0), location][v1 == TRUE, unique(location)]
          d <- d[location %in% locs]
        }

        if (input$scale_heatmap == "Sqrt") d[, count := sqrt(count)]


        # Always set any zero's to NA
        d <- d[, count := fifelse(count == 0, NA_integer_, count)]

        # p <- splineClusterDetector::generate_heatmap(
        p <- generate_heatmap(
          heatmap_data = d,
          plot_type = "plotly",
          y = "display_name",
          logscale = input$scale_heatmap == "Log"
        )

        return(p)

      })

      # Heatmap
      output$heatmap <- renderPlotly({
        heatmap()
      })
      
      ts_choices <- reactive({
        req(results$filtered_records_count)
        
        relevant_dates <- as.IDate(
          c(
            get_baseline_dates(cc$end_date, cc$test_length, cc$baseline_length)[1],
            cc$end_date
          )
        )
        print(relevant_dates)

        # updates choices
        choices = unique(
          results$filtered_records_count[between(date, relevant_dates[1], relevant_dates[2])] |> 
            _[count>0, .(location, display_name)]
        )
        print(choices)
        
        # add the "All" option, and return
        c("All" = "All", setNames(
          choices[["location"]], nm = choices[["display_name"]]
        ))
        
      })
      
      observe({
        req(ts_choices())
        
        updateSelectInput(
          inputId = "ts_region_dropdown", 
          choices = ts_choices()
        )
      })

      tsdata <- reactive({

        req(results$filtered_records_count)
        req(input$ts_region_dropdown)

        d <- results$filtered_records_count
        locations <- cc$list_of_locations
        
        # if time series drop down is not "ALL", then we should filter
        # on these locations
        if(input$ts_region_dropdown!="All") {
          locations = input$ts_region_dropdown
        }
        
        if (d[location %in% locations, .N] == 0) {
          validate("No Cases at these Locations; check State Selection")
        }

        data <- generate_time_series_data(
          data = d,
          end_date = cc$end_date,
          locations = locations,
          baseline_length = cc$baseline_length,
          test_length = cc$test_length,
          guard = 0
        )
        
        list(
          data = data, 
          location = names(ts_choices())[which(ts_choices() == input$ts_region_dropdown)]
        )
      })

      time_series_plot <- reactive({
        req(tsdata())
        validate(
          need(tsdata()$data[, sum(date_count)]>0, "No case counts for the selected location")
        )
        
        p <- generate_time_series_plot(
          time_series_data = tsdata()$data,
          end_date = cc$end_date,
          plot_type = "plotly", 
          locations = tsdata()$location
        )

        return(p)
      })

      # Time series
      output$tSeries <- renderPlotly({
        time_series_plot()
      })


    }
  )
}
