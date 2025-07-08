# © 2025 The Johns Hopkins University Applied Physics Laboratory LLC
# Development of this software was sponsored by the U.S. Government under
# contract no. 75D30124C19958

get_location_information <- function(st, res) {
  if(st == "US") return(us_distance_matrix())
  if(res == "zip") return(zip_distance_matrix(st =st))
  return(county_distance_matrix(st=st))
}

ds_ll <- list(
  scale_heatmap = list(
    l = "Count Transform",
    m = "Transformation to apply to the counts by location"
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
      title="Heatmap Options", 
      radioButtons(
        ns("scale_heatmap"), 
        label = labeltt(ds_ll[["scale_heatmap"]],placement="bottom"),
        choices = c("None", "Sqrt","Log"),
        selected = "None",
        inline = T,
      ),
      input_switch(id=ns("zero_handle"),labeltt(ds_ll[["zero_handle"]]),value = FALSE)
    )
  )

  nav_panel(
    title="Data Explorer",
    navset_card_pill(
      #widths = c(1,11),
      #well = FALSE,
      id = ns("explorer_navset"),
      selected = "Data Summary",
      nav_panel(
        title = "Data Summary",
        card(
          min_height = 300, 
          withSpinner(dataTableOutput(ns("summarytable"))),
          class = 'bg-transparent border-0'
        )
      ),
      nav_panel(
        "Heatmap", 
        card(
          full_screen = TRUE, 
          min_height = "500px",
          plotlyOutput(ns("heatmap")),
          option_controls,
          class = 'bg-transparent border-0'
        )
      ),
      nav_panel(
        "Time Series",
        card(
          full_screen=TRUE,
          min_height = "500px",
          plotlyOutput(ns("tSeries")),
          class = 'bg-transparent border-0'
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
        d = results$filtered_records_count[count>0]

        if(d[location %in% cc$distance_locations, .N] == 0) {
          validate("No Cases at these locations; check filters and/or state selection")
        }
        #splineClusterDetector::generate_summary_table(
        generate_summary_table(
          data = d, 
          end_date = cc$end_date,
          locations = cc$distance_locations,
          baseline_length = cc$baseline_length,
          test_length = cc$test_length,
          guard = 0
        ) |> 
          datatable(
            rowname=FALSE,
            options = list(dom='t')
          )
      }) 
      
      # Show the Summary Table
      output$summarytable <- renderDataTable(summary_stats())
      

      observe({
        
        req(results$filtered_records_count)
        toggleElement(
          id = "zero_handle",
          condition = results$filtered_records_count[count==0, .N] > 0
        )
      }) |> bindEvent(results$filtered_records_count)
      
      hm_data <- reactive({
        req(results$filtered_records_count)
        
        d = results$filtered_records_count
        
        if(d[location %in% cc$distance_locations, .N] == 0) {
          validate("No Cases at these Locations; check State Selection")
        }
        
        
        #splineClusterDetector::generate_heatmap_data(
        generate_heatmap_data(
          data = results$filtered_records_count,
          end_date = cc$end_date,
          locations = isolate(cc$distance_locations),
          baseline_length = cc$baseline_length,
          test_length = cc$test_length, 
          guard = 0
        )
      })
      
      heatmap <- reactive({
        
        d = data.table::copy(hm_data())
        
        if(input$zero_handle == FALSE) {
          locs <- d[, .(v1 = sum(count)>0), location][v1==TRUE, unique(location)]
          d <- d[location %in% locs]            
        }
        
        if(input$scale_heatmap == "Sqrt") d[, count:=sqrt(count)]
        
        
        # Always set any zero's to NA
        d <- d[, count:=fifelse(count==0, NA_integer_,count)]
        
        #p <- splineClusterDetector::generate_heatmap(
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
      
      tsdata <- reactive({
        
        req(results$filtered_records_count)
        
        d = results$filtered_records_count
        
        if(d[location %in% cc$distance_locations, .N] == 0) {
          validate("No Cases at these Locations; check State Selection")
        }
        
        #data <- splineClusterDetector::generate_time_series_data(
        data <- generate_time_series_data(
          data = results$filtered_records_count,
          end_date = cc$end_date,
          locations = isolate(cc$distance_locations),
          baseline_length = cc$baseline_length,
          test_length = cc$test_length, 
          guard = 0
        )
      })
      
      time_series_plot <- reactive({
        
        #p <- splineClusterDetector::generate_time_series_plot(
        p <- generate_time_series_plot(
          time_series_data = tsdata(),
          end_date = cc$end_date,
          plot_type = "plotly"
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
