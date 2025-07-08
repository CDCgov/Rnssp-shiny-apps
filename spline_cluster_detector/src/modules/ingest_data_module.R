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

# ingest module
ingested_data_ui <- function(id) {
  ns <- NS(id)
  hidden(div(
    id = ns("ingest_data_card"),
    card(
      id = ns("ingest_data_card"),
      min_height = 300,
      withSpinner(dataTableOutput(ns("inputdata")), 
                  caption = "Loading Data (API calls can take some time)"),
      card_footer(
        downloadButton(ns("download_ingested_data"), "Download Data", class = "btn-primary btn-sm"),
        actionButton(ns("url"), "Show URL/Source Path", class = "btn-primary btn-sm")
      ),
      class = 'bg-transparent border-0'
    ), 
    hidden(card(
      id = ns("data_details_card"),
      min_height=300,
      withSpinner(
        dataTableOutput(ns("data_details"))
      )
    )
    )
  ))
}

ingested_data_server <- function(id, profile,  results, dc, cc, ibc, parent_session) {
  moduleServer(
    id,
    function(input, output, session) {
      
      observe({
        results$records <- dt_events()$data
        results$filtered_records_count <- dt_events()$data
        results$data_details <- dt_events()$data_details
        results$records_description <- dt_events()$description
      })
      observe({
        cc$distance_locations <- dl()$loc_vec
        cc$distance_matrix <- dl()$distance_matrix
      })
    
      # Hide the data card if the global reactive use nssp changes
      observe(hideElement("ingest_data_card")) |> bindEvent(dc$USE_NSSP)
      
      
      # only get new distance information is state or res change
      dl <- reactive({
        req(dc$state2, dc$res)
        get_location_information(dc$state2, dc$res)
      }) |>  bindEvent(ibc())
      
      # ---------------------------------------------
      #   Load Data
      # ---------------------------------------------
      dt_events <- reactive({
        
        results$records <- NULL
        results$records_description <- NULL
        results$filtered_records <- NULL
        results$filtered_records_count <- NULL
        results$data_details <- NULL
        results$map <- NULL
        results$cluster_data <- NULL
        results$cluster_table_display <- NULL
        results$time_series_plot <- NULL
        results$heatmap <- NULL
        
        showElement(id ="ingest_data_card")
        
        toggle_task_button_color(parent_session$ns("ingest_btn"), busy=TRUE)
        
        tryCatch(
          {
            if(is.null(dc$source_data)) {
              stop("No URL, No API Call, or No File / Invalid File Type / Invalid File Format")
            }
            
            if(is.null(dc$res) || identical(dc$res, character(0))) {
              stop("No geographic level selected. Please choose zip or county")
            }
            if(dc$state2 == "US") {
              if(dc$res == "zip") stop("Only county-level available for US; change from zip to county.")
              if(dc$data_type == "details") stop("Only table-builder queries allowed for US.")
            }
            
            
            if(dc$USE_NSSP && dc$ad_hoc) {
              
              # We have been passed a url
              if(!dc$custom_url_valid) {
                stop("Custom URL is not valid")
              }
              
              data = get_custom_url_data(
                dc$custom_url,
                profile()
              )
              description = dc$custom_url
              
            } else {
              if(dc$USE_NSSP) {
                
                # check that the dates are valid
                start_date = dc$url_params$start_date
                end_date = dc$url_params$end_date
                if((end_date-start_date)>MAX_DATE_RANGE) {
                  validate(
                    paste0("The date range exceeds the current max of ", MAX_DATE_RANGE, " days.")
                  )
                }
                
                sd = do.call(generate_url, dc$url_params)
                description = sd  
              }
              else {
                sd = dc$source_data
                description = "Local File Uploaded"
              }
              
              
              data = get_data(
                source_data = sd,
                USE_NSSP = dc$USE_NSSP,
                profile = profile(),
                state = dc$state2,
                res=dc$res,
                data_type = dc$data_type, 
                data_source=dc$data_source,
                deduplicate = dc$dedup
              )
            }
          },
          error=function(e) {
            print(e)
            validate(e$message)
          },
          finally = {
            update_task_button(session = parent_session, "ingest_btn",state="ready")
            toggle_task_button_color(parent_session$ns("ingest_btn"), busy=FALSE)
          }
            
        )
        
        
        return(list(
          data = data[["data"]],
          data_details =  data[["data_details"]],
          description = description
        ))
        
        
      }) |> bindEvent(ibc())
      
      
      # show modal box if url button is pressed
      observe({
        
        if(dc$USE_NSSP) message <- dt_events()$description
        else message <- "LOCAL FILE SELECTED/LOADED"

        showModal( 
          modalDialog( 
            title = "Data Source (Esc to Close)",
            easyClose = TRUE,
            size = "l", 
            card(div(message, style="font-size:80%"))
          ) 
        ) 
      }) |> bindEvent(input$url)  
      
      # Show the data table
      output$inputdata <- renderDataTable({
        
        req(dt_events()$data)
        
        if(nrow(dt_events()$data)==0) {
          validate("No Data Returned/Inputted")
        }
        datatable(
          dt_events()$data[count>0, list(display_name, date, count)],
          colnames = c("Location", "Date", "Count"),
          rownames = FALSE
        ) 
      },server=TRUE) 
      
      # Download Handler
      output$download_ingested_data <- downloadHandler(
        filename = function() "data.csv",
        content = function(file) data.table::fwrite(dt_events()$data, file)
      )
      
      # Show the data details
      output$data_details <- renderDataTable({
        
        req(dt_events()$data_details)
        
        if(nrow(dt_events()$data_details)==0) {
          validate("No Data Returned/Inputted")
        }
        datatable(
          dt_events()$data_details,
          rownames = FALSE
        ) 
      },server=TRUE) 
      
    }  
  )
}
