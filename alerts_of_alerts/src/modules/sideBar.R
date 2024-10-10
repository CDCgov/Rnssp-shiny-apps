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

sideBarModuleOutput <- function(id) {
  ns <- NS(id)
  
  sidebarPanel(
    width = 3,
    helpPopup(
      id = "", word = "App Summary", title = "App Summary",
      content = paste0(
        "This app tests for and visualizes both temporal and spatial alerts for regions with a user-selected State, for a user-selected CCDD Category, and over a user-defined time period. Temporal alerts are tested for using 3 statewide ",
        "diagnostics of syndrome severity. The 3 state-wide temporal diagnostics are: 1) ",
        "total statewide percent of ED visits, 2) number of alerting ",
        "counties/regions, and 3) the number of counties/regions ",
        "estimated, through the use of Generative Additive Modeling, to have increasing case counts. The second two ",
        "diagnostics are variants of what has been coined 'Alerts of Alerts'. The region-level data associated to or underlying each of these state-level diagnostics ",
        "can be visualized for the currently selected date as a set of ",
        "choropleth maps. Presented with each of these maps are the results of statistical tests ",
        "for spatial autocorrelation (i.e., spatial clustering)."
      ),
      placement = "right", trigger = "click",
      icon_name = "question-circle",
      icon_style = "color:blue;font-size:15px"
    ),
    selectInput(ns("state"), "State", states, "Select a state"),
    selectInput(ns("ccdd"), "CCDD Category", ccdd_cats, "Select a CCDD Category"),
    fluidRow(
      column(
        6,
        dateInput(
          inputId = ns("startDate"), label = "Start Date",
          value = Sys.Date() %m-% months(3)
        )
      ),
      column(
        6,
        dateInput(
          inputId = ns("endDate"), 
          label = "End Date",
          value = Sys.Date() %m-% days(1), 
          max = Sys.Date() %m-% days(1)
        )
      ),
      tags$style(HTML(".datepicker {z-index:99999 !important;}"))
    ),
    actionButton(ns("go"), "Load Data"),
    reportModuleOutput("report"),
    hidden(p(id = ns("runText"), style = "color:red;", "Processing in progress... Please, wait!"))
  )
}

sideBarModule <- function(input, output, session) {
  ns <- session$ns
  
  Reactive_dfs <- reactiveValues(df_1 = NULL, df_2 = NULL)
  
  selected_state <- reactiveValues(
    fp = NULL, state_sf = NULL,
    county_sf = NULL, df_sf = NULL
  )
  
  selected <- reactiveValues(
    state = NULL, CCDD = NULL, startDate = NULL, endDate = NULL, maps_date = NULL
  )
  
  plotly_dims <- reactiveValues(width = NULL, height = NULL)
  
  stat_test <- reactiveValues(
    globalMoran = NULL,
    JoinCount_alert = NULL,
    JoinCount_warning_or_alert = NULL,
    JoinCount_increasing = NULL
  )
  
  
  master_empty <- reactive({
    is.null(Reactive_dfs$df_1)
  })
  
  inputs_invalid <- reactive({
    input$state == 'Select a state' || input$ccdd == 'Select a CCDD Category'
  })
  
  observe({
    disable <- inputs_invalid()
    shinyjs::toggleState("go", !disable)
  })
  
  observe({
    plotly_dims$width <- session$clientData$output_tsPlotly_width
    plotly_dims$height <- session$clientData$output_tsPlotly_height
  })
  
  compute_and_assign_mapping_output_reactives <- reactive({
    
    JoinCount_alert <- NULL
    JoinCount_warning_or_alert = NULL
    JoinCount_increasing = NULL
    globalMoran = NULL
    
    output$date1 <- renderText({selected$maps_date})
    output$date2 <- renderText({selected$maps_date})
    output$date3 <- renderText({selected$maps_date})
    
    selected_state$df_sf = st_as_sf(
      right_join(
        Reactive_dfs$df_2[Reactive_dfs$df_2$date == selected$maps_date,], 
        selected_state$county_sf[,c("NAME","GEOID","geometry")], 
        by = c("fips" = "GEOID")
      )
    )
    
    if (input$state == "All") {
      lookup_table <- st_drop_geometry(selected_state$df_sf) %>%
        filter(!is.na(state_abbr)) %>%
        mutate(fips_prefix = substr(fips, 1, 2)) %>%
        select(fips_prefix, state_abbr) %>%
        mutate(state_abbr = ifelse(state_abbr == "District of Columbia", "DC", state_abbr)) %>%
        distinct()
      
      selected_state$df_sf = selected_state$df_sf %>%
        mutate(fips_prefix = substr(fips, 1, 2)) %>%
        left_join(lookup_table, by = "fips_prefix", suffix = c("", "_lookup")) %>%
        mutate(state_abbr = ifelse(is.na(state_abbr), state_abbr_lookup, state_abbr)) %>%
        mutate(NAME = paste(NAME, state_abbr, sep = ", ")) %>%
        select(-fips_prefix, -state_abbr_lookup)
    }
    
    # get non-null rows for analysis
    df_sf_non_null = selected_state$df_sf[!is.na(selected_state$df_sf$county),]
    assign('df_sf_non_null', df_sf_non_null, envir=.GlobalEnv)
    
    if (nrow(df_sf_non_null) > 0) {
      # Compute local moran df
      pts <- st_centroid(st_transform(df_sf_non_null, 3857))
      #> Warning: st_centroid assumes attributes are constant over geometries of x
      #nb <- dnearneigh(pts, 0, 100000)
      
      # Convert to a matrix of coordinates
      coords <- st_coordinates(pts)
      
      # Find k nearest neighbors
      k <- 20
      k_actual = min(k, nrow(df_sf_non_null)-1)
      knn <- knearneigh(coords, k = k_actual)
      nb <- knn2nb(knn)
      
      # Moran's I with Inverse Distance Weighting with alpha(exponent) = 1
      listw <- nb2listwdist(nb, as(pts, "Spatial"), type = "idw",
                            alpha = 1, zero.policy = TRUE, style = "raw")
      
      # check for 0 counties in each of the bins: alerting, increasing
      if (nlevels(factor(df_sf_non_null$alert_percent_factor)) > 1) {
        JoinCount_alert <- joincount.test(factor(df_sf_non_null$alert_percent_factor), listw, alternative = 'greater')
      } 
      
      if (nlevels(factor(df_sf_non_null$warning_or_alert_percent_factor)) > 1) {
        JoinCount_warning_or_alert <- joincount.test(factor(df_sf_non_null$warning_or_alert_percent_factor), listw, alternative = 'greater')
      } 
      
      if (nlevels(factor(df_sf_non_null$inc_factor)) > 1) {
        JoinCount_increasing <- joincount.test(factor(df_sf_non_null$inc_factor), listw, alternative = 'greater')
      }
      
      globalMoran <- tryCatch(
        moran.test(df_sf_non_null$p, listw, alternative = 'greater'),
        error = function(e) NULL
      )
    }
    
    stat_test$JoinCount_alert = ifelse(is.null(JoinCount_alert), "NA", round(JoinCount_alert[[2]]$p.value[[1]],3))
    stat_test$JoinCount_warning_or_alert = ifelse(is.null(JoinCount_warning_or_alert), "NA", round(JoinCount_warning_or_alert[[2]]$p.value[[1]],3))
    stat_test$JoinCount_increasing = ifelse(is.null(JoinCount_increasing), "NA", round(JoinCount_increasing[[2]]$p.value[[1]],3))
    stat_test$globalMoran = ifelse(is.null(globalMoran$p.value), "NA", round(globalMoran$p.value,3))
    
    output$globalmoran <- renderText({paste0("Global Moran's I p-value: ", stat_test$globalMoran)})
    output$joincount_alert <- renderText({paste0("Join Count 'Alert' p-value: ", stat_test$JoinCount_alert)})
    output$joincount_warning_or_alert <- renderText({paste0("Join Count 'Alert' or 'Warning' p-value: ", stat_test$JoinCount_warning_or_alert)})
    output$joincount_inc <- renderText({paste0("Join Count 'Increasing' p-value: ", stat_test$JoinCount_increasing)})
  })
  
  get_and_mutate_dfs <- get_proc_data(input, output, session)
  
  
  observeEvent(input$go, {
    data_proc_running <- TRUE
    shinyjs::show("runText")
    shinyjs::toggleState("go", FALSE)
    dfs <- get_and_mutate_dfs()
    
    
    if (is.null(dfs)) {
      showModal(modalDialog(
        title = "No Data",
        HTML("There is no data available for the current selections.<br>
             Please make another selection."),
        easyClose = TRUE,
        footer = modalButton("Close")
      ))
      
      # Trigger reload when the modal is closed by the user
      observeEvent(input$shiny.modal.close, {
        session$reload()  # Perform a hard reset when modal is closed
      })
      shinyjs::toggleState("go", TRUE)
      shinyjs::hide("runText")
      data_proc_running <- FALSE
      
      return()  # Ensure no further processing occurs
    }
    
    # Update the reactive values
    Reactive_dfs$df_1 <- dfs[[1]]
    Reactive_dfs$df_2 <- dfs[[2]]
    
    # Assign selected reactives for plotly title
    selected$state = input$state
    selected$CCDD = input$ccdd
    
    # Get selected state geometry
    if (input$state == 'All'){
      selected_state$fp = state_helper$state_fips
      selected_state$state_sf = state_sf
      selected_state$county_sf = county_sf
    } else {
      selected_state$fp = state_helper[state_helper$state_name == input$state,]$state_fips
      selected_state$state_sf = state_sf[state_sf$STATEFP == selected_state$fp,]
      selected_state$county_sf = county_sf[county_sf$STATEFP == selected_state$fp,]
    }
    selected$startDate = as.character(input$startDate)
    selected$endDate = as.character(input$endDate)
    selected$maps_date = as.character(input$endDate)
    
    compute_and_assign_mapping_output_reactives()
    
    shinyjs::toggleState("go", TRUE)
    shinyjs::hide("runText")
    data_proc_running <- FALSE
  })
  
  observeEvent(event_data("plotly_click", source = 'plotlyts'), {
    selected$maps_date <- event_data("plotly_click", source = 'plotlyts')$x[1]
    compute_and_assign_mapping_output_reactives()
  })
  
  
  list(
    reactive_output = compute_and_assign_mapping_output_reactives,
    state = reactive(input$state),
    ccdd = reactive(input$ccdd),
    startDate = reactive(input$startDate),
    endDate = reactive(input$endDate),
    go = reactive(input$go),
    Reactive_dfs = Reactive_dfs,
    selected_state = selected_state,
    selected = selected,
    plotly_dims = plotly_dims,
    stat_test = stat_test
  )
}