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

# Side Bar UI elements
sideBarModuleOutput <- function(id) {
  ns <- NS(id)
  
  sidebarPanel(
    width = 2,
    selectInput(ns("State"), "State", states, 'Select a state'),
    dateInput(inputId = ns("Date"), label = "Date", value = EndDate_0, max = EndDate_0),
    radioButtons(
      inputId = ns("normalize_radio"),
      label = "Select normalization option:",
      choices = list("Count(None)" = "count", "Percent" = "percent"),
      selected = "count",
      inline = TRUE  # Set to TRUE for horizontal orientation
    ),
    radioButtons(
      inputId = ns("method_radio"),
      label = "Select detection option:",
      choices = list("Parametric(Gaussian)" = "gauss", "Non-parametric" = "percentile"),
      selected = "gauss",
      inline = TRUE  # Set to TRUE for horizontal orientation
    ),
    actionButton(ns("go"), "Load Data"),
    hidden(p(id = ns("runText"), style = "color:red;", "Processing in progress... Please, wait!")),
    hr(),
    h4("Filter Controls", 
       style = 'font-size:18px; display: inline-block; margin-right: 12px;'),
    helpPopup(
      id = "", 
      word="",
      title = "",
      content = paste0(
        "Record-count minimum filters are only applied to the Syndromic Category table"
      ),
      placement = "bottom", trigger = "hover",
      icon_name = "exclamation-circle",
      icon_style = "color:blue;font-size:12px"
    ),
    fluidRow(
      column(width=6, numericInput(ns("min_records_baseline"), "Baseline Record Minimum:", value = 0)),
      column(width=6, numericInput(ns("min_records_testdate"), "Test Date Record Minimum:", value = 3))
    ),
    hr(),
    actionButton(ns("resetBtn"), "Reset Filters"),
    actionButton(ns("backBtn"), "Back"),
    h4("Current Filters"),
    htmlOutput(ns("selectionText")),
    hr(),
    reportModuleOutput("report")
  )
}

# Side Bar server elements
sideBarModule <- function(input, output, session, master, p_dfs, filters, selection_history, selected_state) {
  ns <- session$ns
  
  #-----------------------------------------------------------------------------
  #----------------------Reactive value pre-allocation/definition---------------
  
  # -------------------------Initialize reactive data elements------------------
  selected <- reactiveValues(state = NULL, date = NULL, normalize = NULL, 
                             method = NULL)
  
  #-----------------------End reactive value definition-------------------------
  #-----------------------------------------------------------------------------
  
  # Reactive function to store the binary (empty df OR empty selections) status
  master_empty_or_no_selections <- reactive({
    is.null(master$df) || length(selection_history$history) %in% c(0,1)
  })
  
  # Reactive function to store the binary empty df status
  master_empty <- reactive({
    is.null(master$df)
  })
  
  #---------------------------"Filer Controls" event handlers-------------------
  
  inputs_invalid <- reactive({
    input$State == 'Select a state'
  })
  
  observe({
    disable <- inputs_invalid()
    shinyjs::toggleState("go", !disable)
  })
  
  # Observe updates to the master_empty_or_no_selections() and toggle data-filtering 
  # "Controls" on/off
  observe({
    disable <- master_empty_or_no_selections()
    shinyjs::toggleState("resetBtn", !disable)
    shinyjs::toggleState("backBtn", !disable)
  })
  
  # Roll the selections back to their previous state
  observeEvent(input$backBtn, {
    showModal(modalDialog("Processing... Please wait", footer = NULL))
    req(length(selection_history$history) > 1)
  
    selection_history$history <- selection_history$history[-length(selection_history$history)]
    prev_selections <- selection_history$history[[length(selection_history$history)]]
    filters$min_records_baseline <- prev_selections$MinRecordsBaseline
    filters$min_records_testdate <- prev_selections$MinRecordsTestDate
    filters$age <- prev_selections$AgeGroup
    filters$sex <- prev_selections$Sex
    filters$region <- prev_selections$HospitalRegion
    filters$subc <- prev_selections$SubCategory_flat
    filters$ccdd <- prev_selections$CCDDCategory_flat
    filters$dd <- prev_selections$C_DiagnosisCode_ICD10_Flat
    filters$ccsr <- prev_selections$ICD_CCSR_flat
    updateNumericInput(session, "min_records_baseline", value = filters$min_records_baseline)
    updateNumericInput(session, "min_records_testdate", value = filters$min_records_testdate)
    filtered(master, filters, p_dfs, selected_state, selected)
    removeModal()
  })
  
  # Observe the reset button click
  observeEvent(input$resetBtn, {
    showModal(modalDialog("Processing... Please wait", footer = NULL))
    selection_history$history <- list(selection_history$history[[1]])
    first_selections <- selection_history$history[[1]]
    filters$min_records_baseline <- first_selections$MinRecordsBaseline
    filters$min_records_testdate <- first_selections$MinRecordsTestDate
    filters$age <- NULL
    filters$sex <- NULL
    filters$region <- NULL
    filters$subc <- NULL
    filters$ccdd <- NULL
    filters$dd <- NULL
    filters$ccsr <- NULL
    updateNumericInput(session, "min_records_baseline", value = filters$min_records_baseline)
    updateNumericInput(session, "min_records_testdate", value = filters$min_records_testdate)
    filtered(master, filters, p_dfs, selected_state, selected)
    removeModal()
  })
  
  # Observe for baseline record minimum submission
  observeEvent(input$min_records_baseline, {
    showModal(modalDialog("Processing... Please wait", footer = NULL))
    req(!master_empty())
    
    new_selection <- input$min_records_baseline
    if (identical(new_selection, filters$min_records_baseline)) {
      return() # If the selection is the same, don't add to history
    }
    filters$min_records_baseline <- input$min_records_baseline
    appendSelectionHistory(selection_history, filters)
    filtered(master, filters, p_dfs, selected_state, selected, record_number=TRUE)
    removeModal()
  }, ignoreInit = TRUE)
  
  # Observe for baseline record minimum submission
  observeEvent(input$min_records_testdate, {
    showModal(modalDialog("Processing... Please wait", footer = NULL))
    req(!master_empty())
    
    new_selection <- input$min_records_testdate
    if (identical(new_selection, filters$min_records_testdate)) {
      return() # If the selection is the same, don't add to history
    }
    filters$min_records_testdate <- input$min_records_testdate
    appendSelectionHistory(selection_history, filters)
    filtered(master, filters, p_dfs, selected_state, selected, record_number=TRUE)
    removeModal()
  }, ignoreInit = TRUE)
  
  #-------------------------------Load Data event-handler-----------------------
  observeEvent(input$go, {
    showModal(modalDialog("Processing... Please wait", footer = NULL))
    shinyjs::show("runText")
    shinyjs::toggleState("go", FALSE)
    # Assign selected reactives for plotly title
    selected$state = input$State
    selected$date = input$Date
    selected$normalize = input$normalize_radio
    selected$method = input$method_radio
    
    showElement("region_map")
    showElement("age_table")
    showElement("sex_table")
    showElement("subc_table")
    showElement("ccdd_table")
    showElement("dd_table")
    showElement("ccsr_table")
    showElement("line_level_table")
    
    # Reset filters
    updateNumericInput(session, "min_records_baseline", value = 0)
    updateNumericInput(session, "min_records_testdate", value = 3)
    filters$min_records_baseline = 0
    filters$min_records_testdate = 3
    filters$age <- NULL
    filters$sex <- NULL
    filters$region <- NULL
    filters$subc <- NULL
    filters$ccdd <- NULL
    filters$dd <- NULL
    filters$ccsr <- NULL
    selection_history$history <- list(list(MinRecordsBaseline = filters$min_records_baseline,
                                           MinRecordsTestDate = filters$min_records_testdate,
                                           HospitalRegion = filters$region,
                                           AgeGroup = filters$age,
                                           Sex = filters$sex,
                                           SubCategory_flat = filters$subc,
                                           CCDDCategory_flat = filters$ccdd,
                                           C_DiagnosisCode_ICD10_Flat = filters$dd,
                                           ICD_CCSR_flat = filters$ccsr))
    
    # State-wide default data processing for visualization
    master$df = readin_and_process_master_df(input$State, input$Date)
    master$df = master$df %>% mutate(AgeGroup = as.character(label_to_category_age_list[as.character(AgeGroup)]))
    master$df = master$df %>% mutate(Sex = as.character(label_to_category_sex_list[as.character(Sex)]))
    master$filtered_df = master$df
    codes_lists <- read_in_codes_description(input)
    master$icd_list <- codes_lists[[1]]
    master$ccsr_list <- codes_lists[[2]]
    master$region_fips <- master$df[c('HospitalRegion', 'FacilityCountyFIPS')] %>% distinct() %>% 
      mutate(FacilityCountyFIPS = factor(FacilityCountyFIPS))
    master$all_dates <- sort(unique(as.Date(master$df$Date, format = "%m/%d/%Y")))
    p_loop_over_all_features(p_dfs, master$df, master$all_dates, selected$normalize, selected$method, input$min_records_baseline, input$min_records_testdate)
    
    # Get selected state geometry
    selected_state$fp = state_helper[state_helper$state_name == input$State,]$state_fips
    selected_state$state_sf = state_sf[state_sf$STATEFP == selected_state$fp,]
    selected_state$county_sf = county_sf[county_sf$STATEFP == selected_state$fp,]
    
    if (!is.null(p_dfs$region)) {
      selected_state$df_sf = st_as_sf(right_join(right_join(p_dfs$region,
                                                            master$region_fips, by="HospitalRegion"),
                                                 selected_state$county_sf[,c("NAME","GEOID","geometry")],
                                                 by = c("FacilityCountyFIPS" = "GEOID")))
    } else {
      region_fips_copy = master$region_fips
      region_fips_copy$p = rep(NA, nrow(region_fips_copy))
      selected_state$df_sf = st_as_sf(right_join(region_fips_copy,
                                                 selected_state$county_sf[,c("NAME","GEOID","geometry")],
                                                 by = c("FacilityCountyFIPS" = "GEOID")))
    }
    shinyjs::toggleState("go", TRUE)
    shinyjs::hide("runText")
    removeModal()
  })
  
  #------------------------------UI methods/filters event-handlers---------------
  
  # Observe for normalization radio selection
  observeEvent(input$normalize_radio, {
    showModal(modalDialog("Processing... Please wait", footer = NULL))
    req(master$df)
    selected$normalize = input$normalize_radio
    p_loop_over_all_features(p_dfs, master$filtered_df, master$all_dates, selected$normalize, selected$method, input$min_records_baseline, input$min_records_testdate)
    selected_state$df_sf = st_as_sf(right_join(right_join(p_dfs$region,
                                                          master$region_fips, by="HospitalRegion"),
                                               selected_state$county_sf[,c("NAME","GEOID","geometry")],
                                               by = c("FacilityCountyFIPS" = "GEOID")))
    removeModal()
  }, ignoreInit = TRUE)
  
  # Observe for method radio selection
  observeEvent(input$method_radio, {
    showModal(modalDialog("Processing... Please wait", footer = NULL))
    req(master$df)
    selected$method = input$method_radio
    p_loop_over_all_features(p_dfs, master$filtered_df, master$all_dates, selected$normalize, selected$method, input$min_records_baseline, input$min_records_testdate)
    selected_state$df_sf = st_as_sf(right_join(right_join(p_dfs$region,
                                                          master$region_fips, by="HospitalRegion"),
                                               selected_state$county_sf[,c("NAME","GEOID","geometry")],
                                               by = c("FacilityCountyFIPS" = "GEOID")))
    removeModal()
  }, ignoreInit = TRUE)
  
  # Display current set of selections to the user
  output$selectionText <- renderText({
    if (length(selection_history$history) > 0) {
      selected_history <- selection_history$history[[length(selection_history$history)]]
      text <- paste("<B>Total Number of Records Within Current Filters (Including Baseline Period) =</B>", nrow(master$filtered_df), "<br/>")
      text <- paste(text, "<B>Number of Test Date Records Within Current Filters =</B>", nrow(master$filtered_df[master$filtered_df$Date==as.Date(selected$date, format = "%m/%d/%Y"),]), "<br/> <br/>")
      if (!is.null(selected_history$MinRecordsBaseline)) {
        text <- paste(text, "<B>Baseline Record Minimum =</B>", selected_history$MinRecordsBaseline, "<br/>")
      }
      if (!is.null(selected_history$MinRecordsTestDate)) {
        text <- paste(text, "<B>Test Date Record Minimum =</B>", selected_history$MinRecordsTestDate, "<br/>")
      }
      if (!is.null(selected_history$HospitalRegion)) {
        text <- paste(text, "<B>Region =</B>", substr(selected_history$HospitalRegion, 4, nchar(selected_history$HospitalRegion)), "<br/>")
      }
      if (!is.null(selected_history$AgeGroup)) {
        text <- paste(text, "<B>Age group =</B>", selected_history$AgeGroup, "<br/>")
      }
      if (!is.null(selected_history$Sex)) {
        text <- paste(text, "<B>Sex =</B>", selected_history$Sex, "<br/>")
      }
      if (!is.null(selected_history$SubCategory_flat)) {
        text <- paste(text, "<B>Sub-Category =</B>", selected_history$SubCategory_flat, "<br/>")
      }
      if (!is.null(selected_history$CCDDCategory_flat)) {
        text <- paste(text, "<B>CCDD Category =</B>", selected_history$CCDDCategory_flat, "<br/>")
      }
      if (!is.null(selected_history$C_DiagnosisCode_ICD10_Flat)) {
        text <- paste(text, "<B>ICD Diagnosis =</B>", ifelse(selected_history$C_DiagnosisCode_ICD10_Flat %in% names(master$icd_list), master$icd_list[selected_history$C_DiagnosisCode_ICD10_Flat], selected_history$C_DiagnosisCode_ICD10_Flat), "<br/>")
      }
      if (!is.null(selected_history$ICD_CCSR_flat)) {
        text <- paste(text, "<B>CCSR =</B>", ifelse(selected_history$ICD_CCSR_flat %in% names(master$ccsr_list), master$ccsr_list[selected_history$ICD_CCSR_flat], selected_history$ICD_CCSR_flat), "<br/>")
      }
      return(text)
    } else {
      return("<B>No Selections Made</B>")
    }
  })
  
  list(
    selected = selected,
    go = reactive(input$go)
  )
}
