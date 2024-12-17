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

# Main panel UI elements
mainPanelModuleOutput <- function(id) {
  ns <- NS(id)

  mainPanel(width=9,
            helpPopup(
              id = "", word="App Summary", title = "App Summary",
              content = paste0(
                HTML("This app is designed for exploring state-level health data 
                  alerts. Users can visualize alerts based on geographic and 
                  demographic information, and can filtering and analyzing the data 
                  through interactive maps and tables. Alerts are computed using 
                  either parametric (Gaussian) or non-parametric (percentile) 
                  detection methods, helping identify significant deviations in 
                  emergency department visit counts across each pictured feature. 
                  Clicking on a sequence of elements across the map and tables 
                  progressively filters the underlying records. The resulting alerts 
                  depict anomalousness with respect to the subset of records which 
                  remain. <br><br>
                  
                  For more information on the underlying methods used and the 
                  intended interpretaion of results, please read the 
                  'Documentation' panel."
                )),
              placement = "right", trigger = "hover",
              icon_style = "color:blue;font-size:15px"
            ),
            tabsetPanel(
              tabPanel("Alerts Explorer",
                       fluidRow(
                         column(width = 4,
                                h4("Region p-values", 
                                   style = 'font-size:18px; display: inline-block; margin-right: 12px;'),
                                helpPopup(
                                  id = "", 
                                  word="Help",
                                  title = "",
                                  content = paste0(
                                    "This choropleth map shows the anomalousness of emergency department 
                                      visit rates with respect to the current set of filters, stratified by 
                                      hospital region. Clicking on a particular region filters the remaining records 
                                      for only those which occurred in the selected region."
                                  ),
                                  placement = "bottom", trigger = "hover",
                                  icon_name = "question-circle",
                                  icon_style = "color:blue;font-size:12px"
                                ),
                                withSpinner(leafletOutput(ns("region_map"), height = "300px"))),
                         column(width = 4,
                                h4("Age p-values", 
                                   style = 'font-size:18px; display: inline-block; margin-right: 12px;'),
                                helpPopup(
                                  id = "", 
                                  word="Help",
                                  title = "",
                                  content = paste0(
                                    "This table shows the anomalousness of emergency department 
                                      visit rates with respect to the current set of filters, stratified by 
                                      age group. Clicking on a particular row filters the remaining records 
                                      for only those in the selected age range."
                                  ),
                                  placement = "bottom", trigger = "hover",
                                  icon_name = "question-circle",
                                  icon_style = "color:blue;font-size:12px"
                                ),
                                withSpinner(DTOutput(ns("age_table")))),
                         column(width = 4,
                                h4("Sex p-values", 
                                   style = 'font-size:18px; display: inline-block; margin-right: 12px;'),
                                helpPopup(
                                  id = "", 
                                  word="Help",
                                  title = "",
                                  content = paste0(
                                    "This table shows the anomalousness of emergency department 
                                      visit rates with respect to the current set of filters, stratified by 
                                      sex. Clicking on a particular row filters the remaining records 
                                      for only those with the selected sex."
                                  ),
                                  placement = "bottom", trigger = "hover",
                                  icon_name = "question-circle",
                                  icon_style = "color:blue;font-size:12px"
                                ),
                                withSpinner(DTOutput(ns("sex_table"))))
                       ),
                       fluidRow(
                         column(width = 12,
                                h4("Syndromic Category p-values", 
                                   style = 'font-size:18px; display: inline-block; margin-right: 12px;'),
                                helpPopup(
                                  id = "", 
                                  word="Help",
                                  title = "",
                                  content = paste0(
                                    "This table shows the anomalousness of emergency department 
                                      visit rates with respect to the current set of filters, stratified by 
                                      all the most up-to-date elements of Sub-syndrome, CCDD Category, and ICD 
                                      Diagnosis code. Clicking on a particular row filters the remaining 
                                      records for only those with the selected Sub-syndrome, CCDD Category, 
                                      or ICD Diagnosis."
                                  ),
                                  placement = "bottom", trigger = "hover",
                                  icon_name = "question-circle",
                                  icon_style = "color:blue;font-size:12px"
                                ),
                                withSpinner(DTOutput(ns("combined_table"))))
                       )
              ),
              tabPanel("Line-Level Details",
                       column(width = 12,
                              h4("Patient Records ", 
                                 style = 'font-size:18px; display: inline-block; margin-right: 12px;'),
                              helpPopup(
                                id = "", 
                                word="Help",
                                title = "",
                                content = paste0(
                                  "This table shows the line-level details for the records corresponding to 
                                    the current set of applied filters. The defualt view applies an additional (removable) 
                                    filter for only the test date to which the detector algorithms are being applied."
                                ),
                                placement = "bottom", trigger = "hover",
                                icon_name = "question-circle",
                                icon_style = "color:blue;font-size:12px"
                              ),
                              withSpinner(DTOutput(ns("line_level_table"))),
                              downloadButton(ns("download_excel"), "Download Excel"))
              )
            )
  )
}

# Main Panel server elements
mainPanelModule <- function(input, output, session, sideBarInput, master, p_dfs, filters, selection_history, selected_state) {
  
  #--------------------------------Output reactives-----------------------------
  region_map <- reactive({
    req(p_dfs$region)
    normalize = sideBarInput()$selected$normalize
    method = sideBarInput()$selected$method
    
    if (normalize == "count" & method == "gauss") {
      labels_p <- sprintf(
        "<strong>County: </strong>%s<br/>
        <strong>p-value (Normal): </strong>%s<br/>
        <strong>N: </strong>%s<br/>",
        selected_state$df_sf$NAME,
        selected_state$df_sf$p,
        selected_state$df_sf$N
      ) %>%
        lapply(htmltools::HTML)
    } else if (normalize == "percent" & method == "gauss") {
      labels_p <- sprintf(
        "<strong>County: </strong>%s<br/>
        <strong>p-value (Normal): </strong>%s<br/>
        <strong>Percent: </strong>%s%%<br/>
        <strong>N: </strong>%s",
        selected_state$df_sf$NAME,
        selected_state$df_sf$p,
        selected_state$df_sf$Percent,
        selected_state$df_sf$N
      ) %>%
        lapply(htmltools::HTML)
    } else if (normalize == "count" & method == "percentile") {
      labels_p <- sprintf(
        "<strong>County: </strong>%s<br/>
        <strong>p-value (Percentile): </strong>%s<br/>
        <strong>Count: </strong>%s<br/>",
        selected_state$df_sf$NAME,
        selected_state$df_sf$p,
        selected_state$df_sf$N
      ) %>%
        lapply(htmltools::HTML)
    } else {
      labels_p <- sprintf(
        "<strong>County: </strong>%s<br/>
        <strong>p-value (Percentile): </strong>%s<br/>
        <strong>Percent: </strong>%s%%<br/>
        <strong>N: </strong>%s",
        selected_state$df_sf$NAME,
        selected_state$df_sf$p,
        selected_state$df_sf$Percent,
        selected_state$df_sf$N
      ) %>%
        lapply(htmltools::HTML)
    }
    
    # Define breaks and create alerts_cut
    breaks <- c(0, 0.01, 0.05, 1)
    alerts_cut <- cut(
      selected_state$df_sf$p,
      breaks = breaks,
      labels = c('Alert', 'Warning', 'Normal'),
      include.lowest = TRUE
    )
    
    # Handle NA levels
    alerts_cut <- addNA(alerts_cut)
    levels(alerts_cut)[is.na(levels(alerts_cut))] <- 'No records available'
    
    # Assign the resulting factor back to the dataframe
    selected_state$df_sf$alerts <- factor(as.character(alerts_cut))
    
    pal_alerts <- colorFactor(
      palette = c('blue','yellow','red', 'white'), # 'black'
      levels = c('Normal', 'Warning', 'Alert', 'No records available'), #'Not reporting'
      na.color = "white"
    )
    
    p_leaf <-
      leaflet() %>%
      leaflet.extras::setMapWidgetStyle(list(background = "#FFFFFF")) %>%
      leaflet.extras::addFullscreenControl() %>%  # Add fullscreen control
      leaflet.extras::addResetMapButton() %>%  # Add reset button
      addPolylines(
        data = selected_state$state_sf,
        opacity = 1,
        fillOpacity = 0,
        color = "black",
        weight = 1.2
      ) %>%
      addPolygons(
        data = selected_state$df_sf,
        layerId=~NAME,
        stroke = TRUE,
        smoothFactor = 0.5,
        color = "black",
        fillColor = ~pal_alerts(alerts),
        weight = 1.0,
        opacity = 1.0,
        fillOpacity = 0.75,
        highlight = highlightOptions(
          weight = 1,
          color = "black",
          fillOpacity = 0.7,
          opacity = 1.0
        ),
        label = labels_p,
        labelOptions = labelOptions(
          style = list(
            "font-weight" = "normal",
            padding = "3px 8px"
          ),
          textsize = "12px",
          direction = "auto"
        ),
        group = 'counties'
      ) %>%
      addLegendFactor(
        position = "bottomright",
        height = 8,
        width = 10,
        title = "Alerts",
        labelStyle = 'font-size: 12px;',
        pal = pal_alerts,
        values = factor(c('Normal', 'Warning', 'Alert', 'No records available'), levels=c('Normal', 'Warning', 'Alert', 'No records available')),
        opacity = 1,
        fillOpacity = 0.75
      )
  })
  
  age_table <- reactive({
    req(p_dfs$age)
    normalize = sideBarInput()$selected$normalize
    method = sideBarInput()$selected$method
    
    # Clone p_dfs$age to avoid modifying the reactiveValue directly
    age_data <- p_dfs$age
    colnames(age_data)[1] <- "Age Group"
    
    if (method == 'gauss') {
      colnames(age_data)[2] <- "p (Normal)"
    } else {
      colnames(age_data)[2] <- "p (Percentile)"
    }
    
    if (normalize == "percent") {
      percentColumnDef = list(
        targets = 2,
        render = DT::JS(
          "function(data, type, row, meta) {",
          "  if(type === 'display'){",
          "    return data + '%';",
          "  } else {",
          "    return data;",
          "  }",
          "}"
        )
      )
      coldefs = list(percentColumnDef)
    } else {
      coldefs = NULL
    }
    
    dt <- datatable(age_data, selection='single', options = list(dom = 't', columnDefs = coldefs), rownames=FALSE)
    
    column_to_style <- if (method == 'gauss') 'p (Normal)' else 'p (Percentile)'
    
    dt <- dt %>%
      formatStyle(
        column_to_style,
        backgroundColor = styleInterval(
          c(0.01, 0.05),
          c('rgba(255, 0, 0, 0.5)', 'rgba(255, 255, 0, 0.5)', 'rgba(0, 0, 255, 0.5)')
        )
      )
    dt
  })
  
  sex_table <- reactive({
    req(p_dfs$sex)
    normalize = sideBarInput()$selected$normalize
    method = sideBarInput()$selected$method
    
    # Clone p_dfs$sex to avoid modifying the reactiveValue directly
    sex_data <- p_dfs$sex
    
    if (method == 'gauss') {
      colnames(sex_data)[2] <- "p (Normal)"
    } else {
      colnames(sex_data)[2] <- "p (Percentile)"
    }
    
    if (normalize == "percent") {
      percentColumnDef = list(
        targets = 2,
        render = DT::JS(
          "function(data, type, row, meta) {",
          "  if(type === 'display'){",
          "    return data + '%';",
          "  } else {",
          "    return data;",
          "  }",
          "}"
        )
      )
      coldefs = list(percentColumnDef)
    } else {
      coldefs = NULL
    }
    
    #, columnDefs = coldefs
    dt <- datatable(sex_data, selection='single', options = list(dom = 't', columnDefs = coldefs), rownames=FALSE)
    
    column_to_style <- if (method == 'gauss') 'p (Normal)' else 'p (Percentile)'
    
    dt <- dt %>%
      formatStyle(
        column_to_style,
        backgroundColor = styleInterval(
          c(0.01, 0.05),
          c('rgba(255, 0, 0, 0.5)', 'rgba(255, 255, 0, 0.5)', 'rgba(0, 0, 255, 0.5)')
        )
      )
    
    dt
  })
  
  # Reactive for processing combined_df
  processed_combined_df <- reactive({
    req(master$df)
    normalize <- sideBarInput()$selected$normalize
    method <- sideBarInput()$selected$method
    
    if (!is.null(c(p_dfs$subc, p_dfs$ccddp, p_dfs$dd, p_dfs$ccsr))) {
      # SubCategory_flat
      subc_table <- p_dfs$subc
      if (!is.null(subc_table)) {
        colnames(subc_table)[1] <- "Sub-syndrome"
      }
      
      # CC&DDCategory_flat
      ccdd_table <- p_dfs$ccdd
      if (!is.null(ccdd_table)) {
        colnames(ccdd_table)[1] <- "CCDD Category"
      }
      
      dd_table <- p_dfs$dd
      # DischargeDiagnosis
      if (!is.null(dd_table)) {
        ICDDiagnosis <- ifelse(dd_table$C_DiagnosisCode_ICD10_Flat %in% names(master$icd_list), 
                               paste0(dd_table$C_DiagnosisCode_ICD10_Flat, ": ", 
                                      unname(master$icd_list[dd_table$C_DiagnosisCode_ICD10_Flat])), 
                               paste0(dd_table$C_DiagnosisCode_ICD10_Flat, " (not in ICD list)"))
        
        dd_table <- dd_table[, !(names(dd_table) %in% "C_DiagnosisCode_ICD10_Flat")]
        dd_table <- cbind(ICDDiagnosis, dd_table)
        colnames(dd_table)[1] <- "ICD Diagnosis"
      }
      
      ccsr_table <- p_dfs$ccsr
      # CCSR
      if (!is.null(ccsr_table)) {
        CCSR <- ifelse(ccsr_table$ICD_CCSR_flat %in% names(master$ccsr_list), 
                       paste0(ccsr_table$ICD_CCSR_flat, ": ",
                              unname(master$ccsr_list[ccsr_table$ICD_CCSR_flat])), 
                       paste0(ccsr_table$ICD_CCSR_flat, " (not in CCSR list)"))
        ccsr_table <- ccsr_table[, !(names(ccsr_table) %in% "ICD_CCSR_flat")]
        ccsr_table <- cbind(CCSR, ccsr_table)
        colnames(ccsr_table)[1] <- "CCSR Category"
      }
      
      combined_df <- list(
        if (!is.null(dd_table)) dd_table %>% mutate("Source Field" = "ICD Diagnosis") else NULL,
        if (!is.null(ccsr_table)) ccsr_table %>% mutate("Source Field" = "CCSR Category") else NULL,
        if (!is.null(subc_table)) subc_table %>% mutate("Source Field" = "Sub-syndrome") else NULL,
        if (!is.null(ccdd_table)) ccdd_table %>% mutate("Source Field" = "CCDD Category") else NULL
      ) %>%
        purrr::compact() %>%
        bind_rows()
      
      existing_columns <- intersect(c("ICD Diagnosis", "CCSR Category", "Sub-syndrome", "CCDD Category"), names(combined_df))
      
      if (length(existing_columns) > 0) {
        # Dynamically construct the columns to select
        columns_to_select <- c(
          "Source Field",
          "Syndromic Category",
          "p",
          if (normalize == "count") "N" else "Percent",
          if (normalize == "percent") "N"
        )
        
        combined_df <- combined_df %>%
          pivot_longer(
            cols = all_of(existing_columns),
            names_to = "variable", 
            values_to = "Syndromic Category",
            values_drop_na = TRUE
          ) %>%
          filter(!`Syndromic Category` %in% c("none", "Unmapped (not in CCSR list)")) %>%
          select(all_of(columns_to_select)) %>%
          arrange(p) # Ensure `p` is treated as a column name
      } else {
        combined_df <- NULL
      }
      
      # Assign column names dynamically based on method
      if (method == 'gauss') {
        colnames(combined_df)[3] <- "p (Normal)"
      } else {
        colnames(combined_df)[3] <- "p (Percentile)"
      }
      
      combined_df[['Source Field']] <- as.factor(combined_df[['Source Field']])
      
      combined_df  # Return the processed combined_df
    } else {
      NULL  # Return NULL if inputs are missing
    }
  })
  
  # Reactive for creating the datatable using processed_combined_df
  combined_table <- reactive({
    req(master$df)
    normalize <- sideBarInput()$selected$normalize
    method <- sideBarInput()$selected$method
    combined_df <- processed_combined_df()
    
    if (is.null(combined_df)) {
      method_col <- ifelse(method == 'gauss', 'p', 'Percentile')
      normalize_col <- ifelse(normalize == 'count', 'N', 'Percent')
      combined_df <- setNames(
        data.frame(`Source Field` = character(0), `Syndromic Category` = character(0), numeric(0), numeric(0)),
        c('Source Field', 'Syndromic Category', method_col, normalize_col)
      )
      return(datatable(combined_df))
    }
    
    # Column definitions
    baseColDefs <- list(
      list(searchable = TRUE,
           filter = 'select',
           targets = 0,
           render = DT::JS(
             "function(data, type, row, meta) {",
             "return type === 'display' && data.length > 55 ?",
             "'<span title=\"' + data + '\">' + data.substr(0, 55) + '...</span>' : data;",
             "}")
      ),
      list(searchable = TRUE, targets = c(1)),
      list(searchable = FALSE, targets = c(2,3))
    )
    
    if (normalize == "percent") {
      percentColumnDef = list(
        targets = 3,
        render = DT::JS(
          "function(data, type, row, meta) {",
          "  if(type === 'display'){",
          "    return data + '%';",
          "  } else {",
          "    return data;",
          "  }",
          "}"
        )
      )
      baseColDefs = append(baseColDefs, list(percentColumnDef))
    }
    
    # Create the datatable
    dt = datatable(combined_df, selection = 'single', filter = 'top',
              options = list(dom = 'ftip', columnDefs = baseColDefs),
              rownames = FALSE)
    
    # Define the column to format based on method
    column_to_format <- if (method == 'gauss') 'p (Normal)' else 'p (Percentile)'
    
    dt = dt %>% formatStyle(
        column_to_format,
        backgroundColor = styleInterval(c(0.01, 0.05), 
                                        c('rgba(255, 0, 0, 0.5)', 'rgba(255, 255, 0, 0.5)', 'rgba(0, 0, 255, 0.5)'))
      )
    dt
  })
  
  # Observe block to update p_dfs$combined based on processed_combined_df
  observe({
    p_dfs$combined <- processed_combined_df()
  })
  
  
  line_level_table <- reactive({
    req(master$df)
    # Select the appropriate data frame based on the condition
    df_to_display <- if (length(selection_history$history) > 1) {
      master$filtered_df
    } else {
      master$df
    }
    
    df_to_display <- df_to_display %>%
      mutate(C_DiagnosisCode_ICD10_Flat = lapply(C_DiagnosisCode_ICD10_Flat, function(dd) as.character(master$icd_list[dd]))) %>%
      mutate(ICD_CCSR_flat = lapply(ICD_CCSR_flat, function(ccsr) as.character(master$ccsr_list[ccsr]))) %>%
      mutate(C_DiagnosisCode_ICD10_Flat = sapply(C_DiagnosisCode_ICD10_Flat, function(x) paste(x, collapse = "; "))) %>%
      mutate(SubCategory_flat = sapply(SubCategory_flat, function(x) paste(x, collapse = "; "))) %>%
      mutate(CCDDCategory_flat = sapply(CCDDCategory_flat, function(x) paste(x, collapse = "; "))) %>%
      mutate(ICD_CCSR_flat = sapply(ICD_CCSR_flat, function(x) paste(x, collapse = "; ")))
    
    df_to_display$Date = as.factor(df_to_display$Date)
    df_to_display$HospitalRegion = as.factor(df_to_display$HospitalRegion)
    df_to_display$AgeGroup = as.factor(df_to_display$AgeGroup)
    df_to_display$Sex = as.factor(df_to_display$Sex)
    df_to_display$FacilityCountyFIPS = as.factor(df_to_display$FacilityCountyFIPS)
    
    datatable(df_to_display, selection='none', filter='top',
              options = list(dom = 'Bftip', autowidth = TRUE,
                             searchCols = list(
                               list(search = ""),
                               list(search = paste0('["', format(sideBarInput()$selected$date, "%m/%d/%Y"), '"]')),
                               list(search = ""),
                               list(search = ""),
                               list(search = ""),
                               list(search = ""),
                               list(search = ""),
                               list(search = ""),
                               list(search = ""),
                               list(search = ""),
                               list(search = "")
                             ),
                             columnDefs = list(
                               list(
                                 targets = c(0,5,6,7,9,10),
                                 render = DT::JS(
                                   "function(data, type, row, meta) {",
                                   "return type === 'display' && data.length > 15 ?",
                                   "'<span title=\"' + data + '\">' + data.substr(0, 15) + '...</span>' : data;",
                                   "}")
                               ),
                               list(searchable = TRUE, targets = c(1,2,3,4,8), filter = 'select'),
                               list(searchable = TRUE, targets = c(0,5,6,7,9,10)))
              ), callback = JS('table.page(3).draw(false);'), rownames=FALSE)
  })
  
  #-----------------------------Output render objects---------------------------
  # Panel 1: Interactive Leaflet map
  output$region_map <- renderLeaflet({
    region_map()
  })
  
  # Panel 2: Interactive DataFrame 1
  output$age_table <- renderDT({
    age_table()
  })
  
  # Panel 7: Interactive DataFrame 6
  output$sex_table <- renderDT({
    sex_table()
  })
  
  # Panel 3: Interactive DataFrame 2
  output$combined_table <- renderDT({
    combined_table()
  })
  
  # Panel 6: Interactive DataFrame 5
  output$line_level_table <- renderDT({
    line_level_table()
  })
  
  # Excel download handler
  output$download_excel <- downloadHandler(
    filename = function() {
      paste("line_level_", Sys.Date(), ".xlsx", sep = "")
    },
    content = function(file) {
      df_to_display <- if (length(selection_history$history) > 1) {
        master$filtered_df
      } else {
        master$df
      }
      
      df_to_display <- df_to_display %>% 
        mutate(C_DiagnosisCode_ICD10_Flat = lapply(C_DiagnosisCode_ICD10_Flat, function(dd) as.character(master$icd_list[dd]))) %>%
        mutate(ICD_CCSR_flat = lapply(ICD_CCSR_flat, function(ccsr) as.character(master$ccsr_list[ccsr]))) %>%
        mutate(C_DiagnosisCode_ICD10_Flat = sapply(C_DiagnosisCode_ICD10_Flat, function(x) paste(x, collapse = "; "))) %>%
        mutate(SubCategory_flat = sapply(SubCategory_flat, function(x) paste(x, collapse = "; "))) %>%
        mutate(CCDDCategory_flat = sapply(CCDDCategory_flat, function(x) paste(x, collapse = "; "))) %>%
        mutate(ICD_CCSR_flat = sapply(ICD_CCSR_flat, function(x) paste(x, collapse = "; ")))
      
      df_to_display$Date = as.factor(df_to_display$Date)
      df_to_display$HospitalRegion = as.factor(df_to_display$HospitalRegion)
      df_to_display$AgeGroup = as.factor(df_to_display$AgeGroup)
      df_to_display$Sex = as.factor(df_to_display$Sex)
      df_to_display$FacilityCountyFIPS = as.factor(df_to_display$FacilityCountyFIPS)
      
      # Create workbook
      wb <- createWorkbook()
      
      # Add worksheet "Sheet 1" to the workbook
      addWorksheet(wb, 'Sheet 1')
      
      # Adds the first 30 lines of the iris dataset to the sheet "Sheet 1"
      writeDataTable(wb, sheet = 1, x = df_to_display)
      
      # Creates a string named "validate" with Male,Female,Unknown
      validate <-
        paste0("\"",
               paste0(levels(df_to_display$Sex), collapse = ','),
               "\"")
      
      # Add drop-downs to the first column on the sheet "Sheet 1"
      dataValidation(wb, 1, col = 5, rows = 2:11586, type = 'list', value = validate)
      
      # Save the workbook
      saveWorkbook(wb, file, overwrite = TRUE)
    }
  )
  
  #-------------------Observes for main panel outputs-----------------------------  
  # Observe for Region Map clicks
  observeEvent(input$region_map_shape_click, {
    new_selection <- selected_state$df_sf[selected_state$df_sf$NAME == input$region_map_shape_click$id,]$HospitalRegion
    if (identical(new_selection, filters$region) || is.na(new_selection)) {
      return() # If the selection is the same, don't add to history
    }
    filters$region <- new_selection
    appendSelectionHistory(selection_history, filters)
    filtered(master, filters, p_dfs, selected_state, sideBarInput()$selected)
  })
  
  # Observe for AgeGroup table clicks
  observeEvent(input$age_table_rows_selected, {
    new_selection <- p_dfs$age[input$age_table_rows_selected,1]
    if (identical(new_selection, filters$age) || nrow(p_dfs$age)==1) {
      return() # If the selection is the same, don't add to history
    }
    filters$age <- new_selection
    appendSelectionHistory(selection_history, filters)
    filtered(master, filters, p_dfs, selected_state, sideBarInput()$selected)
  })
  
  # Observe for Sex table clicks
  observeEvent(input$sex_table_rows_selected, {
    new_selection <- p_dfs$sex[input$sex_table_rows_selected,1]
    if (identical(new_selection, filters$sex) || nrow(p_dfs$sex)==1) {
      return() # If the selection is the same, don't add to history
    }
    filters$sex <- new_selection
    appendSelectionHistory(selection_history, filters)
    filtered(master, filters, p_dfs, selected_state, sideBarInput()$selected)
  })
  
  # Observe for combined syndromic category table clicks
  observeEvent(input$combined_table_rows_selected, {
    row_index = input$combined_table_rows_selected
    source_field_nice <- p_dfs$combined[row_index,1]
    new_selection <- p_dfs$combined[row_index,2][[1,1]]
    if (source_field_nice == "Sub-syndrome") {
      if (identical(new_selection, filters$subc)) {
        return() # If the selection is the same, don't add to history
      } else {
        filters$subc <- new_selection
        appendSelectionHistory(selection_history, filters)
        filtered(master, filters, p_dfs, selected_state, sideBarInput()$selected)
      }
    } else if (source_field_nice == "ICD Diagnosis") {
      new_selection = substring(new_selection, 6)
      new_selection = names(master$icd_list)[which(master$icd_list == new_selection)]
      if (identical(new_selection, filters$dd)) {
        return() # If the selection is the same, don't add to history
      } else {
        filters$dd <- new_selection
        appendSelectionHistory(selection_history, filters)
        filtered(master, filters, p_dfs, selected_state, sideBarInput()$selected)
      }
    } else if (source_field_nice == "CCSR Category") {
      new_selection = substring(new_selection, 9)
      new_selection = names(master$ccsr_list)[which(master$ccsr_list == new_selection)]
      if (identical(new_selection, filters$ccsr)) {
        return() # If the selection is the same, don't add to history
      } else {
        filters$ccsr <- new_selection
        appendSelectionHistory(selection_history, filters)
        filtered(master, filters, p_dfs, selected_state, sideBarInput()$selected)
      }
    } else if (source_field_nice == "CCDD Category") {
      if (identical(new_selection, filters$ccdd)) {
        return() # If the selection is the same, don't add to history
      } else {
        filters$ccdd <- new_selection
        appendSelectionHistory(selection_history, filters)
        filtered(master, filters, p_dfs, selected_state, sideBarInput()$selected)
      }
    }
  })
  
  list(
    region_object = region_map,
    age_object = age_table,
    sex_object = sex_table,
    diagnostic_object = combined_table
  )
}
