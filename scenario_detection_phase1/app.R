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

# Load helper functions
source("src/helpers/global.R", local=TRUE)
source("src/helpers/helpers.R", local=TRUE)
source("src/helpers/dataproc_alert_navigator.R", local=TRUE)

# Load Modules
source("src/modules/sideBar_alertNavigator.R", local=TRUE)
source("src/modules/mainPanel_alertNavigator.R", local=TRUE)
source("src/modules/report.R", local=TRUE)

# Load ui components
source("src/ui/ui.R", local=TRUE)
source("src/ui/appUI_alertNavigator.R", local=TRUE)
source("src/ui/docUI.R", local=TRUE)

options(shiny.reactlog = TRUE)

## ui --------------------------------------------------------------------------
ui <- appUI()


## server ----------------------------------------------------------------------
server <- function(input, output, session) {
  options(warn = -1)
  
  # To avoid RStudio timeouts -- server code
  output$keepAlive <- renderText({
    req(input$count)
    paste("keep alive ", input$count)
  })
  
  #--Initialize reactivevalues shared between sidebar and mainpanel modules
  master <- reactiveValues(df = NULL, filtered_df = NULL, all_dates = NULL,
                           region_fips = NULL, icd_list = NULL, ccsr_list = NULL)
  
  p_dfs <- reactiveValues(region = NULL, age = NULL, sex = NULL, subc = NULL,
                          ccdd = NULL, dd = NULL, ccsr = NULL, combined = NULL)
  
  filters <- reactiveValues(region = NULL, age = NULL, sex = NULL, subc = NULL,
                            ccdd = NULL, dd = NULL, ccsr = NULL, 
                            min_records_baseline = NULL, min_records_testdate = NULL)
  
  selection_history <- reactiveValues(history = list())
  
  selected_state <- reactiveValues(fp = NULL, state_sf = NULL, county_sf = NULL,
                                   df_sf = NULL)
  #----------------end shared reactiveValues initialization------------------
  
  # Call sidebar module, passing reactive values
  userInput <- callModule(sideBarModule_alertNavigator, "sideBar_alertNavigator",
                          master = master,
                          p_dfs = p_dfs,
                          filters = filters,
                          selection_history = selection_history,
                          selected_state = selected_state)
  
  # Call main panel module, passing the same reactive values
  widgets_alert_navigator <- callModule(mainPanelModule_alertNavigator, "mainPanel_alertNavigator",
                        master = master,
                        p_dfs = p_dfs,
                        filters = filters,
                        selection_history = selection_history,
                        selected_state = selected_state,
                        reactive(userInput))
  
  # Call report module when the `Load Data` button is clicked
  observeEvent(userInput$go, {
    callModule(reportModule, "report", reactive(userInput), reactive(widgets_alert_navigator), master, filters)
  })
}

shinyApp(ui, server)