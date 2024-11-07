# © 2024 The Johns Hopkins University Applied Physics Laboratory LLC
# Development of this software was sponsored by the U.S. Government under contracts no. 75D30120C07643, 75D30122C15442

#----------------------------------------------------
# Alerts of Alerts App
# Authors:
#   Joshua Kimrey
#   Michael Sheppard
#   Raghav Ramachandran
#   Howard Burkom
#   Roseric Azondekon
#----------------------------------------------------

# Load helper functions
source("src/helpers/global.R")
source("src/helpers/helpers.R")
source("src/helpers/dataproc.R")

# Load Modules
source("src/modules/sideBar.R")
source("src/modules/mainPanel.R")
source("src/modules/report.R")

# Load ui components
source("src/ui/ui.R")
source("src/ui/appUI.R")
source("src/ui/docUI.R")


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
  
  # Call side bar module
  userInput <- callModule(sideBarModule, "sideBar")
  
  # Call main module
  widgets <- callModule(mainPanelModule, "mainPanel", reactive(userInput))
  
  # Call report module when the `Load Data` button is clicked
  observeEvent(userInput$go, {
    callModule(reportModule, "report", reactive(userInput), reactive(widgets))
  })
  
}

shinyApp(ui, server)
