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

appPanel_alert_navigator <- tabPanel(
  "Alert Navigator",
  sidebarLayout(
    sideBarModuleOutput_alertNavigator("sideBar_alertNavigator"),
    mainPanelModuleOutput_alertNavigator("mainPanel_alertNavigator")
  )
)