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

appPanel <- tabPanel(
  "Application",
  sidebarLayout(
    sideBarModuleOutput("sideBar"),
    mainPanelModuleOutput("mainPanel")
  )
)