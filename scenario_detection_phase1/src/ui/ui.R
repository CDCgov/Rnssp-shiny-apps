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

appUI <- function() {
  tagList(
    useShinyjs(),
    tags$head(
      HTML(
        "<script>
          var socket_timeout_interval
          var n = 0
          $(document).on('shiny:connected', function(event) {
          socket_timeout_interval = setInterval(function(){
          Shiny.onInputChange('count', n++)
          }, 15000)
          });
          $(document).on('shiny:disconnected', function(event) {
          clearInterval(socket_timeout_interval)
          });
      </script>
      "
      )           
    ),
    theme = shinytheme("cosmo"),
    navbarPage(
      title = "Scenario Detection",
      theme = shinytheme("cosmo"),
      id = "nav",
      appPanel_alert_navigator,
      docPanel
    )
  )
}