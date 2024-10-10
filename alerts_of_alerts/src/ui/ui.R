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
      title = "Alerts of Alerts",
      theme = shinytheme("cosmo"),
      id = "nav",
      appPanel,
      docPanel
    )
  )
}