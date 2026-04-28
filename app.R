# © 2025 The Johns Hopkins University Applied Physics Laboratory LLC
# Development of this software was sponsored by the U.S. Government under
# contract no. 75D30124C19958

# source files --------------------------------------------------------------
source("src/00_setup.R")


options(shiny.maxRequestSize = 1000*1024^2) 
is_tutorial_view <- function(query_string) {
  view <- shiny::parseQueryString(sub("^\\?", "", query_string %||% ""))$view
  identical(view, "tutorial")
}

ui <- function(request) {
  if (is_tutorial_view(request$QUERY_STRING)) {
    return(tutorial_page_ui())
  }
  
  bslib::page(
    # get theme from the setup file
    theme = THEME,
    global_ui_tags,
    useShinyjs(),
    page_navbar(
      id = "main_nav",
      title = "Bayesian Spatiotemporal Modeling",
      # Data Loader
      data_loader_ui("data_load"),
      # INLA Modeling
      inla_model_ui("inla_model"),
      # VIZ Ui
      viz_ui("viz"),
      # Spacer
      nav_spacer(),
      # Extras
      extras_ui("extras"),
      # Nav options
      navbar_options = list(class = "card-header-accent top-dark-nav", theme = "dark", underline = FALSE)
    )
  )
}

server <- function(input, output, session) {
  output$tutorial_page_content <- renderUI(
    render_documentation_content("src/documentation/tutorial.md")
  )
  
  if (is_tutorial_view(session$request$QUERY_STRING)) {
    return(invisible(NULL))
  }

  # ----------------------------------------------------------------------
  # Global Reactives for Profile
  # ----------------------------------------------------------------------
  profile <- reactiveVal(CREDENTIALS$profile)
  valid_profile <- reactiveVal(CREDENTIALS$valid)
  
  observe({
    if(!valid_profile() && ALLOW_SHINY_CREDENTIALS == TRUE)
      credServer("creds", profile, valid_profile)
  })
  
  # ----------------------------------------------------------------------
  # Global Reactives for configuration and results
  # ----------------------------------------------------------------------
  
  # data loader configuration reactives
  dc <- reactiveValues(
    physical_adj = NULL, mobility_adj = NULL,
    time_res = NULL, geo_res=NULL, drange=NULL, synd_cat=NULL, synd_drop_menu=NULL,
    states = NULL, selected_counties = NULL, includes_alaska_hawaii = NULL
  )
  
  # use this to transfer cache widget values from load query or load model
  # through modules
  cache_transitions <- reactiveValues(
    states=NULL, selected_counties=NULL, geo_res=NULL, time_res=NULL, drange=NULL,
    synd_cat=NULL, synd_drop_menu=NULL
  )
  
  # inla model configuration reactives
  im <- reactiveValues(
    model = NULL, data_cls = NULL, posterior = NULL, nforecasts = NULL,
    feature_store = NULL, feature_choices = NULL, features_df = NULL
  ) 
  
  # results reactives (data, plotsm etc; add to as needed for future reporting)
  results <- reactiveValues(
    data = NULL
  )  
  
  # ----------------------------------------------------------------------
  # Module Server calls
  # ----------------------------------------------------------------------
  data_loader_server(id = "data_load", dc, results, profile, valid_profile, cache_transitions)
  inla_model_server(id = "inla_model", dc, im, results, cache_transitions)
  viz_server("viz", dc, im, results)
  tooltip_server("tooltip")
  extras_server("extras")
  
  # ----------------------------------------------------------------------
  # Other functionality
  # ----------------------------------------------------------------------
  
  # Hide the main viz panel until we have a model
  observe(toggleState(
    condition = !is.null(im$model), selector = 'a[data-value="viz-viz_main"]'
  ))

}


#-----------
shinyApp(ui, server)

