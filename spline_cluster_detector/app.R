# © 2025 The Johns Hopkins University Applied Physics Laboratory LLC
# Development of this software was sponsored by the U.S. Government under
# contract no. 75D30124C19958



# for debugging
# options(
#   warn               = 2,
#   shiny.sanitize.errors = FALSE,
#   shiny.error       = browser
# )


# source files --------------------------------------------------------------
source("src/00_setup.R")

# ui ----------------------------------------------------------------------

ui <- page(
  theme = THEME,
  tags$head(tags$style(HTML(paste0("
        .shiny-output-error-validation {
          color: red;
        }
        .card {border: 0;}
        .bslib-full-screen-enter {
          background-color: ", PRIMARY_COLOR, "; /* A darker gray for better contrast in dark mode */
          color: #fff; /* White text for contrast */
        }
      ")))),
  useShinyjs(),
  page_navbar(
    title = "Spline Based Cluster Determination",
    data_loader_ui("data_loader"),
    clustering_ui("clustering"),
    nav_panel(
      "Documentation",
      tags$iframe(src = "documentation.html",
        style  = "width: 100%; height: 800px; border: none;",
        seamless = "seamless"
      )
    ),
    nav_spacer(),
    nav_item(report_ui("report")),
    nav_item(input_dark_mode()),
    navbar_options = list(class = "bg-primary", theme = "dark", underline = FALSE)
  )
)

# server -------------
server <- function(input, output, session) {
  
  options(shiny.maxRequestSize = 30 * 1024^2) 
  
  # ----------------------------------------------------------------------
  # Global Reactives for Profile
  # ----------------------------------------------------------------------
  profile <- reactiveVal(CREDENTIALS$profile)
  valid_profile <- reactiveVal(CREDENTIALS$valid)
  
  # ----------------------------------------------------------------------
  # Global Reactives for Data Configuration
  # ----------------------------------------------------------------------
  data_config <- reactiveValues(
    # basic data characteristics
    res = NULL, state = NULL, state2 = NULL,
    data_load_start = NULL, data_load_end = NULL,

    # data content/syndrome characteristics
    synd_summary = NULL,

    # url and source info
    url_params = NULL, source_data = NULL, USE_NSSP = FALSE, data_type = NULL,
    data_source = NULL, custom_url = NULL, custom_url_valid = TRUE,
    ad_hoc = FALSE, dedup = FALSE
  )
  
  # ----------------------------------------------------------------------
  # Global Reactives for Cluster Configuration
  # ----------------------------------------------------------------------
  
  cluster_config <- reactiveValues(
    radius = NULL,
    test_length = NULL, end_date = NULL, baseline_length = NULL,
    distance_locations = NULL, distance_matrix = NULL,
    spline_lookup = NULL, spline_value = NULL, base_adj_meth = NULL,
    filters = NULL
  )
  
  # ----------------------------------------------------------------------
  # Global Reactives for Main Results
  # ----------------------------------------------------------------------
  results <- reactiveValues(
    cluster_data = NULL, cluster_table_display = NULL, cluster_data_extended = NULL,
    map = NULL, heatmap = NULL,
    time_series_plot = NULL, summary_stats = NULL, records = NULL, records_description = NULL,
    data_details = NULL, filtered_records = NULL, filtered_records_count = NULL
  )
  
  # ---------------------------------------------------------
  #   Module Calls: Data Loader, Clustering, and Report
  # ---------------------------------------------------------
  data_loader_server("data_loader", results, data_config, cluster_config, profile, valid_profile)
  clustering_server("clustering", results, data_config, cluster_config, profile)
  report_server("report", results, data_config, cluster_config)
  

}

#-----------
shinyApp(ui, server)
