# © 2025 The Johns Hopkins University Applied Physics Laboratory LLC
# Development of this software was sponsored by the U.S. Government under
# contract no. 75D30124C19958

viz_ui <- function(id) {
  
  ns <- NS(id)
  
    nav_panel(
      title = "Visualization",
      value = ns("viz_main"),
      navset_bar(
        viz_regional_map_ui(ns("viz_region")),
        viz_time_series_ui(ns("viz_time_series")),
        viz_other_time_series_ui(ns("viz_other_time_series")),
        viz_posterior_ui(ns("viz_posterior")),
        add_feature_ui(ns("add_feature")),
        navbar_options = list(class = "card-header-accent", theme = "dark", underline=FALSE)
    )
  )
}

viz_server <- function(id, dc, im, results) {
  moduleServer(
    id,
    function(input, output, session) {
    
    feature_store_getter <- function() {
      req(im$feature_store)
      im$feature_store
    }
    
    # All viz tabs share the same feature store and stored data so calculated
    # features added in one place are immediately available elsewhere.
    viz_regional_map_server(id = "viz_region", dc, im, results, feature_store = feature_store_getter)
    viz_time_series_server(id = "viz_time_series", im, results, feature_store = feature_store_getter)
    viz_other_time_series_server(id = "viz_other_time_series", im, feature_store = feature_store_getter)
    viz_posterior_server(id = "viz_posterior", im, results, feature_store = feature_store_getter)
    add_feature_server("add_feature", dc, im, results, feature_store = feature_store_getter)
  })
}

