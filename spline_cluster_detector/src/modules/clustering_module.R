
clustering_ui <- function(id) {
  
  btn_class <- "btn-primary btn-sm"
  ns <- NS(id)
  nav_panel(
    title = "Clustering",
    layout_sidebar(
      sidebar = sidebar(
        id = ns("cluster_sidebar"),
        width = 450,
        clust_sidebar_ui(ns("clust_config")),
        input_task_button(
          ns("clusters_btn"), "Compute Clusters", class = btn_class,
          icon = bsicons::bs_icon(name="diagram-3"),
          label_busy = "Computing Clusters"
        )
      ),
      navset_card_underline(
        data_explorer_ui(id=ns("explorer")),
        compute_clusters_ui(id=ns("cluster")),
        cluster_map_ui(id=ns("map")),
        cluster_line_listing_ui(id=ns("cluster_ll")),
        selected = "Data Explorer",
        id = ns("cluster_navset")
      )
    )
  )
}


clustering_server <- function(id,results, dc, cc, profile) {
  moduleServer(
    id,
    function(input, output, session) {
      
      compute_trigger <- reactiveVal(FALSE)
      cluster_btn_click <- reactive(input$clusters_btn)
      
      observe({
        updateNavlistPanel(inputId = "cluster_navset", selected="Cluster Summary")
        session$onFlushed(once = TRUE, function() {
          compute_trigger(TRUE)
        })
      }) |> bindEvent(cluster_btn_click())
      
      clust_sidebar_server(id = "clust_config", results, dc, cc)
      data_explorer_server(id = "explorer", results, dc, cc)
      compute_clusters_server(id="cluster", results, dc, cc, compute_trigger, session)
      cluster_map_server(id="map", results, dc, cc)
      cluster_line_listing_server(id="cluster_ll", results, dc, cc, profile, cluster_btn_click)
      
    }
  )
}
