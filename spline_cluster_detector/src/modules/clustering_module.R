
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
          icon = bsicons::bs_icon(name = "diagram-3"),
          label_busy = "Computing Clusters"
        )
      ),
      # navset_card_underline(
      #   data_explorer_ui(id = ns("explorer")),
      #   compute_clusters_ui(id = ns("cluster")),
      #   cluster_map_ui(id = ns("map")),
      #   cluster_line_listing_ui(id = ns("cluster_ll")),
      #   selected = "Data Explorer",
      #   id = ns("cluster_navset")
      # )
      navset_card_underline(
        nav_panel("Data Explorer", value = "explorer", data_explorer_ui(id = ns("explorer"))),
        nav_panel("Cluster Summary", value = "cluster", compute_clusters_ui(id = ns("cluster"))),
        nav_panel("Map", value = "map", cluster_map_ui(id = ns("map"))),
        nav_panel("Line Listing", value = "cluster_ll", cluster_line_listing_ui(id = ns("cluster_ll"))),
        selected = "explorer",
        id = ns("cluster_navset")
      )
    )
  )
}


clustering_server <- function(id, results, dc, cc, profile) {
  moduleServer(
    id,
    function(input, output, session) {
      
      ns = session$ns
      
      session$onFlushed(function() {
        runjs("$('a[data-value=\"cluster\"]').addClass('disabled-tab');")
        runjs("$('a[data-value=\"map\"]').addClass('disabled-tab');")
        runjs("$('a[data-value=\"cluster_ll\"]').addClass('disabled-tab');")
      }, once = TRUE)
      
      observe({
        runjs("$('a[data-value=\"cluster\"]').removeClass('disabled-tab');")
        runjs("$('a[data-value=\"map\"]').removeClass('disabled-tab');")
        runjs("$('a[data-value=\"cluster_ll\"]').removeClass('disabled-tab');")
        updateTabsetPanel(inputId = "cluster_navset", selected = "cluster")
      }) |> bindEvent(cluster_btn_click())
      

      observe({
        # Disable all non-explorer tabs
        runjs("$('a[data-value=\"cluster\"]').addClass('disabled-tab');")
        runjs("$('a[data-value=\"map\"]').addClass('disabled-tab');")
        runjs("$('a[data-value=\"cluster_ll\"]').addClass('disabled-tab');")
        updateTabsetPanel(inputId = "cluster_navset", selected = "explorer")
      }) |> bindEvent(reactiveValuesToList(cc),reactiveValuesToList(dc))
      
      compute_trigger <- reactiveVal(FALSE)
      cluster_btn_click <- reactive(input$clusters_btn)
  
      observe({
        session$onFlushed(once = TRUE, function() {
          compute_trigger(TRUE)
        })
      }) |> bindEvent(cluster_btn_click())

      clust_sidebar_server(id = "clust_config", results, dc, cc)
      data_explorer_server(id = "explorer", results, dc, cc)
      compute_clusters_server(id = "cluster", results, dc, cc, compute_trigger, session)
      cluster_map_server(id = "map", results, dc, cc)
      cluster_line_listing_server(id = "cluster_ll", results, dc, cc, profile, cluster_btn_click)

    }
  )
}
