# © 2025 The Johns Hopkins University Applied Physics Laboratory LLC
# Development of this software was sponsored by the U.S. Government under
# contract no. 75D30124C19958

map_height <- "1000px"
cluster_map_ui <- function(id) {
  ns <- NS(id)
  nav_panel(
    id = ns("cluster_map_panel"),
    title = "Cluster Map",
    hidden(card(
      id = ns("map_card"),
      min_height = map_height,
      full_screen = TRUE,
      card_header(htmlOutput(outputId = ns("map_summary")), class = "bg-primary"),
      withSpinner(
        leafletOutput(ns("clustermap"), height = map_height)
      ),
      card_footer(htmlOutput(outputId = ns("cluster_psa")))
    )
  ))
}

cluster_map_server <- function(id, results, dc, cc) {
  moduleServer(
    id,
    function(input, output, session) {

      observe(results$map <- cluster_map())

      clusters_found <- reactive({
        "data.frame" %in% class(results$cluster_data[[1]])
      })

      # # # REVEAL THE MAP ONLY IF CLUSTERS FOUND
      # observe({
      #     showElement(id = "map_btn")
      #   } else {
      #     hideElement(id = "map_btn")
      #   }
      # }) |> bindEvent(results$cluster_data)

      # Reactive holds the map data
      map_data <- reactive({

        if (clusters_found()) {
          map_data <- tryCatch(
            generate_leaflet_data(
              cluster_data = results$cluster_data,
              state = dc$state2,
              distance_matrix = cc$distance_matrix,
              level = dc$res,
              zctas_from_tigris = ZCTAS_FROM_TIGRIS
            ),
            error = function(e) "Error Mapping Data",
            warning = function(w) "Error Mapping Data"
          )
        } else map_data <- "No Clusters To Map"
      }) |> bindEvent(clusters_found())


      # Map reactive object
      cluster_map <- reactive({
        if (!is.null(nrow(map_data()))) {
          generate_leaflet_plot(map_data(), level = isolate(dc$res))[["plot"]]
        }
      })

      # render the cluster map
      output$clustermap <- renderLeaflet(cluster_map())

      # hide the map card if no map_data()
      observe(toggleElement("map_card", condition = !is.null(nrow(map_data()))))

      # render the cluster computation psa
      output$cluster_psa <- renderText(
        cluster_computation_psa()
      )

      # Add the map summary/title to the map
      output$map_summary <- renderText(
        toupper(dc$synd_summary)
      ) |> bindEvent(map_data())


    }
  )
}
