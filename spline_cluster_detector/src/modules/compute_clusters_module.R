# © 2025 The Johns Hopkins University Applied Physics Laboratory LLC
# Development of this software was sponsored by the U.S. Government under
# contract no. 75D30124C19958

get_cluster_table_sketch <- function(res=c("zip", "county")) {
  res = match.arg(res)
  
  if(res == "zip") {
    t_areas = c("Counties Affected", "Counties overlapping with these zip codes")
  } else {
    t_areas = c("Zip Codes Affected", "Zip Codes overlapping with these Counties")
  }
  cluster_table_sketch = htmltools::withTags(table(
    class = 'display',
    thead(
      tr(
        th('Center', title = 'The center of the cluster'),
        th('Cluster Date', title = 'The date of this cluster'),
        th('Duration', title = 'Duration (days)'),
        th('Observed', title = 'Number observed in the cluster'),
        th('Expected', title = 'Number expected in the cluster'),
        th('Obs/Exp', title = 'Observed/Expected'),
        th('p-value', title = "Approximate p-value"),
        th('Max Distance From Center', title = 'Distance (in miles) from the cluster center to the center of the furthers location included in the cluster'),
        th('Number of Locations', title = 'Number of locations in this cluster'),
        th('Cluster Locations', title = 'Locations included in this cluster'),
        th(t_areas[1], title = t_areas[2])
      )
    )
  ))
}

compute_clusters_ui <- function(id) {
  
  
  ns <- NS(id)
  
  nav_panel(
    title = "Cluster Summary",
    hidden(card(
      id = ns("cluster_card"),
      withSpinner(
        uiOutput(outputId = ns("clusterdata"))
        # ,caption="Computing clusters ... can take some time"
      ),
      card_footer(
        downloadButton(ns("cluster_display_download_btn"),
        "Download Clusters",
        class = "btn-primary btn-sm"
      ))
    )),
    hidden(htmlOutput(outputId = ns("cluster_psa")))
  )
}

compute_clusters_server <- function(id, results, dc, cc, trigger, parent_session) {
  moduleServer(
    id,
    function(input, output, session) {
      
      observe(results$cluster_data <- cluster_data())
      observe(results$cluster_table_display <- cluster_table_display()$display)
      observe(results$cluster_data_extended <- cluster_table_display()$cd)
      # observe({
      #   req(cluster_table_display())
      #   if(is.list(cluster_table_display())) {
      #     results$cluster_table_display <- cluster_table_display()$display  
      #   } else {
      #     results$cluster_table_display <- cluster_table_display()
      #   }
      # })
      # 
      # observe({
      #   req(cluster_table_display())
      #   if(is.list(cluster_table_display())) {
      #     results$cluster_data_extended <- cluster_table_display()$cd
      #   }
      #   
      # })
      
      ns = session$ns
      
      # ---------------------------------------------
      #   Compute Clusters
      # ---------------------------------------------
      
      cluster_data <- reactiveVal()
      
      observe({
        if(trigger()) {
          req(results$filtered_records_count)
          toggle_task_button_color(parent_session$ns("clusters_btn"), busy=TRUE)
          cluster_data(NULL)
          
          clusters <- tryCatch(
            find_clusters(
              cases = results$filtered_records_count,
              distance_matrix = cc$distance_matrix,
              detect_date = cc$end_date,
              spline_lookup = cc$spline_lookup,
              baseline_length = as.numeric(cc$baseline_length),
              max_test_window_days = as.numeric(cc$test_length),
              guard_band =0,
              distance_limit = as.numeric(cc$radius),
              min_clust_cases = as.numeric(cc$minimum_cluster_count),
              max_clust_cases = as.numeric(cc$maximum_cluster_count),
              baseline_adjustment = cc$base_adj_meth,
              use_fast = TRUE
            ),
            error = function(e) "No Clusters Found"
          )
          
          trigger(FALSE)
          cluster_data(clusters)
          toggle_task_button_color(parent_session$ns("clusters_btn"), busy=FALSE)
        }

      }) |> bindEvent(trigger())

      cluster_table_display <- reactive({
        
        req(cluster_data())
        req(dc$res)
        
        if(!is.data.frame(cluster_data()[[1]])) {
          return(list(
          display = HTML(cluster_data()), cd = NULL
          ))
        }
        else {
          
          cd <- cluster_data()[[1]][
            ,
            .(
              `Cluster Date` = date,
              Duration = detect_date-date+1,
              Observed = observed,
              Expected = expected,
              `Obs/Exp` = exp(log_obs_exp),
              `p-value` = interpolate_p_value(INTERP_FUNCTIONS, observed, log_obs_exp),
              `Max Distance From Center` = distance_value,
              `Number of Locations` = nr_locs
            ),
            by = .(Center = target)
          ]
          
          # add a list of locations as one of the columns
          # first, get the locations
          
          locs = data.table::copy(cluster_data()[[2]])
          
          # get the locations intersection (i.e. zips if counties, counties if zips)
          intersected_locs = find_intersects(locs, ZFI, res=dc$res)
          
          # second, get a display name
          if(dc$res == "county") {
            locs[, display_location:=gen_display_name_from_fips(location)]
          } else {
            locs[, display_location:=location]
          }
          
          # third, convert to a list
          locs = locs[,
                      .(`Cluster Locations` = toString(display_location))
                      ,target
          ]
          
          # fourth, merge on cd
          cd = merge(cd, locs, by.x="Center", by.y = "target", all.x=TRUE)
          
          # fifth, merge in the intersects
          cd = merge(cd, intersected_locs, by.x = "Center", by.y = "target", all.x=TRUE)
          
          # add a display name to replace Center if this is county
          if(dc$res == "county") {
            cd[, Center:=gen_display_name_from_fips(Center)]
          }
          
          dom_value = fifelse(nrow(cd)<10, 't','tp')
          
  
          cd[, names(.SD):=lapply(.SD, \(s) round(s,2)), .SDcols = is.numeric]
          cd[, intersected_locs:=paste0(
            "<details><summary>Click</summary>",
            intersected_locs,
            "</details>"
            )]
          
          display <- datatable(
            cd,
            container=get_cluster_table_sketch(dc$res),
            rownames=FALSE,
            plugins = "ellipsis",
            escape = FALSE,
            selection = list(mode = 'none'),
            options = list(
              dom = dom_value,
              columnDefs = list(list(
                targets = "Cluster Locations",
                render = JS("$.fn.dataTable.render.ellipsis(25, false )")
              ))
              
            )
          )
          
          return(list("display" = display, "cd" = cd))
        }
        
      })
      

      # Show the identified clusters
      output$clusterdata <- renderUI({
        results$cluster_table_display
      })

      # hide the cluster card if no clusters
      observe({
        toggleElement("cluster_card", condition=!is.null(results$cluster_data))
        toggleElement("cluster_psa", condition=!is.null(results$cluster_data) && is.data.frame(cluster_data()[[1]]))
      })
      

      output$cluster_psa <- renderText(
        cluster_computation_psa()
      )
      
      output$cluster_display_download_btn <- downloadHandler(
        filename = "clusters_found.csv",
        content = function(file) {
          data.table::fwrite(
            results$cluster_data_extended,
            file
          )
        }
      )

    }
  )
}
