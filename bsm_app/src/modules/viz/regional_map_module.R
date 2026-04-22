# © 2025 The Johns Hopkins University Applied Physics Laboratory LLC
# Development of this software was sponsored by the U.S. Government under
# contract no. 75D30124C19958


label_list_rm <- list(
  date_sparkline = list(
    l = "Date Selected:",
    m = paste(
      "The red vertical line can be used to select the date to display on the map.",
      "The sparkline summarizes the selected feature across all regions at each date.",
      sep = " "
    )
  ),
  advanced_settings = list(
    l = "Advanced map settings",
    m = "Optional controls for how values are colored and where the legend appears."
  ),
  shared_colorbar = list(
    l = "Use shared colorbar range across all dates",
    m = "Keep the same color scale across dates so map colors can be compared over time. Turn this off to rescale the colors separately for the selected date."
  ),
  legend_location = list(
    l = "Legend location",
    m = "Choose where the map legend is placed."
  ),
  color_palette = list(
    l = "Color palette",
    m = "Choose the color palette used to shade regions on the map."
  )
)

viz_regional_map_ui <- function(id) {
  ns <- NS(id)
  
  spark_card <- card(
    style = "width: 100%; height:100px;",
    class = "mb-1",
    card_body(
      style = "height:100%; padding: 0;",
      plotlyOutput(ns("date_spark"), height = "100%", width = "100%")
    )
  )
  
  nav_panel(
    title = "Region-Wide Map",
    layout_sidebar(
      leafletOutput(ns("region_map")),
      sidebar = sidebar(
        id = ns("region_map_sidebar"),
        width = SIDEBAR_WIDTH * 2,
        div(
          style = "font-weight: 600; font-size: 0.95rem; margin-bottom: 6px; display:flex; gap:8px; align-items:baseline;",
          div(labeltt(label_list_rm[["date_sparkline"]])),
          htmlOutput(ns("target_date_lbl"))
        ),
        spark_card,
        tags$hr(),
        feature_sidepanel_ui(ns("feature_side"), title = "Feature Filters", allow_multiple = FALSE),
        tags$hr(),
        tags$details(
          tags$summary(labeltt(label_list_rm[["advanced_settings"]])),
          checkboxInput(
            inputId = ns("map_use_global_range"),
            label   = labeltt(label_list_rm[["shared_colorbar"]]),
            value   = TRUE
          ),
          selectInput(
            ns("map_legend_position"),
            label = labeltt(label_list_rm[["legend_location"]]),
            choices = c(
              "Bottom right" = "bottomright",
              "Bottom left"  = "bottomleft",
              "Top right"    = "topright",
              "Top left"     = "topleft"
            ),
            selected = "bottomright"
          ),
          selectInput(
            inputId = ns("map_palette"),
            label   = labeltt(label_list_rm[["color_palette"]]),
            choices = c(
              "Viridis" = "viridis",
              "Plasma" = "plasma",
              "Magma" = "magma",
              "Inferno" = "inferno",
              "Cividis" = "cividis",
              "Greys" = "Greys",
              "YlOrRd" = "YlOrRd",
              "RdBu" = "RdBu"
            ),
            selected = "viridis"
          ),
          tags$br()
        ),
        position = "left"
      )
    )
  )
}

viz_regional_map_server <- function(id, dc, im, results, feature_store) {
  moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns
      
      get_store <- function() {
        if (is.function(feature_store)) feature_store() else feature_store
      }
      
      get_base_source <- reactive({
        req(im$data_cls)
        im$data_cls$data
      })
      
      # The map can display any stored single-column numeric feature except
      # composite intervals, which do not map cleanly to one choropleth value.
      plottable_features_df <- reactive({
        store <- get_store()
        base_source <- get_base_source()
        req(store, base_source)
        
        df <- as.data.frame(store$features_df())
        if (!nrow(df)) return(df)
        
        plot_dt <- data.table::as.data.table(base_source)
        keep <- vapply(df$id, function(fid) {
          f <- store$get_feature(fid)
          if (is.null(f)) return(FALSE)
          if (identical(f$feature_type %||% "", "confidence_interval")) return(FALSE)
          
          out_cols <- f$out_cols %||% character(0)
          if (length(out_cols) != 1) return(FALSE)
          if (!out_cols[[1]] %in% names(plot_dt)) return(FALSE)
          
          is.numeric(plot_dt[[out_cols[[1]]]])
        }, logical(1))
        
        df[keep, , drop = FALSE]
      })
      
      filtered_feature_store <- list(
        features_df = reactive(plottable_features_df())
      )
      
      # Reuse the shared feature selector UI so the map tab stays aligned
      # with the app-wide feature catalog and filter semantics.
      feature_filters <- feature_sidepanel_server(
        "feature_side",
        feature_store = filtered_feature_store,
        allow_multiple = FALSE,
        initial_selected_id = "builtin__mean__proportion"
      )
      
      selected_feature_id <- reactive({
        keys <- feature_filters()$selected_features %||% character(0)
        if (!length(keys)) return("")
        key <- keys[[1]]
        sub("::.*$", "", key)
      })
      
      selected_feature <- reactive({
        store <- get_store()
        if (is.null(store)) return(NULL)
        fid <- selected_feature_id()
        if (!nzchar(fid)) return(NULL)
        feature <- store$get_feature(fid)
        if (is.null(feature)) return(NULL)
        feature
      })
      
      # Reset the sparkline cursor when the mapped feature changes so the date
      # selector redraws from the new feature's stored time series.
      observe({
        if (!nzchar(selected_feature_id())) return()
        cursor_date(NULL)
      }) |> bindEvent(selected_feature_id(), ignoreInit = TRUE)
      
      all_dates <- reactive({
        req(im$data_cls)
        date_col <- im$data_cls$date_column
        req(!is.null(date_col), date_col %in% names(im$data_cls$data))
        as.Date(sort(unique(im$data_cls$data[[date_col]])))
      })
      
      last_observed_date <- reactive({
        d <- all_dates()
        req(length(d) > 0)
        k <- im$nforecasts %||% 0
        idx <- max(1, length(d) - k)
        as.Date(d[idx])
      })
      
      cursor_date <- reactiveVal(NULL)
      
      target_date <- reactive({
        d <- all_dates()
        req(length(d) > 0)
        
        cd <- cursor_date()
        if (is.null(cd)) return(last_observed_date())
        
        d[which.min(abs(d - cd))]
      })
      
      output$target_date_lbl <- renderUI({
        req(target_date())
        tags$span(sprintf("(%s)", format(target_date(), "%Y-%m-%d")))
      })
      
      observe({
        r <- plotly::event_data("plotly_relayout", source = "date_spark_src", priority = "event")
        x0 <- r[["shapes[0].x0"]]
        x1 <- r[["shapes[0].x1"]]
        if (is.null(x0) && is.null(x1)) return()
        
        t0 <- as.POSIXct(x0, tz = "UTC")
        t1 <- as.POSIXct(x1, tz = "UTC")
        x_new <- as.Date(as.POSIXct(mean(as.numeric(c(t0, t1)), na.rm = TRUE), origin = "1970-01-01", tz = "UTC"))
        
        d <- all_dates()
        x_new <- min(max(x_new, min(d)), max(d))
        x_snap <- d[which.min(abs(d - x_new))]
        
        if (identical(cursor_date(), x_snap)) return()
        cursor_date(x_snap)
        
        plotly::plotlyProxy("date_spark", session) |>
          plotly::plotlyProxyInvoke(
            "relayout",
            list(shapes = list(vertical_date_line(x_snap)))
          )
      }) |> bindEvent(input$date_spark_relayout, ignoreInit = TRUE)
      
      spark_series <- reactive({
        req(im$data_cls)
        feat <- selected_feature()
        req(!is.null(feat))
        get_map_feature_sparkline_data(
          feature_data = get_base_source(),
          data_cls = im$data_cls,
          feature = feat,
          future_steps = im$nforecasts %||% 0L
        )
      })
      
      output$date_spark <- renderPlotly({
        validate(need(!is.null(im$data_cls), "Load data and run model first"))
        validate(need(nzchar(selected_feature_id()), "Select a feature to display"))
        feat <- selected_feature()
        validate(need(!is.null(feat), "Select a feature to display"))
        
        # The sparkline is a pure view over the stored feature values and
        # drives the selected map date through the draggable vertical line.
        p <- build_map_feature_sparkline(
          series = as.data.frame(spark_series()),
          init_date = isolate(cursor_date() %||% last_observed_date()),
          yaxis_title = feat$label %||% "Value"
        )
        
        htmlwidgets::onRender(
          p,
          sprintf(
            "function(el, x) {
              el.on('plotly_relayout', function(e) {
                if (window.Shiny) Shiny.setInputValue('%s', e, {priority: 'event'});
              });
            }",
            ns("date_spark_relayout")
          )
        )
      })
      
      map_base_locations <- reactive({
        req(im$data_cls)
        get_map_locations(im$data_cls)
      })
      
      output$region_map <- renderLeaflet({
        validate(need(!is.null(im$data_cls), "Load data and run model first"))
        validate(need(nzchar(selected_feature_id()), "Select a feature to display"))
        feat <- selected_feature()
        validate(need(!is.null(feat), "Select a feature to display"))
        
        # The map tab only renders stored columns; any calculated feature
        # values must already have been saved during model fit or Add Feature.
        build_regional_feature_map(
          map_locations = map_base_locations(),
          feature_data = get_base_source(),
          data_cls = im$data_cls,
          feature = feat,
          target_date = target_date(),
          includes_alaska_hawaii = dc$includes_alaska_hawaii,
          use_global_range = isTRUE(input$map_use_global_range),
          palette = input$map_palette %||% "viridis",
          legend_position = input$map_legend_position %||% "bottomright"
        ) |>
          enable_draggable_legend()
      })
      
      observe({
        click <- input$region_map_shape_click
        if (is.null(click$id)) return()
        feat <- selected_feature()
        if (is.null(feat)) return()
        
        # Polygon clicks reuse the same stored feature to show the selected
        # region's time path without recomputing posterior summaries.
        plot_data <- get_map_feature_popup_data(
          feature_data = get_base_source(),
          data_cls = im$data_cls,
          feature = feat,
          region_id = click$id,
          future_steps = im$nforecasts %||% 0L
        )
        
        p <- build_map_feature_popup_plot(
          ts = plot_data$ts,
          region_label = plot_data$region_label,
          feature_label = plot_data$feature_label,
          v_date = target_date()
        )
        
        popup_html <- leafpop::popupGraph(list(p), type = "png", width = 300, height = 200)
        
        leafletProxy("region_map") |>
          clearPopups() |>
          addPopups(
            lng = click$lng,
            lat = click$lat,
            popup = popup_html,
            layerId = "timeseries_popup"
          )
      }) |> bindEvent(input$region_map_shape_click)
    }
  )
}
