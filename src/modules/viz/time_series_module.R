# © 2025 The Johns Hopkins University Applied Physics Laboratory LLC
# Development of this software was sponsored by the U.S. Government under
# contract no. 75D30124C19958

label_list_ts <- list(
  scale_radio = list(
    l = "Scale",
    m = "Select the desired scale for the displayed results. Select count to display the number of visits and proportion to display the proportion of visits with the requested diagnosis out of all visits. Note that forecasts are only available when the scale matches the natural scale of the model, i.e. count for poisson and negative binomial and proportion for binomial and beta binomial."
  ),
  ci_select =  list(
    l = "Credible Interval",
    m = "Select from the available credible interval features for the current scale."
  ),
  region_selector =  list(
    l = "Select Region(s)",
    m = "Click or type the name(s) of the regions you would like to plot using the dropdown."
  ),
  free_ts_y_axis = list(
    l = "Free y-axis",
    m = 'Toggle on for independent y-axes across plots (recommended when scale is set to "count"); Toggle off for a common/shared axis (recommended when scale is set to "proportion").'
  )
)

viz_time_series_ui <- function(id) {
  ns <- NS(id)
  
  nav_panel(
    title = "Prediction Time Series Plots",
    layout_sidebar(
      sidebar = sidebar(
        id = ns("time_series_sidebar"),
        width = SIDEBAR_WIDTH*2,
        radioButtons(ns("ts_use_count"),
                     labeltt(label_list_ts[["scale_radio"]]),
                     choices = c("Count", "Proportion"),selected = "Proportion",
                     inline=TRUE),
        uiOutput(ns("ts_ci_ui")),
        selectizeInput(ns("viz_regions"), 
                       labeltt(label_list_ts[["region_selector"]]),
                       choices=NULL, 
                       multiple=TRUE)
      ),
      card(
        plotlyOutput(ns("ts_plots")),
        card_footer(
          input_switch(
            id = ns("free_ts_y_axis"),
            label = labeltt(label_list_ts[["free_ts_y_axis"]]),
            value=TRUE
          )
        )
      )
    ),
  )
  
}

viz_time_series_server <- function(id, im, results, feature_store) {
  moduleServer(
    id,
    function(input, output, session) {
      get_store <- function() {
        if (is.function(feature_store)) feature_store() else feature_store
      }
      
      scale_key <- reactive({
        if (identical(input$ts_use_count, "Count")) "counts" else "proportion"
      })
      
      # Prediction Time Series only offers CIs that already exist in the
      # stored calculated-feature catalog for the chosen scale.
      available_ci_features <- reactive({
        store <- get_store()
        req(store)
        df <- as.data.frame(store$features_df())
        req(nrow(df) > 0)
        
        df <- df[
          df$feature_type == "confidence_interval" &
            df$feature_scale == scale_key(),
          ,
          drop = FALSE
        ]
        req(nrow(df) > 0)
        
        ci_vals <- vapply(df$id, function(fid) {
          f <- store$get_feature(fid)
          as.numeric(f$params$ci %||% NA_real_)
        }, numeric(1))
        
        df$ci_val <- ci_vals
        df <- df[order(-df$ci_val, df$label, df$id), , drop = FALSE]
        df
      })
      
      selected_ci_feature <- reactive({
        store <- get_store()
        req(store)
        fid <- input$ts_ci_feature
        req(!is.null(fid), nzchar(fid))
        f <- store$get_feature(fid)
        req(!is.null(f), identical(f$feature_type, "confidence_interval"))
        f
      })
      
      median_feature <- reactive({
        store <- get_store()
        req(store)
        fid <- paste0("builtin__quantile__", scale_key(), "__q0.5")
        f <- store$get_feature(fid)
        req(!is.null(f), identical(f$feature_type, "quantile"))
        f
      })
      
      ts_spec <- reactive({
        ci_feature <- selected_ci_feature()
        build_time_series_plot_spec(
          region_ids = input$viz_regions,
          ci = as.numeric(ci_feature$params$ci %||% 0.95) * 100,
          fixed_y = !isTRUE(input$free_ts_y_axis),
          display_col = "region",
          use_count = identical(input$ts_use_count, "Count"),
          future_steps = im$nforecasts
        )
      })
      
      observe({
        ci_df <- available_ci_features()
        selected_id <- isolate(input$ts_ci_feature)
        
        default_id <- ci_df$id[which.min(abs(ci_df$ci_val - 0.95))]
        if (!length(default_id) || is.na(default_id)) default_id <- ci_df$id[[1]]
        if (!length(selected_id) || !selected_id %in% ci_df$id) selected_id <- default_id
        
        output$ts_ci_ui <- renderUI({
          selectInput(
            inputId = session$ns("ts_ci_feature"),
            label = labeltt(label_list_ts[["ci_select"]]),
            choices = stats::setNames(ci_df$id, ci_df$label),
            selected = selected_id
          )
        })
      }) |> bindEvent(scale_key(), available_ci_features())
      
      # get the region choices
      input_region_choices <- reactive({
        req(im$data_cls)
        get_time_series_region_choices(im$data_cls, display_col = "region")
      })
      
      # Update the region choices based on the data
      observe({
        req(im$data_cls)
        choices <- input_region_choices()
        updateSelectizeInput(
          session = session,
          inputId = "viz_regions",
          choices = choices,
          selected = choices[1]
        )
      })
      
      # Prediction plots now read the stored median/CI columns rather than
      # recomputing posterior summaries inside the viz tab.
      tspd <- reactive({
        req(im$data_cls)
        prepare_time_series_feature_plot_data(
          feature_data = im$data_cls$data,
          data_cls = im$data_cls,
          spec = ts_spec(),
          ci_feature = selected_ci_feature(),
          median_feature = median_feature()
        )
      })
      
      ts_plots <- reactive({
        req(tspd())
        build_time_series_plotly(
          ts_plot_data = tspd(),
          spec = ts_spec()
        )
      })
      
      output$ts_plots <- renderPlotly({
        validate(need(!is.null(im$data_cls), "Load data and run model first"))
        validate(need(input$viz_regions, "Must have a least one region selected"))
        ts_plots()
      })
      
    }
    
  )
}
