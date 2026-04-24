# Development of this software was sponsored by the U.S. Government under
# contract no. 75D30124C19958

label_list_ots <- list(
  region_selector = list(
    l = "Select Region(s)",
    m = "Click or type the name(s) of the regions you would like to plot using the dropdown."
  ),
  split_panels = list(
    l = "Separate feature panels",
    m = "Toggle on to create a separate subplot for each selected feature within each region. Toggle off to overlay selected features within each region."
  ),
  free_y_axis = list(
    l = "Free y-axis",
    m = "Toggle on for independent y-axes across plots. Toggle off for a common/shared axis."
  )
)

viz_other_time_series_ui <- function(id) {
  ns <- NS(id)
  
  nav_panel(
    title = "Other Time Series Plots",
    layout_sidebar(
      sidebar = sidebar(
        id = ns("other_time_series_sidebar"),
        width = SIDEBAR_WIDTH * 2,
        feature_sidepanel_ui(ns("feature_side"), title = "Feature Filters", allow_multiple = TRUE),
        tags$hr(),
        selectizeInput(
          ns("viz_regions"),
          labeltt(label_list_ots[["region_selector"]]),
          choices = NULL,
          multiple = TRUE
        ),
        input_switch(
          id = ns("split_feature_panels"),
          label = labeltt(label_list_ots[["split_panels"]]),
          value = FALSE
        ),
        input_switch(
          id = ns("free_ts_y_axis"),
          label = labeltt(label_list_ots[["free_y_axis"]]),
          value = TRUE
        )
      ),
      card(
        plotlyOutput(ns("other_ts_plots"))
      )
    )
  )
}

viz_other_time_series_server <- function(id, im, feature_store) {
  moduleServer(id, function(input, output, session) {
    # Resolve the shared feature-store object passed in from viz_server().
    get_store <- function() {
      if (is.function(feature_store)) feature_store() else feature_store
    }
    
    # This tab only reads from the stored shared data table.
    get_base_source <- reactive({
      req(im$data_cls)
      im$data_cls$data
    })
    
    # Region choices come from the same stored data that supplies the plot.
    region_choices <- reactive({
      req(im$data_cls)
      get_time_series_region_choices(im$data_cls, display_col = "region")
    })
    
    # Keep the region selector synchronized with the current stored data.
    observe({
      req(im$data_cls)
      choices <- region_choices()
      updateSelectizeInput(
        session = session,
        inputId = "viz_regions",
        choices = choices,
        selected = choices[1]
      )
    })
    
    plottable_features_df <- reactive({
      # A feature is plottable on this tab only if its stored output columns
      # already exist and are numeric in the shared data table.
      store <- get_store()
      base_source <- get_base_source()
      req(store, base_source)
      
      df <- as.data.frame(store$features_df())
      if (!nrow(df)) return(df)
      
      plot_dt <- data.table::as.data.table(base_source)
      keep <- vapply(df$id, function(fid) {
        f <- store$get_feature(fid)
        if (is.null(f)) return(FALSE)
        
        out_cols <- f$out_cols %||% character(0)
        if (!length(out_cols)) return(FALSE)
        if (!all(out_cols %in% names(plot_dt))) return(FALSE)
        
        if (identical(f$feature_type, "confidence_interval")) {
          if (length(out_cols) < 2) return(FALSE)
          return(all(vapply(out_cols[1:2], function(col) is.numeric(plot_dt[[col]]), logical(1))))
        }
        
        if (length(out_cols) != 1) return(FALSE)
        is.numeric(plot_dt[[out_cols[[1]]]])
      }, logical(1))
      
      df[keep, , drop = FALSE]
    })
    
    # Pass a filtered metadata view into the shared feature selector so users
    # only see features that can actually be plotted on this tab.
    filtered_feature_store <- list(
      features_df = reactive(plottable_features_df())
    )
    
    feature_filters <- feature_sidepanel_server(
      "feature_side",
      feature_store = filtered_feature_store,
      allow_multiple = TRUE
    )
    
    # The feature selector returns keys that may include row suffixes; strip
    # those off so downstream logic works with plain feature IDs.
    selected_feature_ids <- reactive({
      keys <- feature_filters()$selected_features %||% character(0)
      ids <- sub("::.*$", "", keys)
      unique(ids[nzchar(ids)])
    })
    
    # Mark each row as historical or forecast so the plotting helper can style
    # the two segments differently and bridge them visually.
    mark_time_series_type <- function(dt, region_col, date_col, future_steps = 0L) {
      data.table::setDT(dt)
      dt <- data.table::copy(dt)
      if (!inherits(dt[[date_col]], "Date")) dt[, (date_col) := as.Date(get(date_col))]
      if (region_col %in% names(dt)) dt[, (region_col) := as.character(get(region_col))]
      
      if ("type" %in% names(dt)) return(dt[])
      
      data.table::setorderv(dt, c(region_col, date_col))
      dt[, i := seq_len(.N), by = region_col]
      dt[, type := data.table::fifelse(i > .N - future_steps, "Forecast", "Historical"), by = region_col]
      dt[, i := NULL]
      
      if (future_steps > 0) {
        bridge <- dt[type == "Historical"][, .SD[which.max(get(date_col))], by = region_col]
        if (nrow(bridge) > 0) {
          bridge[, type := "Forecast"]
          dt <- data.table::rbindlist(list(dt, bridge), use.names = TRUE, fill = TRUE)
        }
      }

      dt[, type_order := data.table::fifelse(type == "Historical", 1L, 2L)]
      data.table::setorderv(dt, c(region_col, date_col, "type_order"))
      dt[, type_order := NULL]
      
      dt[]
    }
    
    feature_plot_data <- reactive({
      # Scalar features are plotted as lines, while stored confidence
      # intervals are reshaped into ribbons using their lower/upper columns.
      req(im$data_cls)
      store <- get_store()
      req(store)
      
      selected_ids <- selected_feature_ids()
      validate(need(length(selected_ids) > 0, "Select at least one feature to plot"))
      
      dcls <- im$data_cls
      base_source <- get_base_source()
      req(base_source)
      
      reg_col <- dcls$region_column
      date_col <- dcls$date_column
      region_label_col <- if ("region" %in% names(base_source)) "region" else reg_col
      # Pull only the columns needed for the selected features plus the time
      # and region identifiers used to group panels.
      feature_cols <- unique(unlist(lapply(selected_ids, function(fid) {
        f <- store$get_feature(fid)
        if (is.null(f)) return(character(0))
        f$out_cols %||% character(0)
      })))
      
      plot_keep <- unique(c(
        reg_col,
        date_col,
        region_label_col,
        feature_cols
      ))
      plot_keep <- intersect(plot_keep, names(base_source))
      
      plot_dt_base <- data.table::as.data.table(base_source)[, ..plot_keep]
      plot_dt_base <- mark_time_series_type(plot_dt_base, reg_col, date_col, future_steps = im$nforecasts %||% 0L)
      
      # Reshape each selected feature into a standard plotting structure so
      # line and ribbon features can be combined downstream.
      series_list <- lapply(selected_ids, function(fid) {
        f <- store$get_feature(fid)
        if (is.null(f)) return(NULL)
        
        ft <- f$feature_type %||% "other"
        out_cols <- f$out_cols %||% character(0)
        work_dt <- data.table::copy(plot_dt_base)
        
        if (ft == "confidence_interval" && length(out_cols) >= 2 && all(out_cols[1:2] %in% names(work_dt))) {
          out <- work_dt[, .(
            region_id = as.character(get(reg_col)),
            region_label = as.character(get(region_label_col)),
            date = as.Date(get(date_col)),
            type = as.character(type),
            feature_id = fid,
            feature_label = f$label %||% fid,
            value = NA_real_,
            lower = as.numeric(get(out_cols[[1]])),
            upper = as.numeric(get(out_cols[[2]]))
          )]
          out[, hover_text := paste0(
            "Date: ", format(date, "%Y-%m-%d"), "<br>",
            feature_label, "<br>",
            "Interval: [", round(lower, 4), ", ", round(upper, 4), "]"
          )]
          return(out[])
        }
        
        src_col <- out_cols[[1]]
        if (length(src_col) != 1 || !src_col %in% names(work_dt) || !is.numeric(work_dt[[src_col]])) return(NULL)
        
        out <- work_dt[, .(
          region_id = as.character(get(reg_col)),
          region_label = as.character(get(region_label_col)),
          date = as.Date(get(date_col)),
          type = as.character(type),
          feature_id = fid,
          feature_label = f$label %||% fid,
          value = as.numeric(get(src_col)),
          lower = NA_real_,
          upper = NA_real_
        )]
        out[, hover_text := paste0(
          "Date: ", format(date, "%Y-%m-%d"), "<br>",
          feature_label, ": ", round(value, 4)
        )]
        out[]
      })
      
      # Combine all selected features into one long table for the shared
      # plot-building helper.
      plot_dt <- data.table::rbindlist(Filter(Negate(is.null), series_list), use.names = TRUE, fill = TRUE)
      validate(need(nrow(plot_dt) > 0, "No plottable numeric data available for the selected features"))
      data.table::setDT(plot_dt)
      plot_dt[, type_order := data.table::fifelse(type == "Historical", 1L, 2L)]
      data.table::setorderv(plot_dt, c("region_id", "feature_label", "date", "type_order"))
      plot_dt[, type_order := NULL]
      plot_dt[]
    })
    
    # Filter the long plotting table to the requested regions and hand off to
    # the shared plotly assembly helper.
    output$other_ts_plots <- renderPlotly({
      req(im$data_cls)
      validate(need(length(input$viz_regions %||% character(0)) > 0, "Must have at least one region selected"))
      plot_dt <- feature_plot_data()
      plot_dt <- plot_dt[region_id %in% as.character(input$viz_regions %||% character(0))]
      validate(need(nrow(plot_dt) > 0, "No plot data available for the selected region/feature combination"))
      
      build_other_time_series_plotly(
        plot_dt = as.data.frame(plot_dt),
        region_ids = as.character(input$viz_regions %||% character(0)),
        separate_features = isTRUE(input$split_feature_panels),
        fixed_y = !isTRUE(input$free_ts_y_axis)
      )
    })
  })
}
