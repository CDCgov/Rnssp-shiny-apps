# © 2024 The Johns Hopkins University Applied Physics Laboratory LLC
# Development of this software was sponsored by the U.S. Government under contracts no. 75D30120C07643, 75D30122C15442

#-------------------------------------------------
# Rnssp-shiny-apps: alerts_of_alerts App
#
# Authors:
#   Joshua Kimrey
#   Michael Sheppard
#   Raghav Ramachandran
#   Howard Burkom
#   Roseric Azondekon
#-------------------------------------------------

mainPanelModuleOutput <- function(id) {
  ns <- NS(id)
  
  mainPanel(
    fluidRow(
      box(
        title = div(
          h3("Alerts of Alerts Time Series",
             style = "font-size:18px; font-weight: bold; display: inline-block;"
          ),
          helpPopup(
            id = "",
            word = "Methods",
            title = "Alerts of Alerts Time Series",
            content = paste0(
              "This plot depicts time series for each of three state-wide temporal diagnostics of syndrome burden: 1) ",
              "total statewide percent of ED visits, 2) number of alerting ",
              "counties/regions, determined via the use of the the <a href=",
              "'https://github.com/CDCgov/Rnssp' target=",
              "'_blank'>Rnssp</a> Switch (Adaptive ",
              "Regression/EWMA) alert detection algorithm, and 3) the number of counties/regions ",
              "estimated--through the use of Generative Additive Modeling--to have increasing case counts. ",
              "The two leading subplots subsequently apply the <a href=",
              "'https://github.com/CDCgov/Rnssp' target=",
              "'_blank'>Rnssp</a> Switch (Adaptive ",
              "Regression/EWMA) alert detection algorithm to test ",
              "for Alerts in these aggregated series. ",
              "The third subplot re-applies the Generalized Additive Model estimation method to estimate whether ",
              "the number of regions estimated to have increasing case counts is itself increasing. Clicking on a ",
              "particular date (indicated by a vertical dashed line) plots the spatial data and statistical tests associated with ",
              "that date."
            ),
            placement = "right", trigger = "click",
            icon_name = "question-circle",
            icon_style = "color:blue;font-size:10px"
          )
        ),
        withSpinner(
          plotlyOutput(outputId = ns("tsPlotly"), height = "400px")
        ),
        width = 12
      ),
      box(
        title = div(
          h3("Spatial Distribution of p-values",
             style = "font-size:18px; font-weight: bold; display: inline-block; margin-right: 10px;"
          ),
          helpPopup(
            id = "",
            word = "Methods",
            title = "Spatial Distribution of p-values",
            content = paste0(
              "This map shows the spatial ",
              "distribution of p-values corresponding to each ",
              "respective region’s test for temporal alerts along ",
              "with the results of the <a href=",
              "'https://www.rdocumentation.org/packages/spdep/versions/1.2-8/topics/moran.test' ",
              "target='_blank'>Global Moran's I</a> statistical ",
              "test for spatial autocorrelation (i.e., ",
              "clustering) of these p-values. Moran’s I operates ",
              "on continuous data; small p-values for the Global ",
              "Moran's I test indicate that both regions with ",
              "low temporal p-values and regions with high ",
              "temporal p-values are spatially clustered, ",
              "respectively."
            ),
            placement = "bottom", trigger = "click",
            icon_name = "question-circle",
            icon_style = "color:blue;font-size:10px"
          )
        ),
        withSpinner(leafletOutput(ns("p_choropleth"), height = "200px")),
        box(
          h3(textOutput(ns("date1")),
             style = "font-size:14px; font-weight: bold;"
          ),
          h1(textOutput(ns("globalmoran")), style = "font-size:14px;")
        ),
        width = 4
      ),
      box(
        title = div(
          h3("Spatial Distribution of Alerts",
             style = "font-size:18px; font-weight: bold; height: 30px; color: rgb(22, 96, 167); display: inline-block; margin-right: 10px;"
          ),
          helpPopup(
            id = "",
            word = "Methods",
            title = "Spatial Distribution of Alerts",
            content = paste0(
              "This map shows the spatial ",
              "distribution of Alerts computed via the Switch ",
              "(Adaptive Regression/EWMA) algorithm along with ",
              "the results of two <a href=",
              "'https://www.rdocumentation.org/packages/spdep/versions/1.2-8/topics/joincount.test' ",
              "target='_blank'>Join Count</a> statistical tests ",
              "for global spatial clustering. The first test ",
              "tests whether Alerts (red) are spatially ",
              "clustered, while the second test tests whether ",
              "both Alerts and Warnings (red and yellow), treated ",
              "as a single grouping, are spatially clustered."
            ),
            placement = "bottom", trigger = "click",
            icon_name = "question-circle",
            icon_style = "color:blue;font-size:10px"
          )
        ),
        withSpinner(leafletOutput(ns("alerts_choropleth"), height = "200px")),
        box(
          h3(textOutput(ns("date2")),
             style = "font-size:14px; font-weight: bold;"
          ),
          h1(textOutput(ns("joincount_alert")),
             style = "font-size:14px;"
          ),
          h1(textOutput(ns("joincount_warning_or_alert")),
             style = "font-size:14px;"
          )
        ),
        width = 4
      ),
      box(
        title = div(
          h3("Spatial Distribution of Trends",
             style = "font-size:18px; font-weight: bold; height: 30px; color: rgb(60, 0, 155); display: inline-block; margin-right: 10px;"
          ),
          helpPopup(
            id = "",
            word = "Methods",
            title = "Spatial Distribution of Trends",
            content = paste0(
              "This map shows the spatial distribution of trends in case counts ",
              "estimated via <a href='https://www.rdocumentation.org/packages/mgcv/versions/1.9-1/topics/gam' ",
              "target='_blank'>Generalized Additive Models</a> (GAMs). ",
              "Only those regions for which more than 20% of the selected dates contain non-zero ",
              "case counts are retained for GAM estimation analysis. ",
              "The removed regions are denoted as 'Sparse'. ",
              "Each series is treated as a binomial process. The regions which are estimated to have an 'Increasing' slope are subjected to a ",
              "<a href='https://www.rdocumentation.org/packages/spdep/versions/1.2-8/topics/joincount.test' ",
              "target='_blank'>Join Count</a> statistical test for global spatial clustering. ",
              "The test tests whether regions labeled 'Increasing' (red) are spatially clustered."
            ),
            placement = "bottom", trigger = "click",
            icon_name = "question-circle",
            icon_style = "color:blue;font-size:10px"
          )
        ),
        withSpinner(leafletOutput(ns("increasing_choropleth"), height = "200px")),
        box(
          h3(textOutput(ns("date3")), style = "font-size:14px; font-weight: bold;"),
          h1(textOutput(ns("joincount_inc")), style = "font-size:14px;")
        ),
        width = 4
      )
    )
  )
}

mainPanelModule <- function(input, output, session, sideBarInput) {
  ns <- session$ns
  
  # annotations <- reactive({
  #   get_annotations
  # })
  
  
  plotly_plot <- reactive({
    color_scale <- c("NA" = "gray", "None" = "blue", "Warning" = "yellow", "Alert" = "red")
    
    # Adding annotations as sub-legends
    topy <- 0.9
    middley <- 0.55
    bottomy <- 0.125
    
    Reactive_dfs <- sideBarInput()$Reactive_dfs
    
    percent_plot <- plot_ly(
      data = Reactive_dfs$df_1, source = "plotlyts", x = ~date, y = ~percent, type = "scatter", mode = "lines+markers",
      marker = list(
        color = ~alert_percent,
        size = 25 / log(dim(Reactive_dfs$df_1)[1])
      ),
      line = list(color = "gray", width = 12 / log(dim(Reactive_dfs$df_1)[1])),
      hoverinfo = "text",
      text = ~ paste(
        "<br>Date:</b>", date,
        "<br>%:</b>", format(percent, big.mark = ",")
      ),
      name = "% CCDD", legendgroup = "1st"
    )
    
    alert_plot <- plot_ly(
      data = Reactive_dfs$df_1, source = "plotlyts", x = ~date, y = ~count, type = "scatter", mode = "lines+markers",
      marker = list(color = ~alert_alert, size = 25 / log(dim(Reactive_dfs$df_1)[1])),
      line = list(color = "rgb(22, 96, 167)", width = 12 / log(dim(Reactive_dfs$df_1)[1])),
      hoverinfo = "text",
      text = ~ paste(
        "<br>Date:</b>", date,
        "<br>Count:</b>", format(count, big.mark = ",")
      ),
      name = "Count of Alerting Regions", legendgroup = "2nd"
    )
    
    trending_plot <- plot_ly(data = Reactive_dfs$df_1, source = "plotlyts", x = ~date) %>%
      add_trace(
        y = ~fitted, type = "scatter", mode = "lines",
        line = list(color = "rgb(60, 0, 155)", width = 12 / log(dim(Reactive_dfs$df_1)[1])),
        hoverinfo = "text",
        text = ~ paste(
          "<br>Date:</b>", date,
          "<br>fitted:</b>", format(fitted, big.mark = ",")
        ),
        name = "GAM-estimated trend", legendgroup = "3rd"
      ) %>%
      add_trace(
        y = ~n_increasing, type = "scatter", mode = "markers",
        marker = list(color = "grey", size = 25 / log(dim(Reactive_dfs$df_1)[1]), opacity = 0.5),
        hoverinfo = "text",
        text = ~ paste(
          "<br>Date:</b>", date,
          "<br># Increasing:</b>", format(n_increasing, big.mark = ",")
        ),
        name = "Count of Regions with Increasing Trend", legendgroup = "3rd"
      )
    
    color_mapping <- setNames(c(1, 2, 3, 4), c("red", "yellow", "blue", "lightgray"))
    Reactive_dfs$df_1$alert_numeric <- color_mapping[Reactive_dfs$df_1$alert_trend]
    
    # Create a matrix for hover text, matching the z matrix (1 row)
    hover_text_matrix <- matrix(
      paste("Date:", Reactive_dfs$df_1$date, "<br>Status:", Reactive_dfs$df_1$trajectory),
      nrow = 1
    )
    
    # Create a one-row heatmap
    trend_heatmap <- plot_ly(
      data = Reactive_dfs$df_1,
      source = "plotlyts",
      x = ~date,
      z = matrix(Reactive_dfs$df_1$alert_numeric, nrow = 1),
      type = "heatmap",
      text = hover_text_matrix,
      hoverinfo = "text",
      xgap = 0.05,
      ygap = 0.5,
      showscale = FALSE,
      colorscale = list(
        list(0, "red"),
        list(0.33, "yellow"),
        list(0.66, "blue"),
        list(1.0, "lightgray")
      ),
      zmin = 1,
      zmax = 4
    ) %>%
      layout(
        plot_bgcolor = "black",
        xaxis = list(title = "Date", tickfont = list(size = 10)),
        yaxis = list(title = "", showticklabels = FALSE, ticks = ""),
        margin = list(r = 50, b = 50, l = 50, pad = 4),
        hovermode = "x"
      ) %>%
      hide_colorbar()
    
    # subplots object
    plt <- subplot(percent_plot, alert_plot, trending_plot, trend_heatmap,
                   nrows = 4,
                   which_layout = 1, shareX = TRUE, heights = c(0.32, 0.32, 0.32, 0.04)
    ) %>%
      layout(
        title = list(text = paste0(sideBarInput()$selected$state, ": ", sideBarInput()$selected$CCDD), x = 0.4),
        hovermode = "x unified",
        xaxis = list(
          title = "<b>Date<b>",
          showspikes = TRUE,
          spikemode = "across",
          ticks = "outside",
          spikedash = "dot",
          spikecolor = "black",
          spikethickness = -2,
          tickformat = "%Y-%m-%d",
          ticklabelmode = "period",
          tickangle = 0
        ),
        yaxis = list(
          title = list(
            text = paste0("<b>%</b>"),
            font = list(
              color = "grey"
            )
          ),
          tickfont = list(color = "grey"), # 0s are offset
          showline = TRUE,
          showgrid = TRUE,
          rangemode = "tozero",
          ticks = "outside",
          showgrid = FALSE
        ),
        yaxis2 = list(
          title = list(
            text = "<b>Alerts<b>",
            font = list(
              color = "rgb(22, 96, 167)"
            )
          ),
          tickfont = list(color = "rgb(22, 96, 167)"),
          showline = TRUE,
          showgrid = TRUE,
          rangemode = "tozero",
          ticks = "outside"
        ),
        yaxis3 = list(
          title = list(
            text = "<b>Increasing<br>Trend<b>",
            font = list(
              color = "rgb(60, 0, 155)"
            )
          ),
          tickfont = list(color = "rgb(60, 0, 155)"),
          showline = TRUE,
          showgrid = TRUE,
          rangemode = "tozero",
          ticks = "outside" # ,
          # domain = c(0.04, 0.36)
        ),
        yaxis4 = list(
          domain = c(0.0, 0.04), # Set the heatmap to start right below the scatter plot
          showticklabels = FALSE
        ),
        shapes = list(
          type = "line",
          x0 = sideBarInput()$selected$maps_date,
          x1 = sideBarInput()$selected$maps_date,
          y0 = 0,
          y1 = 1,
          yref = "paper",
          line = list(color = "black", dash = "dash", width = 2)
        ),
        annotations = get_annotations(topy, middley, bottomy),
        legend = list(tracegroupgap = 91.5)
      ) %>%
      event_register(., "plotly_click")
    
    plt %>%
      config(modeBarButtons = list(list("toImage"), list("autoScale2d")))
  })
  
  # leaflet reactive functions
  leaflet_p_choropleth <- reactive({
    labels_p <- sprintf(
      "<strong>County: </strong>%s<br/>
        <strong>P-value: </strong>%s<br/>
        <strong>Percent: </strong>%s<br/>",
      sideBarInput()$selected_state$df_sf$NAME,
      sideBarInput()$selected_state$df_sf$p,
      sideBarInput()$selected_state$df_sf$percent
    ) %>%
      lapply(htmltools::HTML)
    
    pal_p <- colorNumeric(
      palette = colorRampPalette(c("red", "yellow", "blue"), bias = log(0.1) / log(0.5))(100), # length(df_sf$p)
      domain = c(0, 1),
      na.color = "black",
      alpha = TRUE
    )
    
    p_leaf <- leaflet() %>%
      leaflet.extras::setMapWidgetStyle(list(background = "#FFFFFF")) %>%
      leaflet.extras::addFullscreenControl() %>% # Add fullscreen control
      leaflet.extras::addResetMapButton() %>% # Add reset button
      addPolylines(
        data = sideBarInput()$selected_state$state_sf,
        opacity = 1,
        fillOpacity = 0.5,
        color = "black",
        weight = 2.0
      ) %>%
      addPolygons(
        data = sideBarInput()$selected_state$df_sf,
        layerId = ~NAME,
        stroke = TRUE,
        smoothFactor = 0.5,
        color = "black",
        fillColor = ~ pal_p(p),
        weight = 1.0,
        opacity = 1.0,
        fillOpacity = 0.75,
        highlight = highlightOptions(
          weight = 1,
          color = "black",
          fillOpacity = 0.7,
          opacity = 1.0
        ),
        label = labels_p,
        labelOptions = labelOptions(
          style = list(
            "font-weight" = "normal",
            padding = "3px 8px"
          ),
          textsize = "12px",
          direction = "auto"
        ),
        group = "counties"
      ) %>%
      addLegend(
        position = "bottomright",
        colors = "black", # Custom color for NA values
        labels = htmltools::HTML('<span style="font-size:12px;">No data</span>'),
        opacity = 0.75
      ) %>%
      addLegendNumeric(
        orientation = "horizontal",
        height = 20,
        width = 120,
        bins = 11,
        position = "bottomright",
        title = "P-value",
        pal = pal_p,
        values = seq(from = 0, to = 1, length.out = 100),
        fillOpacity = 0.75
      )
  })
  
  leaflet_alerts_choropleth <- reactive({
    # leaflet alerts plot
    labels_alerts <- sprintf(
      "<strong>County: </strong>%s<br/>
         <strong>Alert: </strong>%s<br/>
         <strong>p-value: </strong>%s<br/>",
      sideBarInput()$selected_state$df_sf$NAME,
      sideBarInput()$selected_state$df_sf$alert_percent,
      sideBarInput()$selected_state$df_sf$p
    ) %>%
      lapply(htmltools::HTML)
    
    pal_alerts <- colorFactor(
      palette = c("blue", "yellow", "red", "black"),
      levels = c("None", "Warning", "Alert", "No data available")
    ) # ,
    # na.color = "black")
    
    alert_leaf <- leaflet() %>%
      leaflet.extras::setMapWidgetStyle(list(background = "#FFFFFF")) %>%
      leaflet.extras::addFullscreenControl() %>% # Add fullscreen control
      leaflet.extras::addResetMapButton() %>% # Add reset button
      addPolylines(
        data = sideBarInput()$selected_state$state_sf,
        opacity = 1,
        fillOpacity = 0.5,
        color = "black",
        weight = 2.0
      ) %>%
      addPolygons(
        data = sideBarInput()$selected_state$df_sf,
        layerId = ~NAME,
        stroke = TRUE,
        smoothFactor = 0.5,
        color = "black",
        fillColor = ~ pal_alerts(ifelse(is.na(alert_percent), "No data available", as.character(alert_percent))),
        weight = 1.0,
        opacity = 1.0,
        fillOpacity = 0.75,
        highlight = highlightOptions(
          weight = 1,
          color = "black",
          fillOpacity = 1.0,
          opacity = 1.0
        ),
        label = labels_alerts,
        labelOptions = labelOptions(
          style = list(
            "font-weight" = "normal",
            padding = "3px 8px"
          ),
          textsize = "12px",
          direction = "auto"
        )
      ) %>%
      addLegendFactor(
        position = "bottomright",
        height = 8,
        width = 10,
        title = "Alerts",
        labelStyle = "font-size: 12px;",
        pal = pal_alerts,
        values = factor(c("None", "Warning", "Alert", "No data available"), levels = c("None", "Warning", "Alert", "No data available")),
        opacity = 1,
        fillOpacity = 0.75
      )
  })
  
  leaflet_increasing_choropleth <- reactive({
    labels_inc <- sprintf(
      "<strong>County: </strong>%s<br/>
        <strong>Slope: </strong>%s<br/>",
      sideBarInput()$selected_state$df_sf$NAME,
      round(sideBarInput()$selected_state$df_sf$deriv, digits = 3)
    ) %>%
      lapply(htmltools::HTML)
    
    pal_inc <- colorFactor(
      palette = c("blue", "yellow", "red", "lightgray", "black"),
      levels = c("Decreasing", "Stable", "Increasing", "Sparse", "No data available")
    )
    
    inc_leaf <- leaflet() %>%
      leaflet.extras::setMapWidgetStyle(list(background = "#FFFFFF")) %>%
      leaflet.extras::addFullscreenControl() %>% # Add fullscreen control
      leaflet.extras::addResetMapButton() %>% # Add reset button
      addPolylines(
        data = sideBarInput()$selected_state$state_sf,
        opacity = 1,
        fillOpacity = 0.5,
        color = "black",
        weight = 2.0
      ) %>%
      addPolygons(
        data = sideBarInput()$selected_state$df_sf,
        layerId = ~NAME,
        stroke = TRUE,
        smoothFactor = 0.5,
        color = "black",
        fillColor = ~ pal_inc(ifelse(is.na(trajectory), "No data available", as.character(trajectory))),
        weight = 1.0,
        opacity = 1.0,
        fillOpacity = 0.75,
        highlight = highlightOptions(
          weight = 1,
          color = "black",
          fillOpacity = 0.7,
          opacity = 1.0
        ),
        label = labels_inc,
        labelOptions = labelOptions(
          style = list(
            "font-weight" = "normal",
            padding = "3px 8px"
          ),
          textsize = "12px",
          direction = "auto"
        )
      ) %>%
      addLegendFactor(
        position = "bottomright",
        height = 8,
        width = 10,
        title = "Increasing",
        labelStyle = "font-size: 12px;",
        pal = pal_inc,
        values = factor(c("Decreasing", "Stable", "Increasing", "Sparse", "No data available"),
                        levels = c("Decreasing", "Stable", "Increasing", "Sparse", "No data available")
        ),
        fillOpacity = 0.75,
        opacity = 1
      )
  })
  
  # Alerts time series Plotly time series
  output$tsPlotly <- renderPlotly({
    if (!is.null(sideBarInput()$Reactive_dfs$df_1)) {
      plotly_plot()
    }
  })
  
  # P-value choropleth map
  output$p_choropleth <- renderLeaflet({
    if (!is.null(sideBarInput()$selected_state$df_sf)) {
      leaflet_p_choropleth()
    }
  })
  
  # Alerts choropleth map
  output$alerts_choropleth <- renderLeaflet({
    if (!is.null(sideBarInput()$selected_state$df_sf)) {
      leaflet_alerts_choropleth()
    }
  })
  
  # Smoothed slope bins choropleth map
  output$increasing_choropleth <- renderLeaflet({
    if (!is.null(sideBarInput()$selected_state$df_sf)) {
      leaflet_increasing_choropleth()
    }
  })
  
  # Update map dates and stats
  output$date1 <- renderText(sideBarInput()$selected$maps_date)
  output$date2 <- renderText(sideBarInput()$selected$maps_date)
  output$date3 <- renderText(sideBarInput()$selected$maps_date)
  
  output$globalmoran <- renderText({
    sideBarInput()$stat_test$globalMoran %>% 
      paste0("Global Moran's I p-value: ", .)
  })
  output$joincount_alert <- renderText({
    sideBarInput()$stat_test$JoinCount_alert %>% 
      paste0("Join Count 'Alert' p-value: ", .)
  })
  output$joincount_warning_or_alert <- renderText({
    sideBarInput()$stat_test$JoinCount_warning_or_alert %>% 
      paste0("Join Count 'Alert' or 'Warning' p-value: ", .)
  })
  output$joincount_inc <- renderText({
    sideBarInput()$stat_test$JoinCount_increasing %>% 
      paste0("Join Count 'Increasing' p-value: ", .)
  })
  
  #------------------ADD COUNTY-WISE TIME SERIES POPUP MODAL-----------------#
  
  # Reactive element to plot time series from p_choropleth on click
  observeEvent(input$p_choropleth_shape_click, {
    p_choropleth_click <- input$p_choropleth_shape_click
    selected_county <- p_choropleth_click$id
    
    if (sideBarInput()$selected$state == "All") {
      state <- substr(selected_county, nchar(selected_county) - 2 + 1, nchar(selected_county))
      selected_county <- substr(selected_county, 1, nchar(selected_county) - 4)
    }
    
    time_series_data <- st_as_sf(
      right_join(
        sideBarInput()$Reactive_dfs$df_2,
        sideBarInput()$selected_state$county_sf[, c("NAME", "GEOID")],
        by = c("fips" = "GEOID")
      )
    ) %>%
      select(state_abbr, fips, NAME, date, percent, color) %>%
      as.data.frame() %>%
      mutate(NAME = as.character(NAME))
    
    df_temp <- time_series_data[time_series_data$NAME == selected_county, ]
    if (sideBarInput()$selected$state == "All") {
      df_temp <- df_temp[df_temp$state_abbr == state, ]
    }
    
    if (dim(df_temp)[1] > 1) {
      showModal(modalDialog(
        withSpinner(plotlyOutput(ns("CountyTimeSeriesPlot"))),
        size = "l"
      ))
    } else {
      showModal(modalDialog(
        paste0("There is no data available for this county"),
        size = "s"
      ))
    }
    
      output$CountyTimeSeriesPlot <- renderPlotly({
        df_temp_before <- df_temp %>%
          filter(date <= sideBarInput()$selected$maps_date)
        
        df_temp_after <- df_temp %>%
          filter(date >= sideBarInput()$selected$maps_date)
        
        plot_ly() %>%
          add_trace(
            data = df_temp_before, x = ~date, y = ~percent, type = "scatter", mode = "lines+markers",
            marker = list(color = ~color),
            hoverinfo = "text",
            name = paste0("Before ", sideBarInput()$selected$maps_date),
            text = ~ paste(
              "<br>Date:</b>", date,
              "<br>%:</b>", format(percent, big.mark = ",")
            )
          ) %>%
          add_trace(
            data = df_temp_after, x = ~date, y = ~percent, type = "scatter", mode = "lines+markers",
            line = list(color = "gray", dash = "dot"),
            marker = list(color = ~color),
            hoverinfo = "text",
            name = paste0("After ", sideBarInput()$selected$maps_date),
            text = ~ paste(
              "<br>Date:</b>", date,
              "<br>%:</b>", format(percent, big.mark = ",")
            )
          ) %>%
          layout(
            shapes = list(
              type = "line",
              x0 = sideBarInput()$selected$maps_date,
              x1 = sideBarInput()$selected$maps_date,
              y0 = min(df_temp$percent),
              y1 = max(df_temp$percent),
              line = list(color = "black", dash = "dash", width = 2)
            ),
            title = paste0("<b>", selected_county, ": ", sideBarInput()$selected$CCDD, "<b>"),
            yaxis = list(title = paste0("<b>Percent (%)<b>")),
            xaxis = list(title = paste0("<b>Date<b>"))
          ) %>%
          config(modeBarButtons = list(list("toImage"), list("autoScale2d")))
      })
  })
  
  observeEvent(input$alerts_choropleth_shape_click, {
    alerts_choropleth_click <- input$alerts_choropleth_shape_click
    selected_county <- alerts_choropleth_click$id
    if (sideBarInput()$selected$state == "All") {
      state <- substr(selected_county, nchar(selected_county) - 2 + 1, nchar(selected_county))
      selected_county <- substr(selected_county, 1, nchar(selected_county) - 4)
    }
    
    time_series_data <- st_as_sf(
      right_join(
        sideBarInput()$Reactive_dfs$df_2,
        sideBarInput()$selected_state$county_sf[, c("NAME", "GEOID")],
        by = c("fips" = "GEOID")
      )
    ) %>%
      select(state_abbr, fips, NAME, date, percent, color) %>%
      as.data.frame() %>%
      mutate(NAME = as.character(NAME))
    
    df_temp <- time_series_data[time_series_data$NAME == selected_county, ]
    if (sideBarInput()$selected$state == "All") {
      df_temp <- df_temp[df_temp$state_abbr == state, ]
    }
    
    if (dim(df_temp)[1] > 1) {
      showModal(modalDialog(
        withSpinner(plotlyOutput(ns("CountyTimeSeriesPlot"))),
        size = "l"
      ))
    } else {
      showModal(modalDialog(
        paste0("There is no data available for this county"),
        size = "s"
      ))
    }
    
    output$CountyTimeSeriesPlot <- renderPlotly({
      df_temp_before <- df_temp %>%
        filter(date <= sideBarInput()$selected$maps_date)
      
      df_temp_after <- df_temp %>%
        filter(date >= sideBarInput()$selected$maps_date)
      
      plot_ly() %>%
        add_trace(
          data = df_temp_before, x = ~date, y = ~percent, type = "scatter", mode = "lines+markers",
          marker = list(color = ~color),
          hoverinfo = "text",
          name = paste0("Before ", sideBarInput()$selected$maps_date),
          text = ~ paste(
            "<br>Date:</b>", date,
            "<br>%:</b>", format(percent, big.mark = ",")
          )
        ) %>%
        add_trace(
          data = df_temp_after, x = ~date, y = ~percent, type = "scatter", mode = "lines+markers",
          line = list(color = "gray", dash = "dot"),
          marker = list(color = ~color),
          hoverinfo = "text",
          name = paste0("After ", sideBarInput()$selected$maps_date),
          text = ~ paste(
            "<br>Date:</b>", date,
            "<br>%:</b>", format(percent, big.mark = ",")
          )
        ) %>%
        layout(
          shapes = list(
            type = "line",
            x0 = sideBarInput()$selected$maps_date,
            x1 = sideBarInput()$selected$maps_date,
            y0 = min(df_temp$percent),
            y1 = max(df_temp$percent),
            line = list(color = "black", dash = "dash", width = 2)
          ),
          title = paste0("<b>", selected_county, ": ", sideBarInput()$selected$CCDD, "<b>"),
          yaxis = list(title = paste0("<b>Percent (%)<b>")),
          xaxis = list(title = paste0("<b>Date<b>"))
        ) %>%
        config(modeBarButtons = list(list("toImage"), list("autoScale2d")))
    })
  })
  
  
  # Reactive element to plot time series from increasing_choropleth on click
  observeEvent(input$increasing_choropleth_shape_click, {
    increasing_choropleth_click <- input$increasing_choropleth_shape_click
    selected_county <- increasing_choropleth_click$id
    if (sideBarInput()$selected$state == "All") {
      state <- substr(selected_county, nchar(selected_county) - 2 + 1, nchar(selected_county))
      selected_county <- substr(selected_county, 1, nchar(selected_county) - 4)
    }
    
    time_series_data <- st_as_sf(
      right_join(
        sideBarInput()$Reactive_dfs$df_2,
        sideBarInput()$selected_state$county_sf[, c("NAME", "GEOID")],
        by = c("fips" = "GEOID")
      )
    ) %>%
      select(state_abbr, fips, NAME, date, percent, fitted, deriv, color) %>%
      as.data.frame() %>%
      mutate(NAME = as.character(NAME))
    
    df_temp <- time_series_data[time_series_data$NAME == selected_county, ]
    if (sideBarInput()$selected$state == "All") {
      df_temp <- df_temp[df_temp$state_abbr == state, ]
    }
    
    if (dim(df_temp)[1] > 1) {
      showModal(modalDialog(
        withSpinner(plotlyOutput(ns("CountyTimeSeriesPlot"))),
        size = "l"
      ))
    } else {
      showModal(modalDialog(
        paste0("There is no data available for this county"),
        size = "s"
      ))
    }
    
    output$CountyTimeSeriesPlot <- renderPlotly({
      df_temp_before <- df_temp %>%
        filter(date <= sideBarInput()$selected$maps_date)
      
      df_temp_after <- df_temp %>%
        filter(date >= sideBarInput()$selected$maps_date)
      
      df_gam <- subset(df_temp_before, df_temp_before$date <= as.Date(sideBarInput()$selected$maps_date))
      
      plot_ly() %>%
        add_trace(
          data = df_temp_before, x = ~date, y = ~percent, type = "scatter", mode = "lines+markers",
          marker = list(color = ~color),
          hoverinfo = "text",
          name = paste0("Before ", sideBarInput()$selected$maps_date),
          text = ~ paste(
            "<br>Date:</b>", date,
            "<br>%:</b>", format(percent, big.mark = ",")
          )
        ) %>%
        add_trace(
          data = df_temp_after, x = ~date, y = ~percent, type = "scatter", mode = "lines+markers",
          line = list(color = "gray", dash = "dot"),
          marker = list(color = ~color),
          hoverinfo = "text",
          name = paste0("After ", sideBarInput()$selected$maps_date),
          text = ~ paste(
            "<br>Date:</b>", date,
            "<br>%:</b>", format(percent, big.mark = ",")
          )
        ) %>%
        add_trace(
          data = df_gam, x = ~date, y = ~fitted, type = "scatter", mode = "lines",
          line = list(color = "rgb(60, 0, 155)", width = 5),
          # marker = list(color = ~color),
          hoverinfo = "text",
          name = paste0("GAM-based trend estimate "),
          text = ~ paste(
            "<br>Date:</b>", date,
            "<br>%:</b>", format(fitted, big.mark = ","),
            "<br>slope:</b>", format(deriv, big.mark = ",")
          )
        ) %>%
        layout(
          shapes = list(
            type = "line",
            x0 = sideBarInput()$selected$maps_date,
            x1 = sideBarInput()$selected$maps_date,
            y0 = min(df_temp$percent),
            y1 = max(df_temp$percent),
            line = list(color = "black", dash = "dash", width = 2)
          ),
          title = paste0("<b>", selected_county, ": ", sideBarInput()$selected$CCDD, "<b>"),
          yaxis = list(title = paste0("<b>Percent (%)<b>")),
          xaxis = list(title = paste0("<b>Date<b>"))
        ) %>%
        config(modeBarButtons = list(list("toImage"), list("autoScale2d")))
    })
    
    #-------------------SYNCHRONIZE MAPS VIEW AND ZOOM---------------------------#
    # Initialize flags to keep track of user action for each map
    user_action <- reactiveValues(p_choropleth = FALSE, alerts_choropleth = FALSE, increasing_choropleth = FALSE)
    
    # Define names of all choropleth maps
    choropleth_maps <- c("p_choropleth", "alerts_choropleth", "increasing_choropleth")
    
    # Observe changes in bounds for each map
    lapply(choropleth_maps, function(map_name) {
      observeEvent(input[[paste0(map_name, "_bounds")]], {
        if (!user_action[[map_name]]) { # User action
          
          # Update all other maps
          other_maps <- setdiff(choropleth_maps, map_name)
          lapply(other_maps, function(other_map) {
            user_action[[other_map]] <<- TRUE
            leafletProxy(other_map, session) %>%
              fitBounds(
                input[[paste0(map_name, "_bounds")]]$west,
                input[[paste0(map_name, "_bounds")]]$south,
                input[[paste0(map_name, "_bounds")]]$east,
                input[[paste0(map_name, "_bounds")]]$north
              )
          })
        } else { # Program action
          user_action[[map_name]] <<- FALSE
        }
      })
    })
  })
  
  list(
    plotly_object = plotly_plot,
    leaflet_object_p = leaflet_p_choropleth,
    leaflet_object_alerts = leaflet_alerts_choropleth,
    leaflet_object_increasing = leaflet_increasing_choropleth
  )
}
