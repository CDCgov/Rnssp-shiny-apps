# © 2022 The Johns Hopkins University Applied Physics Laboratory LLC
# Development of this software was sponsored by the U.S. Government under contracts no. 75D30120C07643, 75D30122C15442

#----------------------------------------------------
# Alerts of Alerts App
# Authors:
#   Joshua Kimrey
#   Michael Sheppard
#   Raghav Ramachandran
#   Howard Burkom
#

# load libraries
suppressPackageStartupMessages({
  packages <- c(
    "shiny", "shinyjs", "dplyr", "Rnssp", "purrr",
    "data.table", "lubridate", "shinycssloaders",
    "plotly", "shinyWidgets", "sf", "shinythemes",
    "janitor", "tidyverse", "leaflet", "leaflegend",
    "spdep", "shinydashboard"
  )
})

if (length(setdiff(packages, rownames(installed.packages()))) > 0) {
  install.packages(setdiff(packages, rownames(installed.packages())))
}
lapply(packages, library, character.only = TRUE)

if (length(setdiff("Rnssp", rownames(installed.packages()))) > 0) {
  devtools::install_github("cdcgov/Rnssp", upgrade = "never")
}
lapply("Rnssp", library, character.only = TRUE)

# Help Popup
helpPopup <- function(
    id,
    title,
    content,
    placement = c("right", "top", "left", "bottom"),
    trigger = c("click", "hover", "focus", "manual"),
    icon_name = "question-circle",
    icon_style = "color:red") {
  
  tagList(
    singleton(
      tags$head(
        tags$script(
          HTML("
            $(function() {
              $('[data-toggle=\"popover\"]').popover({ html : true, sanitize: false });
            });")
        )
      )
    ),
    HTML(id),
    tags$a(
      href = "#",
      style = "margin-left:10px;",
      `data-toggle` = "popover",
      title = title,
      `data-content` = content,
      `data-animation` = TRUE,
      `data-placement` = match.arg(placement, several.ok = TRUE)[1],
      `data-trigger` = match.arg(trigger, several.ok = TRUE)[1],
      shiny::icon(name = icon_name, class = "shinyhelper-icon", style = icon_style)
    )
  )
}

load_profile <- rstudioapi::showQuestion(
  "Alerts of Alerts App",
  "NSSP-ESSENCE Credentials are required to use this app!",
  "Load a profile File",
  "Supply User Credentials"
)

myProfile <- NULL
prof_file <- NULL

if (load_profile) {
  filtres <- matrix(c(
    "R images (*.RData,*.rda)", "Binary R files (*.rds)",
    "*.RData;*.rda", "*.rds"
  ), 2, 2)
  if (interactive() && .Platform$OS.type == "windows") {
    prof_file <- choose.files(filters = filtres)
  } else if (interactive() && .Platform$OS.type == "unix") {
    prof_file <- file.choose()
  } else if (!interactive()) {
    prof_file <- readline("Enter full path to the profile file: ")
  }
  if (!any(endsWith(prof_file, c(".rda", ".rds")))) {
    cli::cli_alert_danger("Failed to load. File provided must be either an {.field .rda} or {.field .rds} file")
  }
  
  if (all(endsWith(tolower(prof_file), ".rda"))) {
    myProfile <- get(load(prof_file))
  } else {
    myProfile <- prof_file %>%
      readRDS() %>%
      try(silent = TRUE)
  }
  if (all(class(myProfile) == "try-error")) {
    cli::cli_alert_danger("No or corrupt file loaded!")
    myProfile <- create_profile() %>%
      try(silent = TRUE)
    if (all(class(myProfile) == "try-error")) {
      cli::cli_abort("App stopped. No credentials provided!")
    }
  }
} else {
  myProfile <- create_profile() %>%
    try(silent = TRUE)
  if (all(class(myProfile) == "try-error")) {
    cli::cli_abort("App stopped. No credentials provided!")
  }
}

# Read in States

state_helper <- state_sf %>%
  as.data.frame() %>%
  select(
    state_name = NAME, 
    state_abbr = STUSPS,
    state_fips = STATEFP
  )

states <- as.character(state_helper$state_abbr)

# Read in CCDD Categories

ccdd_cats <- "https://essence.syndromicsurveillance.org/nssp_essence/api/datasources/va_hosp/fields/ccddCategory" %>%
  get_api_data(profile = myProfile) %>%
  pluck("values") %>%
  pull("value") %>%
  try(silent = TRUE)

if (any(class(ccdd_cats) == "try-error")) {
  cli::cli_abort("App failed to establish connection with NSSP-ESSENCE servers!
                 Check your credentials and try again")
}

# Set start date and end date

StartDate_0 <- Sys.Date() %m-%
  months(3)

EndDate_0 <- Sys.Date() %m-%
  days(1)

# User interface object

ui <- tagList(
  useShinyjs(),
  tags$head(
    HTML(
      "<script>
          var socket_timeout_interval
          var n = 0
          $(document).on('shiny:connected', function(event) {
          socket_timeout_interval = setInterval(function(){
          Shiny.onInputChange('count', n++)
          }, 15000)
          });
          $(document).on('shiny:disconnected', function(event) {
          clearInterval(socket_timeout_interval)
          });
          </script>
      "
    )
  ),
  theme = shinytheme("cosmo"),
  navbarPage(
    title = "Alerts of Alerts",
    theme = shinytheme("cosmo"),
    id = "nav",
    tabPanel(
      "Selectable Options",
      sidebarLayout(
        sidebarPanel(
          selectInput("State", "State", states, 'FL'),
          selectInput("CCDD", "CCDD", ccdd_cats, 
                      ccdd_cats[which(grepl("COVID-Specific", ccdd_cats))]),
          fluidRow(
            column(
              6,
              dateInput(inputId = "StartDate", label = "Start Date", 
                        value = StartDate_0)
            ),
            column(
              6,
              dateInput(inputId = "EndDate", label = "End Date", 
                        value = EndDate_0)
            )
          ),
          actionButton("go", "Load Data")
        ),
        mainPanel(
          helpPopup(
            id = "", title = "",
            content = paste0(
              "This app computes and test for temporal alerts in 3 statewide ",
              "diagnostics of syndrome severity given a user-selected state, ",
              "CC and DD category, and date range. The 3 diagnostics are: 1) ",
              "total statewide percent of ED visits, 2) number of alerting ",
              "counties/regions, and 3) the number of counties/regions ",
              "estimated to have increasing case counts. The second two ",
              "diagnostics have been labelled 'Alerts of Alerts'. Data ",
              "associated to each date can be visualized as a set of ",
              "choropleth maps along with the results of statistical tests ",
              "for spatial autocorrelation (i.e., spatial clustering)."
            ),
            placement = "right", trigger = "focus",
            icon_name = "question-circle",
            icon_style = "color:blue;font-size:15px"
          ),
          fluidRow(
            box(helpPopup(id = "", title = "",
                          content = paste0("This plot uses the <a href=",
                                           "'https://github.com/CDCgov/Rnssp' target=",
                                           "'_blank'>Rnssp</a> Switch (Adaptive ",
                                           "Regression/EWMA) alert detection algorithm to test ",
                                           "for Alerts in both the first and second subplots. ",
                                           "The third subplot uses an estimated slope ",
                                           "threshold to signal Alerts. Clicking on a ",
                                           "particular date will plot the data associated with ",
                                           "that date as a family of choropleth maps and will ",
                                           "run a battery of tests for spatial autocorrelation ",
                                           "in the plotted quantity."
                          ),
                          placement = "right", trigger = "focus",
                          icon_name = "question-circle",
                          icon_style = "color:blue;font-size:10px"),
                title = h3("Alerts of Alerts Time Series", 
                           style = 'font-size:18px; font-weight: bold;'),
                withSpinner(plotlyOutput(outputId = "tsPlotly", 
                                         height = "280px")),
                width = 12),
            box(helpPopup(id = "", title = "",
                          content = paste0("This map shows the spatial ",
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
                                           "respectively."),
                          placement = "bottom", trigger = "focus",
                          icon_name = "question-circle",
                          icon_style = "color:blue;font-size:10px"),
                title = h3("Spatial Distribution of p-values", 
                           style = 'font-size:18px; font-weight: bold; height: 30px'),
                leafletOutput("p_choropleth", height = "300px"),
                box(h3(textOutput('date1'), 
                       style = 'font-size:14px; font-weight: bold;'),
                    h1(textOutput('globalmoran'), style = 'font-size:14px;')),
                width = 4),
            box(helpPopup(id = "", title = "",
                          content = paste0("This map shows the spatial ",
                                           "distribution of Alerts computed via the Switch ",
                                           "(Adaptive Regression/EWMA) algorithm along with ",
                                           "the results of two <a href=",
                                           "'https://www.rdocumentation.org/packages/spdep/versions/1.2-8/topics/joincount.test' ",
                                           "target='_blank'>Join Count</a> statistical tests ",
                                           "for global spatial clustering. The first test ",
                                           "tests whether Alerts (red) are spatially ",
                                           "clustered, while the second test tests whether ",
                                           "both Alerts and Warnings (red and yellow), treated ",
                                           "as a single grouping, are spatially clustered."),
                          placement = "bottom", trigger = "focus",
                          icon_name = "question-circle",
                          icon_style = "color:blue;font-size:10px"),
                title = h3("Spatial Distribution of Alerts", 
                           style = 'font-size:18px; font-weight: bold; height: 30px'),
                leafletOutput("alerts_choropleth", height = "300px"),
                box(h3(textOutput('date2'), 
                       style = 'font-size:14px; font-weight: bold;'),
                    h1(textOutput('joincount_alert'), 
                       style = 'font-size:14px;'),
                    h1(textOutput('joincount_warning_or_alert'), 
                       style = 'font-size:14px;')),
                width = 4),
            box(helpPopup(id = "", title = "",
                          content = paste0("This map shows the spatial 
                          distribution of Alerts computed via slope ",
                                           "thresholds along with the results of a single ",
                                           "<a href=",
                                           "'https://www.rdocumentation.org/packages/spdep/versions/1.2-8/topics/joincount.test' ",
                                           "target='_blank'>Join Count</a> statistical test ",
                                           "for global spatial clustering. The test tests ",
                                           "whether Alerts (red) are spatially ",
                                           "clustered."),
                          placement = "bottom", trigger = "focus",
                          icon_name = "question-circle",
                          icon_style = "color:blue;font-size:10px"),
                title = h3("Spatial Distribution of Smoothed Slopes", style = 'font-size:18px; font-weight: bold; height: 30px'),
                leafletOutput("increasing_choropleth", height = "300px"),
                box(h3(textOutput('date3'), style = 'font-size:14px; font-weight: bold;'),
                    h1(textOutput('joincount_inc'), style = 'font-size:14px;')),
                width = 4)
          )
        )
      )
    )
  )
)

# Server object; event handler

server <- function(input, output, session) {
  options(warn = -1)
  
  # To avoid RStudio timeouts -- server code
  output$keepAlive <- renderText({
    req(input$count)
    paste("keep alive ", input$count)
  })
  
  Reactive_dfs <- reactiveValues(df_1 = NULL, df_2 = NULL, state_sf = NULL)
  selected_state <- reactiveValues(state = NULL)
  selected_date <- reactiveValues(date = NULL)
  
  spline_classifier <- function(df, t, y) {
    
    t <- deparse(substitute(t))
    y <- deparse(substitute(y))
    
    .ss <- smooth.spline(x = as.numeric(df[[t]]), y = df[[y]], spar = 0.5)
    .fitted <- predict(.ss, as.numeric(df[[t]]))$y
    .deriv1 <- predict(.ss, as.numeric(df[[t]]), deriv = 1)$y
    .deriv2 <- predict(.ss, as.numeric(df[[t]]), deriv = 2)$y
    
    bind_cols(df, fitted = .fitted, deriv1 = .deriv1, deriv2 = .deriv2)
    
  }
  
  get_and_mutate_dfs <- function(input) {
    
    url_ccdd <- "https://essence.syndromicsurveillance.org/nssp_essence/servlet/SyndromeDefinitionsServlet_CCDD?action=getCCDDTerms"
    
    ccdd_list <- myProfile$get_api_data(url_ccdd) %>%
      pluck("categories") %>%
      select(category) %>%
      mutate(
        category_api = tolower(category),
        category_api = gsub(" ", "%20", category_api)
      ) %>%
      arrange(category)
    
    # filter state-associated fips
    fips_for_url <- county_sf %>%
      rename(state_fips = STATEFP) %>%
      left_join(state_helper, by = "state_fips") %>%
      filter(state_abbr == input$State) %>%
      pull(GEOID) %>%
      as.character() %>%
      paste0(., collapse = "&facilityfips=")
    
    # CCDD category
    category_for_url <- ccdd_list %>%
      filter(category == input$CCDD) %>%
      pull(category_api)
    
    url <- paste0("https://essence2.syndromicsurveillance.org/nssp_essence/api/timeSeries?nonZeroComposite=false",
                  "&startMonth=january&graphOnly=true&datasource=va_hosp&startDate=", format(input$StartDate %m-% days(13), "%d%b%Y"), 
                  "&medicalGroupingSystem=essencesyndromes&userId=2362&multiStratVal=facilityfips&endDate=", 
                  format(input$EndDate, "%d%b%Y"), "&facilityfips=", fips_for_url, "&percentParam=ccddCategory",
                  "&graphOptions=multiplesmall&aqtTarget=TimeSeries&ccddCategory=", category_for_url, 
                  "&geographySystem=hospital&detector=probrepswitch&removeZeroSeries=true&timeResolution=daily&hasBeenE=1")#,
    #"&field=date&field=alert_percent&field=alert_count&field=line_label&field=facilityfips_id&field=data_count",
    #"&field=all_count&field=count&field=levels")
    
    df <- myProfile$get_api_data(url) %>%
      pluck("timeSeriesData") %>%
      clean_names() %>%
      mutate(
        date = as.Date(date), 
        alert_percent = case_when(
          color %in% c("grey", "blue") ~ "None",
          color == "yellow" ~ "Warning",
          color == "red" ~ "Alert"
        ),
        alert_count = case_when(
          color_data_count %in% c("grey", "blue") ~ "None",
          color_data_count == "yellow" ~ "Warning",
          color_data_count == "red" ~ "Alert"
        )
      ) %>%
      separate(line_label, c("state_abbr", "county"), sep = " - ") %>%
      select(
        state_abbr, 
        fips = facilityfips_id,
        county, 
        date,
        data_count, 
        all_count, 
        percent = count, 
        alert_percent,
        alert_count,
        p = levels,
        color,
        color_data_count
      ) %>%
      arrange(fips, date) %>%
      mutate(
        warning_or_alert_percent_factor = ifelse(
          alert_percent %in% c("Warning","Alert"), 1, 0),
        alert_percent_factor = ifelse(
          alert_percent == "Alert", 1, 0),
        alert_count_factor = ifelse(
          alert_count == "Alert", 1, 0),
        p = as.numeric(p)
      )
    
    #-----Compute each of total % CCDD alerts, alerts of alerts, and increasing CCDD percent alerts-----
    
    # Compute alerts over state-wide CCDD % df
    
    df_switch_percent <- df %>% 
      group_by(date)  %>%
      summarise(total_CCDD = sum(data_count),
                total_all = sum(all_count),
                .groups = 'drop') %>% 
      mutate(percent = total_CCDD/total_all) %>%
      select(date, percent) %>%
      alert_switch(., t=date, y=percent) %>%
      select(
        date,
        percent,
        alert_percent = alert,
        p.value_percent = p.value
      )
    
    # Compute alerts over total alert counts df
    
    df_switch_alert_count = df %>%
      group_by(date) %>%
      summarise(count = sum(alert_percent == "Alert")) %>%
      alert_switch(., t=date, y=count) %>%
      select(
        date,
        count,
        alert_alert = alert,
        p.value_alert = p.value
      )
    
    # Compute trend classification (i.e., increasing, decreasing, stable) alerts of alerts df
    
    ed_county_waves <- df %>%
      group_by(fips) %>%
      mutate(
        seven_day = slide(
          .x = tibble(data_count, all_count),
          .f = function(.x){
            (sum(.x$data_count) / sum(.x$all_count)) * 100
          },
          .before = 6, 
          .complete = FALSE
        ),
        seven_day = as.numeric(seven_day),
        seven_day = ifelse(is.nan(seven_day), 0, seven_day)
      ) %>%
      ungroup() %>%
      mutate(fips_copy = fips) %>%
      nest(data = -fips) %>%
      mutate(
        ss = map(.x = data, .f = function(.x) {
          
          .ss <- smooth.spline(x = as.numeric(.x$date), y = .x$seven_day, spar = 0.5)
          .fitted <- predict(.ss, as.numeric(.x$date))$y
          .deriv1 <- predict(.ss, as.numeric(.x$date), deriv = 1)$y
          
          data.frame(spline = .fitted, deriv1 = .deriv1)
          
        })
      ) %>%
      unnest(c(data, ss)) %>%
      group_by(fips) %>%
      mutate(
        row = row_number(), 
        trajectory = case_when(
          abs(deriv1) < 0.01 ~ "Stable", 
          deriv1 <= -0.01 ~ "Decreasing", 
          deriv1 >= 0.01 ~ "Increasing"
        )
      ) %>%
      group_by(fips, grp = with(rle(trajectory), rep(seq_along(lengths), lengths))) %>%
      mutate(
        period_total = max(seq_along(grp)), 
        counter = seq_along(grp), 
        start_date = min(date)
      ) %>%
      ungroup() %>%
      mutate(trajectory = factor(trajectory, levels = c("Increasing", "Stable", "Decreasing")),
             inc_factor = ifelse(trajectory == "Increasing", 1, 0)
      )
    
    ed_increasing_status <- ed_county_waves %>%
      group_by(date) %>%
      summarise(
        data_count = sum(data_count), 
        all_count = sum(all_count), 
        n_increasing = sum(trajectory == "Increasing"),
        n_decreasing = sum(trajectory == "Decreasing"), 
        n_stable = sum(trajectory == "Stable"), 
        n_total = n_distinct(fips)
      ) %>%
      mutate(
        percent = (data_count / all_count) * 100, 
        n_check = n_increasing + n_decreasing + n_stable
      ) %>%
      ungroup() %>%
      mutate(
        percent_increasing = (n_increasing / n_total) * 100,
        percent_decreasing = (n_decreasing / n_total) * 100, 
        percent_stable = (n_stable / n_total) * 100
      )
    
    ed_anomalies_trend <- ed_increasing_status %>%
      nest(data = everything()) %>%
      mutate(
        spline_out = map(.x = data, .f = function(.x) {
          
          .ss <- smooth.spline(x = as.numeric(.x$date), y = .x$n_increasing, spar = 0.5)
          .fitted <- predict(.ss, as.numeric(.x$date))$y
          .deriv1 <- predict(.ss, as.numeric(.x$date), deriv = 1)$y
          
          data.frame(fitted = .fitted, deriv1 = .deriv1)
          
        })
      ) %>%
      unnest(c(data, spline_out)) %>%
      mutate(
        alert_trend = case_when(
          deriv1 > 0.5 ~ "red",
          deriv1 <= 0.5 ~ "blue"
        ),
        alert_trend = factor(alert_trend, levels = c("red", "blue"))
      ) %>%
      select(
        date,
        n_increasing,
        alert_trend
      )
    
    #---------------------------------------------------------------------------------------
    
    df_all = df_switch_alert_count %>% 
      left_join(., df_switch_percent, by = 'date') %>%
      left_join(., ed_anomalies_trend, by = 'date') %>%
      tail(., -13)
    
    return(list(df_all, ed_county_waves))
  }
  
  observeEvent(input$go, {
    hideElement("p_choropleth")
    hideElement("alerts_choropleth")
    hideElement("increasing_choropleth")
    hideElement("joincount_alert")
    hideElement("joincount_warning_or_alert")
    hideElement("joincount_inc")
    hideElement("globalmoran")
    hideElement("date1")
    hideElement("date2")
    hideElement("date3")
    
    dfs <- get_and_mutate_dfs(input)
    
    # Update the reactive values
    Reactive_dfs$df_1 <- dfs[[1]]
    Reactive_dfs$df_2 <- dfs[[2]]
    
    selected_state$state <- state_sf %>% filter(STUSPS == input$State)
  })
  
  # time series subplots reactive element
  oPlot <- eventReactive(
    input$go,
    {
      alert_plot <- plot_ly(data = Reactive_dfs$df_1, source='plotlyts', x = ~date, y = ~count, type = 'scatter', mode = 'lines+markers',
                            marker = list(color = ~alert_alert, size=25/log(dim(Reactive_dfs$df_1)[1])),
                            line = list(color = "rgb(22, 96, 167)", width = 12/log(dim(Reactive_dfs$df_1)[1])),
                            hoverinfo = "text",
                            text = ~ paste(
                              "<br>Date:</b>", date,
                              "<br>Count:</b>", format(count, big.mark = ",")
                            ),
                            name = "Count of Alerting Regions")
      
      percent_plot <- plot_ly(data = Reactive_dfs$df_1, source='plotlyts', x = ~date, y = ~percent, type = "scatter", mode = "lines+markers",
                              marker = list(color= ~alert_percent, size=25/log(dim(Reactive_dfs$df_1)[1])),
                              line = list(color="gray", width=12/log(dim(Reactive_dfs$df_1)[1])),
                              hoverinfo = "text",
                              text = ~ paste(
                                "<br>Date:</b>", date,
                                "<br>%:</b>", format(percent, big.mark = ",")
                              ),
                              name = "% CCDD")
      
      trending_plot <- plot_ly(data = Reactive_dfs$df_1, source='plotlyts', x = ~date, y = ~n_increasing, type = 'scatter', mode = 'lines+markers',
                               marker = list(color = ~alert_trend, size=25/log(dim(Reactive_dfs$df_1)[1])),
                               line = list(color = "rgb(60, 0, 155)", width = 12/log(dim(Reactive_dfs$df_1)[1])),
                               hoverinfo = "text",
                               text = ~ paste(
                                 "<br>Date:</b>", date,
                                 "<br># Increasing:</b>", format(n_increasing, big.mark = ",")
                               ),
                               name = "Count of Increasing Regions")
      # subplots object
      plt <- subplot(percent_plot, alert_plot, trending_plot, nrows = 3, shareX = TRUE, margin = c(0.02, 0.02, 0.02, 0.02)) %>%
        layout(title = list(text = paste0("<b>", input$State,' ',input$CCDD,"<b>"), x = 0.5),
               hovermode = "x unified",
               xaxis = list(
                 title = "<b>Date<b>",
                 showspikes = TRUE,
                 spikemode = "across",
                 ticks = "outside",
                 spikedash = "dot",
                 spikecolor = "black",
                 spikethickness = -2,
                 tickformat="%b %d %Y",
                 ticklabelmode="period",
                 tickangle = 45
               ),
               yaxis = list(
                 title = list(text=paste0("<b>%</b>"),
                              font = list(
                                color = "grey")),
                 tickfont = list(color = "grey"), # 0s are offset
                 showline = TRUE,
                 showgrid = TRUE,
                 rangemode = 'tozero',
                 ticks = "outside",
                 showgrid = FALSE
               ),
               yaxis2 = list(
                 title = list(text = "<b>Alerts<b>",
                              font = list(
                                color = "rgb(22, 96, 167)")),
                 tickfont = list(color = "rgb(22, 96, 167)"),
                 showline = TRUE,
                 showgrid = TRUE,
                 rangemode = "tozero",
                 ticks = "outside"
               ),
               yaxis3 = list(
                 title = list(text = "<b>Rising<b>",
                              font = list(
                                color = "rgb(60, 0, 155)")),
                 tickfont = list(color = "rgb(60, 0, 155)"),
                 showline = TRUE,
                 showgrid = TRUE,
                 rangemode = "tozero",
                 ticks = "outside"
               )
        ) %>%
        event_register(.,'plotly_click')
      
      plt %>%
        config(modeBarButtons = list(list("toImage"), list("autoScale2d")))
    })
  
  # Outputs
  
  # Alerts time series
  output$tsPlotly <- renderPlotly({
    oPlot()
  })
  
  observeEvent(event_data("plotly_click", source = 'plotlyts'), {
    showElement("p_choropleth")
    showElement("alerts_choropleth")
    showElement("increasing_choropleth")
    showElement("joincount_alert")
    showElement("joincount_warning_or_alert")
    showElement("joincount_inc")
    showElement("globalmoran")
    showElement("date1")
    showElement("date2")
    showElement("date3")
    
    selected_date$date <- event_data("plotly_click", source = 'plotlyts')$x
    output$date1 <- renderText({selected_date$date})
    output$date2 <- renderText({selected_date$date})
    output$date3 <- renderText({selected_date$date})
    
    # Get selected state geometry
    selected_state_sf = county_sf %>% filter(STATEFP == as.integer(substr(Reactive_dfs$df_2$fips[1], start = 1, stop = 2)))
    Reactive_dfs$state_sf = selected_state_sf
    
    ##----------------Choropleth coloration-----------------------
    
    # Slice alerts df
    df_sf = st_as_sf(right_join(Reactive_dfs$df_2[Reactive_dfs$df_2$date == selected_date$date,], selected_state_sf[,c("NAME","GEOID","geometry")], by = c("fips" = "GEOID")))
    
    # get non-null rows for analysis
    df_sf_non_null = df_sf[!is.na(df_sf$county),]
    
    # Compute local moran df
    pts <- st_centroid(st_transform(df_sf_non_null, 3857))
    #> Warning: st_centroid assumes attributes are constant over geometries of x
    nb <- dnearneigh(pts, 0, 1000000)
    # Moran's I with Inverse Distance Weighting with alpha(exponent) = 1
    listw <- nb2listwdist(nb, as(pts, "Spatial"), type = "idw",
                          alpha = 1, zero.policy = TRUE, style = "raw")
    
    # check for 0 counties in each of the bins: alerting, increasing 
    if (nlevels(factor(df_sf_non_null$alert_percent_factor)) > 1) {
      JoinCount_alert <- joincount.test(factor(df_sf_non_null$alert_percent_factor), listw, alternative = 'greater')
    } else {
      JoinCount_alert <- NULL
    }
    
    if (nlevels(factor(df_sf_non_null$warning_or_alert_percent_factor)) > 1) {
      JoinCount_warning_or_alert <- joincount.test(factor(df_sf_non_null$warning_or_alert_percent_factor), listw, alternative = 'greater')
    } else {
      JoinCount_warning_or_alert = NULL
    }
    
    if (nlevels(factor(df_sf_non_null$inc_factor)) > 1) {
      JoinCount_increasing <- joincount.test(factor(df_sf_non_null$inc_factor), listw, alternative = 'greater')
    } else {
      JoinCount_increasing = NULL
    }
    
    globalMoran <- moran.test(df_sf_non_null$p, listw, alternative = 'greater')
    
    # renderText({paste0("Join Count 'Alert' p-value: ", ifelse(is.null(JoinCount_alert), "<b>NA</b>", paste("<B>",round(JoinCount_alert[[2]]$p.value[[1]],3),"</B>")))})
    output$globalmoran <- renderText({paste0("Global Moran's I p-value: ", round(globalMoran$p.value,3))})
    
    output$joincount_alert <- renderText({paste0("Join Count 'Alert' p-value: ", ifelse(is.null(JoinCount_alert), "NA", round(JoinCount_alert[[2]]$p.value[[1]],3)))})
    output$joincount_warning_or_alert <- renderText({paste0("Join Count 'Alert' or 'Warning' p-value: ", ifelse(is.null(JoinCount_warning_or_alert), "NA", round(JoinCount_warning_or_alert[[2]]$p.value[[1]],3)))})
    
    output$joincount_inc <- renderText({paste0("Join Count 'Increasing' p-value: ", ifelse(is.null(JoinCount_increasing), "NA", round(JoinCount_increasing[[2]]$p.value[[1]],3)))})
    
    # get coordinates of pts as data.frame
    xy = data.frame(st_coordinates(st_cast(st_centroid(df_sf)$geometry,"POINT")))
    
    ##------------Plot Leaflet Choropleths---------------------------
    
    # P-value choropleth map
    output$p_choropleth <- renderLeaflet({
      
      labels_p <- sprintf(
        "<strong>County: </strong>%s<br/>
        <strong>P-value: </strong>%s<br/>
        <strong>Percent: </strong>%s<br/>",
        df_sf$NAME,
        df_sf$p,
        df_sf$percent
      ) %>%
        lapply(htmltools::HTML)
      
      pal_p <- colorNumeric(
        palette = colorRampPalette(c('red','yellow','blue'), bias=log(0.1)/log(0.5))(100), #length(df_sf$p) 
        domain = c(0,1),
        na.color = "black",
        alpha = TRUE)
      
      p_leaf <-
        leaflet() %>%
        leaflet.extras::setMapWidgetStyle(list(background = "#FFFFFF")) %>%
        addPolylines(
          data = selected_state$state,
          opacity = 1,
          fillOpacity = 0,
          color = "black", 
          weight = 1.2
        ) %>%
        addPolygons(
          data = df_sf,
          layerId=~NAME,
          stroke = TRUE,
          smoothFactor = 0.5,
          color = "black",
          fillColor = ~pal_p(p),
          weight = 1.0,
          opacity = 0.2,
          fillOpacity = 0.5,
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
          group = 'counties'
        ) %>%
        addLegendNumeric(
          orientation = 'horizontal',
          height = 20,
          width = 120,
          bins = 11,
          position = "bottomright",
          title = "P-value",
          pal = pal_p,
          values = seq(from = 0, to = 1, length.out = 100), #df_sf$p,
          fillOpacity = 0.5
        ) %>%
        fitBounds(lng1 = min(xy['X']),
                  lat1 = min(xy['Y']),#-0.25*(max(xy['Y']) - min(xy['Y'])),
                  lng2 = max(xy['X']),
                  lat2 = max(xy['Y']))#-0.25*(max(xy['Y']) - min(xy['Y'])))
    })
    
    # Alerts choropleth map
    output$alerts_choropleth <- renderLeaflet({
      
      # leaflet alerts plot
      labels_alerts <- sprintf(
        "<strong>County: </strong>%s<br/>
         <strong>Alert: </strong>%s<br/>
         <strong>p-value: </strong>%s<br/>", 
        df_sf$NAME,
        df_sf$alert_percent,
        df_sf$p
      ) %>%
        lapply(htmltools::HTML)
      
      pal_alerts <- colorFactor(
        palette = c('blue','yellow','red'), 
        levels = c('None', 'Warning', 'Alert'),
        na.color = "black")
      
      alert_leaf <- leaflet() %>%
        leaflet.extras::setMapWidgetStyle(list(background = "#FFFFFF")) %>%
        addPolylines(
          data = selected_state$state,
          opacity = 1,
          fillOpacity = 0,
          color = "black", 
          weight = 1.2
        ) %>%
        addPolygons(
          data = df_sf,
          layerId=~NAME,
          stroke = TRUE,
          smoothFactor = 0.5,
          color = "black",
          fillColor = ~pal_alerts(alert_percent),
          weight = 1.0,
          opacity = 0.2,
          fillOpacity = 0.5,
          highlight = highlightOptions(
            weight = 1,
            color = "black",
            fillOpacity = 0.7,
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
          labelStyle = 'font-size: 12px;',
          pal = pal_alerts,
          values = df_sf$alert_percent,
          fillOpacity = 0.5
        ) %>%
        fitBounds(lng1 = min(xy['X']),
                  lat1 = min(xy['Y']),#-0.25*(max(xy['Y']) - min(xy['Y'])),
                  lng2 = max(xy['X']),
                  lat2 = max(xy['Y']))#-0.25*(max(xy['Y']) - min(xy['Y'])))
    })
    
    # Smoothed slope bins choropleth map
    output$increasing_choropleth <- renderLeaflet({
      
      labels_inc <- sprintf(
        "<strong>County: </strong>%s<br/>
        <strong>Slope: </strong>%s<br/>",
        df_sf$NAME,
        round(df_sf$deriv1, digits = 3)
      ) %>%
        lapply(htmltools::HTML)
      
      pal_inc <- colorFactor(
        palette = c('blue','yellow','red'), 
        levels = c('Decreasing', 'Stable', 'Increasing'),
        na.color = "black")
      
      inc_leaf <-
        leaflet() %>%
        leaflet.extras::setMapWidgetStyle(list(background = "#FFFFFF")) %>%
        addPolylines(
          data = selected_state$state,
          opacity = 1,
          fillOpacity = 0,
          color = "black", 
          weight = 1.2
        ) %>%
        addPolygons(
          data = df_sf,
          layerId=~NAME,
          stroke = TRUE,
          smoothFactor = 0.5,
          color = "black",
          fillColor = ~pal_inc(trajectory),
          weight = 1.0,
          opacity = 0.2,
          fillOpacity = 0.5,
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
          labelStyle = 'font-size: 12px;',
          pal = pal_inc,
          values = df_sf$trajectory,
          fillOpacity = 0.5
        ) %>%
        fitBounds(lng1 = min(xy['X']),
                  lat1 = min(xy['Y']),#-0.25*(max(xy['Y']) - min(xy['Y'])),
                  lng2 = max(xy['X']),
                  lat2 = max(xy['Y']))#-0.25*(max(xy['Y']) - min(xy['Y'])))
    })
  })
  
  #------------------ADD COUNTY-WISE TIME SERIES POPUP MODAL-----------------#
  
  # Reactive element to plot time series from p_choropleth on click
  observeEvent(input$p_choropleth_shape_click, {
    p_choropleth_click <- input$p_choropleth_shape_click
    selected_county <- p_choropleth_click$id
    
    time_series_data <- st_as_sf(right_join(Reactive_dfs$df_2, Reactive_dfs$state_sf[,c("NAME","GEOID")], by = c("fips" = "GEOID"))) %>%
      select(fips,
             NAME,
             date,
             percent,
             color) %>%
      as.data.frame() %>%
      mutate(NAME = as.character(NAME))
    
    df_temp = time_series_data[time_series_data$NAME == selected_county,]
    
    if(dim(df_temp)[1] > 1){
      showModal(modalDialog(
        plotlyOutput("CountyTimeSeriesPlot"),
        size = "l"
      ))
    } else {
      showModal(modalDialog(
        paste0('There is no data available for this county'),
        size = "s"
      ))
    }
    
    output$CountyTimeSeriesPlot <- renderPlotly({
      
      df_temp_before = df_temp %>% filter(date <= selected_date$date)
      df_temp_after = df_temp %>% filter(date >= selected_date$date)
      
      plot_ly() %>%
        add_trace(data=df_temp_before, x=~date, y=~percent, type='scatter', mode='lines+markers',
                  marker = list(color = ~color),
                  hoverinfo = "text",
                  name = paste0("Before ", selected_date$date),
                  text = ~ paste(
                    "<br>Date:</b>", date,
                    "<br>%:</b>", format(percent, big.mark = ",")
                  )) %>%
        add_trace(data=df_temp_after, x=~date, y=~percent, type='scatter', mode='lines+markers',
                  line = list(color = 'gray', dash = 'dot'),
                  marker = list(color = ~color),
                  hoverinfo = "text",
                  name = paste0("After ", selected_date$date),
                  text = ~ paste(
                    "<br>Date:</b>", date,
                    "<br>%:</b>", format(percent, big.mark = ",")
                  )) %>%
        layout(shapes = list(
          type = "line",
          x0 = selected_date$date,
          x1 = selected_date$date,
          y0 = min(df_temp$percent),
          y1 = max(df_temp$percent),
          line = list(color = "black", width = 2)
        ),
        title=paste0("<b>", selected_county, ": ", input$CCDD,"<b>"),
        yaxis = list(title=paste0("<b>Percent (%)<b>")),
        xaxis = list(title=paste0("<b>Date<b>")))
      })
    })
  
  # Reactive element to plot time series from alerts_choropleth on click
  observeEvent(input$alerts_choropleth_shape_click, {
    alerts_choropleth_click <- input$alerts_choropleth_shape_click
    selected_county <- alerts_choropleth_click$id
    
    time_series_data <- st_as_sf(right_join(Reactive_dfs$df_2, Reactive_dfs$state_sf[,c("NAME","GEOID")], by = c("fips" = "GEOID"))) %>%
      select(fips,
             NAME,
             date,
             percent,
             color) %>%
      as.data.frame() %>%
      mutate(NAME = as.character(NAME))
    
    df_temp = time_series_data[time_series_data$NAME == selected_county,]
    
    if(dim(df_temp)[1] > 1){
      showModal(modalDialog(
        plotlyOutput("CountyTimeSeriesPlot"),
        size = "l"
      ))
    } else {
      showModal(modalDialog(
        paste0('There is no data available for this county'),
        size = "s"
      ))
    }
    
    output$CountyTimeSeriesPlot <- renderPlotly({
      
      df_temp_before = df_temp %>% filter(date <= selected_date$date)
      df_temp_after = df_temp %>% filter(date >= selected_date$date)
      
      plot_ly() %>%
        add_trace(data=df_temp_before, x=~date, y=~percent, type='scatter', mode='lines+markers',
                  marker = list(color = ~color),
                  hoverinfo = "text",
                  name = paste0("Before ", selected_date$date),
                  text = ~ paste(
                    "<br>Date:</b>", date,
                    "<br>%:</b>", format(percent, big.mark = ",")
                  )) %>%
        add_trace(data=df_temp_after, x=~date, y=~percent, type='scatter', mode='lines+markers',
                  line = list(color = 'gray', dash = 'dot'),
                  marker = list(color = ~color),
                  hoverinfo = "text",
                  name = paste0("After ", selected_date$date),
                  text = ~ paste(
                    "<br>Date:</b>", date,
                    "<br>%:</b>", format(percent, big.mark = ",")
                  )) %>%
        layout(shapes = list(
          type = "line",
          x0 = selected_date$date,
          x1 = selected_date$date,
          y0 = min(df_temp$percent),
          y1 = max(df_temp$percent),
          line = list(color = "black", width = 2)
        ),
        title=paste0("<b>", selected_county, ": ", input$CCDD,"<b>"),
        yaxis = list(title=paste0("<b>Percent (%)<b>")),
        xaxis = list(title=paste0("<b>Date<b>")))
    })
  })
  
  # Reactive element to plot time series from increasing_choropleth on click
  observeEvent(input$increasing_choropleth_shape_click, {
    increasing_choropleth_click <- input$increasing_choropleth_shape_click
    selected_county <- increasing_choropleth_click$id
    
    time_series_data <- st_as_sf(right_join(Reactive_dfs$df_2, Reactive_dfs$state_sf[,c("NAME","GEOID")], by = c("fips" = "GEOID"))) %>%
      select(fips,
             NAME,
             date,
             percent,
             color,
             spline) %>%
      as.data.frame() %>%
      mutate(NAME = as.character(NAME))
    
    df_temp = time_series_data[time_series_data$NAME == selected_county,]
    
    if(dim(df_temp)[1] > 1){
      showModal(modalDialog(
        plotlyOutput("CountyTimeSeriesPlot"),
        size = "l"
      ))
    } else {
      showModal(modalDialog(
        paste0('There is no data available for this county'),
        size = "s"
      ))
    }
    
    output$CountyTimeSeriesPlot <- renderPlotly({
      
      df_temp_before = df_temp %>% filter(date <= selected_date$date)
      df_temp_after = df_temp %>% filter(date >= selected_date$date)
      df_spline = subset(df_temp_before, df_temp_before$date > as.Date(selected_date$date)-7)
      
      plot_ly() %>%
        add_trace(data=df_temp_before, x=~date, y=~percent, type='scatter', mode='lines+markers',
                  marker = list(color = ~color),
                  hoverinfo = "text",
                  name = paste0("Before ", selected_date$date),
                  text = ~ paste(
                    "<br>Date:</b>", date,
                    "<br>%:</b>", format(percent, big.mark = ",")
                  )) %>%
        add_trace(data=df_temp_after, x=~date, y=~percent, type='scatter', mode='lines+markers',
                  line = list(color = 'gray', dash = 'dot'),
                  marker = list(color = ~color),
                  hoverinfo = "text",
                  name = paste0("After ", selected_date$date),
                  text = ~ paste(
                    "<br>Date:</b>", date,
                    "<br>%:</b>", format(percent, big.mark = ",")
                  )) %>%
        add_trace(data=df_spline, x=~date, y=~spline, type='scatter', mode='lines',
                  line = list(color = "rgb(60, 0, 155)", width=5),
                  #marker = list(color = ~color),
                  hoverinfo = "text",
                  name = paste0("7-Day Smoothing Spline"),
                  text = ~ paste(
                    "<br>Date:</b>", date,
                    "<br>%:</b>", format(spline, big.mark = ",")
                  )) %>%
        layout(shapes = list(
          type = "line",
          x0 = selected_date$date,
          x1 = selected_date$date,
          y0 = min(df_temp$percent),
          y1 = max(df_temp$percent),
          line = list(color = "black", width = 2)
        ),
        title=paste0("<b>", selected_county, ": ", input$CCDD,"<b>"),
        yaxis = list(title=paste0("<b>Percent (%)<b>")),
        xaxis = list(title=paste0("<b>Date<b>")))
    })
  })
  
  #-------------------SYNCHRONIZE MAPS VIEW AND ZOOM---------------------------#
  # Initialize flags to keep track of user action for each map
  user_action <- reactiveValues(p_choropleth = FALSE, alerts_choropleth = FALSE, increasing_choropleth = FALSE)
  
  # Define names of all choropleth maps
  choropleth_maps <- c("p_choropleth", "alerts_choropleth", "increasing_choropleth")
  
  # Observe changes in bounds for each map
  lapply(choropleth_maps, function(map_name) {
    
    observeEvent(input[[paste0(map_name, "_bounds")]], {
      if(!user_action[[map_name]]) { # User action
        
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
}

shinyApp(ui, server)