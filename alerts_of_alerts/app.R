# © 2022 The Johns Hopkins University Applied Physics Laboratory LLC
# Development of this software was sponsored by the U.S. Government under contracts no. 75D30120C07643, 75D30122C15442

#----------------------------------------------------
# Alerts of Alerts App
# Authors:
#   Joshua Kimrey
#   Michael Sheppard
#   Raghav Ramachandran
#   Howard Burkom
#   Roseric Azondekon

# load libraries
suppressPackageStartupMessages({
  packages <- c(
    "shiny", "shinyjs", "dplyr", "Rnssp", "purrr",
    "data.table", "lubridate", "shinycssloaders",
    "plotly", "shinyWidgets", "sf", "shinythemes",
    "janitor", "tidyverse", "leaflet", "leaflegend",
    "spdep", "shinydashboard", "htmltools",
    "leafsync", "knitr", "kableExtra", "mgcv", "gratia"
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
    word = "methods",
    title,
    content,
    placement = c("right", "top", "left", "bottom"),
    trigger = c("click", "hover", "focus", "manual"),
    icon_name = "question-circle",
    icon_style = "color:red; cursor:pointer;") {
  
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
    tags$span(
      shiny::icon(name = icon_name, class = "shinyhelper-icon", style = icon_style),
      word,
      style = paste("margin-left:10px;", icon_style),
      `data-toggle` = "popover",
      title = title,
      `data-content` = content,
      `data-animation` = TRUE,
      `data-placement` = match.arg(placement, several.ok = TRUE)[1],
      `data-trigger` = match.arg(trigger, several.ok = TRUE)[1]
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

states <- as.character(sort(state_helper$state_name))
states <- c(states, "All")

# Read in CCDD Categories

ccdd_cats <- "https://essence.syndromicsurveillance.org/nssp_essence/api/datasources/va_hosp/fields/ccddCategory" %>%
  get_api_data(profile = myProfile) %>%
  pluck("values") %>%
  pull("value") %>%
  try(silent = TRUE)

if (any(class(ccdd_cats) == "try-error")) {
  cli::cli_abort("App failed to establish connection with ESSENCE server!
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
        sidebarPanel(width=3,
                     helpPopup(
                       id = "", word="App Summary", title = "",
                       content = paste0(
                         "This app tests for and visualizes both temporal and spatial alerts for regions with a user-selected State, for a user-selected CC & DD Category, and over a user-defined time period. Temporal alerts are tested for using 3 statewide ",
                         "diagnostics of syndrome severity. The 3 state-wide temporal diagnostics are: 1) ",
                         "total statewide percent of ED visits, 2) number of alerting ",
                         "counties/regions, and 3) the number of counties/regions ",
                         "estimated, through the use of Generative Additive Modeling, to have increasing case counts. The second two ",
                         "diagnostics are variants of what has been coined 'Alerts of Alerts'. The region-level data associated to or underlying each of these state-level diagnostics ",
                         "can be visualized for the currently selected date as a set of ",
                         "choropleth maps. Presented with each of these maps are the results of statistical tests ",
                         "for spatial autocorrelation (i.e., spatial clustering)."
                       ),
                       placement = "right", trigger = "click",
                       icon_name = "question-circle",
                       icon_style = "color:blue;font-size:15px"
                     ),
                     selectInput("State", "State", states, 'Florida'),
                     selectInput("CCDD", "CC & DD Category", ccdd_cats, 
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
                                   value = EndDate_0, max = EndDate_0)
                       ),
                       tags$style(HTML(".datepicker {z-index:99999 !important;}"))
                     ),
                     actionButton("go", "Load Data"),
                     downloadButton(outputId = "report", "Download Report")
        ),
        mainPanel(
          fluidRow(
            box(
              title = div(
                h3("Alerts of Alerts Time Series", 
                   style = 'font-size:18px; font-weight: bold; display: inline-block;'),
                helpPopup(
                  id = "", 
                  word="Methods", 
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
                plotlyOutput(outputId = "tsPlotly", height = "400px")
              ),
              width = 12
            ),
            box(
              title = div(
                h3("Spatial Distribution of p-values", 
                   style = 'font-size:18px; font-weight: bold; display: inline-block; margin-right: 10px;'),
                helpPopup(
                  id = "", 
                  word="Methods", 
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
              withSpinner(leafletOutput("p_choropleth", height = "200px")),
              box(
                h3(textOutput('date1'), 
                   style = 'font-size:14px; font-weight: bold;'),
                h1(textOutput('globalmoran'), style = 'font-size:14px;')
              ),
              width = 4
            ),
            box(
              title = div(
                h3("Spatial Distribution of Alerts", 
                   style = 'font-size:18px; font-weight: bold; height: 30px; color: rgb(22, 96, 167); display: inline-block; margin-right: 10px;'),
                helpPopup(
                  id = "", 
                  word="Methods", 
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
              withSpinner(leafletOutput("alerts_choropleth", height = "200px")),
              box(
                h3(textOutput('date2'), 
                   style = 'font-size:14px; font-weight: bold;'),
                h1(textOutput('joincount_alert'), 
                   style = 'font-size:14px;'),
                h1(textOutput('joincount_warning_or_alert'), 
                   style = 'font-size:14px;')
              ),
              width = 4
            ),
            box(
              title = div(
                h3("Spatial Distribution of Trends", 
                   style = 'font-size:18px; font-weight: bold; height: 30px; color: rgb(60, 0, 155); display: inline-block; margin-right: 10px;'),
                helpPopup(
                  id = "", 
                  word="Methods", 
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
              withSpinner(leafletOutput("increasing_choropleth", height = "200px")),
              box(
                h3(textOutput('date3'), style = 'font-size:14px; font-weight: bold;'),
                h1(textOutput('joincount_inc'), style = 'font-size:14px;')
              ),
              width = 4
            )
          )
        )
      )
    )
  )
)

# Server object

server <- function(input, output, session) {
  options(warn = -1)
  
  # To avoid RStudio timeouts -- server code
  output$keepAlive <- renderText({
    req(input$count)
    paste("keep alive ", input$count)
  })
  
  Reactive_dfs <- reactiveValues(df_1 = NULL, df_2 = NULL)
  selected_state <- reactiveValues(fp = NULL, state_sf = NULL, 
                                   county_sf = NULL, df_sf = NULL)
  
  selected <- reactiveValues(state = NULL, CCDD = NULL, startDate = NULL, endDate = NULL, maps_date = NULL)
  plotly_dims <- reactiveValues(width = NULL, height = NULL)
  stat_test <- reactiveValues(globalMoran=NULL, JoinCount_alert=NULL,
                              JoinCount_warning_or_alert=NULL,
                              JoinCount_increasing=NULL)
  
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
    
    if (input$State == 'All') {
      # obtain all U.S. fips
      fips_for_url <- county_sf %>%
        rename(state_fips = STATEFP) %>%
        left_join(state_helper, by = "state_fips") %>%
        pull(GEOID) %>%
        as.character() %>%
        paste0(., collapse = "&facilityfips=")
    } else {
      # filter state-associated fips
      fips_for_url <- county_sf %>%
        rename(state_fips = STATEFP) %>%
        left_join(state_helper, by = "state_fips") %>%
        filter(state_name == input$State) %>%
        pull(GEOID) %>%
        as.character() %>%
        paste0(., collapse = "&facilityfips=")
    }
    
    # CCDD category
    category_for_url <- ccdd_list %>%
      filter(category == input$CCDD) %>%
      pull(category_api)
    
    url <- paste0("https://essence.syndromicsurveillance.org/nssp_essence/api/timeSeries?nonZeroComposite=false",
                  "&startMonth=january&graphOnly=true&datasource=va_hosp&startDate=", format(input$StartDate %m-% days(13), "%d%b%Y"), 
                  "&medicalGroupingSystem=essencesyndromes&userId=2362&multiStratVal=facilityfips&endDate=", 
                  format(input$EndDate, "%d%b%Y"), "&facilityfips=", fips_for_url, "&percentParam=ccddCategory",
                  "&graphOptions=multiplesmall&aqtTarget=TimeSeries&ccddCategory=", category_for_url, 
                  "&geographySystem=hospital&detector=probrepswitch&removeZeroSeries=true&timeResolution=daily&hasBeenE=1")
    
    withProgress(message="Loading and processing data:", value=0, {
      incProgress(0.25, detail = "Loading data...")
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
      
      incProgress(0.25, detail = "Testing CC & DD % for Alerts...")
      df_switch_percent <- df %>% 
        group_by(date) %>%
        summarise(total_CCDD = sum(data_count),
                  total_all = sum(all_count),
                  .groups = 'drop') %>%
        mutate(percent = (total_CCDD / total_all)*100.0) %>%
        { 
          nan_dates <- .$date[is.nan(.$percent)]
          if(length(nan_dates) > 0) { 
            warning(paste("0 statewide visits reported for dates:", paste(nan_dates, collapse = ", "), ". CCDD % treated as 0."))
          } 
          mutate(., percent = ifelse(is.nan(percent), 0, percent))
        } %>%
        select(date, percent) %>%
        alert_switch(., t = date, y = percent) %>%
        select(
          date,
          percent,
          alert_percent = alert,
          p.value_percent = p.value
        )
      
      # Compute alerts over total alert counts df
      
      incProgress(0.25, detail = "Testing daily Alert counts % for Alerts...")
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
      
      incProgress(0.25, detail = "Estimating county trends and testing for increase (this may take a moment)...")
      
      #future::plan(multicore, workers = 5)
      
      ed_county_waves <- df %>%
        nest(data = -fips) %>%
        mutate(
          model = map(.x = data, function (.x) {
            
            sparsity_ratio90 <- .x %>%
              arrange(date) %>%
              slice_tail(n = 90) %>%
              mutate(
                nonzero_obs = sum(data_count > 0), 
                sp_ratio = n() / nonzero_obs
              ) %>%
              pull(sp_ratio) %>%
              unique()
            
            sparsity_ratio <- .x %>%
              arrange(date) %>%
              filter(date >= max(date) %m-% years(1) + 1) %>%
              mutate(
                nonzero_obs = sum(data_count > 0), 
                sp_ratio = n() / nonzero_obs
              ) %>%
              pull(sp_ratio) %>%
              unique() 
            
            if (sparsity_ratio < 5 & sparsity_ratio90 < 5) {
              
              .y <- .x %>%
                select(
                  date,
                  data_count,
                  all_count
                ) %>%
                mutate(date = as.double(date))
              
              .gam_out <- gam(cbind(data_count, all_count - data_count) ~ s(as.double(date), 
                                                                            bs = "ad"), 
                              family = binomial, 
                              data = .y, 
                              method = "REML",
                              control = gam.control(maxit = 3, eps = 1e-2, mgcv.tol = 1e-2, trace = FALSE))
              
              if (.gam_out$converged) {
                
                .deriv <- derivatives(.gam_out, data = .y, n = nrow(.y), level = 0.95, type = "central")
                
                data.frame(
                  fitted = .gam_out$fitted.values * 100, 
                  sp_ratio90 = sparsity_ratio90,
                  sp_ratio = sparsity_ratio
                ) %>%
                  bind_cols(.deriv) %>%
                  rename(
                    deriv = .derivative,
                    lower = .lower_ci, 
                    upper = .upper_ci
                  )
                
              } else {
                
                data.frame(
                  fitted = rep(NA, nrow(.x)), 
                  sp_ratio90 = sparsity_ratio90,
                  sp_ratio = sparsity_ratio
                ) %>%
                  mutate(
                    lower = NA, 
                    deriv = NA, 
                    upper = NA
                  )
                
              }
              
              
            } else {
              
              data.frame(
                fitted = rep(NA, nrow(.x)),
                sp_ratio90 = sparsity_ratio90,
                sp_ratio = sparsity_ratio
              ) %>%
                mutate(
                  lower = NA, 
                  deriv = NA, 
                  upper = NA
                )
              
            }
            
          })
        ) %>%
        unnest(c(data, model)) %>%
        group_by(fips) %>%
        mutate(
          row = row_number(), 
          trajectory = case_when(
            is.na(sp_ratio90) ~ "Sparse",
            sp_ratio90 >= 5 | sp_ratio >= 5 ~ "Sparse",
            deriv > 0 & lower > 0 ~ "Increasing",
            deriv < 0 & upper < 0 ~ "Decreasing",
            deriv > 0 & lower < 0 ~ "Stable",
            deriv < 0 & upper > 0 ~ "Stable",
            is.na(deriv) ~ "Sparse",
            TRUE ~ "Stable"
          )
        ) %>%
        group_by(fips, grp = with(rle(trajectory), rep(seq_along(lengths), lengths))) %>%
        mutate(
          period_total = max(seq_along(grp)), 
          counter = seq_along(grp), 
          start_date = min(date)
        ) %>%
        ungroup() %>%
        mutate(trajectory = factor(trajectory, levels = c("Increasing", "Stable", "Decreasing", "Sparse")))
      
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
        mutate(
          sparsity_ratio90 = {
            ed_increasing_status %>%
              arrange(date) %>%
              slice_tail(n = 90) %>%
              mutate(
                nonzero_obs = sum(n_increasing > 0), 
                sp_ratio = n() / nonzero_obs
              ) %>%
              pull(sp_ratio) %>%
              unique()
          },
          sparsity_ratio = {
            ed_increasing_status %>%
              arrange(date) %>%
              filter(date >= max(date) %m-% years(1) + 1) %>%
              mutate(
                nonzero_obs = sum(n_increasing > 0), 
                sp_ratio = n() / nonzero_obs
              ) %>%
              pull(sp_ratio) %>%
              unique()
          }
        ) %>%
        mutate(
          model_output = if (sparsity_ratio[1] < 5 & sparsity_ratio90[1] < 5) {
            .y <- ed_increasing_status %>%
              select(
                date,
                n_increasing,
                n_total
              ) %>%
              mutate(date = as.double(date))
            
            .gam_out <- gam(cbind(n_increasing, n_total - n_increasing) ~ s(as.double(date), 
                                                                            bs = "ad"), 
                            family = binomial, 
                            data = .y, 
                            method = "REML",
                            control = gam.control(maxit = 3, eps = 1e-2, mgcv.tol = 1e-2, trace = FALSE))
            
            if (.gam_out$converged) {
              
              .deriv <- derivatives(.gam_out, data = .y, n = nrow(.y), level = 0.95, type = "central")
              
              data.frame(
                fitted = .gam_out$fitted.values * n_total, 
                sp_ratio90 = sparsity_ratio90,
                sp_ratio = sparsity_ratio
              ) %>%
                bind_cols(.deriv) %>%
                rename(
                  deriv = .derivative,
                  lower = .lower_ci, 
                  upper = .upper_ci
                )
              
            } else {
              
              data.frame(
                fitted = rep(NA, nrow(ed_increasing_status)), 
                sp_ratio90 = sparsity_ratio90,
                sp_ratio = sparsity_ratio
              ) %>%
                mutate(
                  lower = NA, 
                  deriv = NA, 
                  upper = NA
                )
              
            }
            
            
          } else {
            
            data.frame(
              fitted = rep(NA, nrow(ed_increasing_status)),
              sp_ratio90 = sparsity_ratio90,
              sp_ratio = sparsity_ratio
            ) %>%
              mutate(
                lower = NA, 
                deriv = NA, 
                upper = NA
              )
            
          }
        ) %>%
        unnest(cols = c(model_output)) %>%
        mutate(
          row = row_number(), 
          trajectory = case_when(
            is.na(sp_ratio90) ~ "Sparse", 
            sp_ratio90 >= 5 | sp_ratio >= 5 ~ "Sparse", 
            deriv > 0 & lower > 0 ~ "Increasing", 
            deriv < 0 & upper < 0 ~ "Decreasing",
            deriv > 0 & lower < 0 ~ "Stable", 
            deriv < 0 & upper > 0 ~ "Stable", 
            is.na(deriv) ~ "Sparse", 
            TRUE ~ "Stable"
          )
        ) %>%
        group_by(grp = with(rle(trajectory), rep(seq_along(lengths), lengths))) %>%
        mutate(
          period_total = max(seq_along(grp)), 
          counter = seq_along(grp), 
          start_date = min(date)
        ) %>%
        ungroup() %>%
        mutate(trajectory = factor(trajectory, levels = c("Increasing", "Stable", "Decreasing", "Sparse"))) %>%
        mutate(
          alert_trend = case_when(
            trajectory == "Increasing" ~ "red",
            trajectory == "Stable" ~ "yellow",
            trajectory == "Decreasing" ~ "blue",
            trajectory == "Sparse" ~ "lightgray"
          ),
          alert_trend = factor(alert_trend, levels = c("red", "yellow", "blue", "lightgray"))
        ) %>%
        select(
          date,
          n_increasing,
          fitted,
          alert_trend
        )
    })
    
    #---------------------------------------------------------------------------------------
    
    df_all = df_switch_alert_count %>% 
      left_join(., df_switch_percent, by = 'date') %>%
      left_join(., ed_anomalies_trend, by = 'date') %>%
      tail(., -13)
    
    return(list(df_all, ed_county_waves))
  }
  
  # Reactive function to store the binary empty df status
  master_empty <- reactive({
    is.null(Reactive_dfs$df_1)
  })
  
  observe({
    disable <- master_empty()
    shinyjs::toggleState("report", !disable)
  })
  
  compute_and_assign_mapping_output_reactives <- reactive({
    
    JoinCount_alert <- NULL
    JoinCount_warning_or_alert = NULL
    JoinCount_increasing = NULL
    globalMoran = NULL
    
    output$date1 <- renderText({selected$maps_date})
    output$date2 <- renderText({selected$maps_date})
    output$date3 <- renderText({selected$maps_date})
    
    selected_state$df_sf = st_as_sf(right_join(Reactive_dfs$df_2[Reactive_dfs$df_2$date == selected$maps_date,], selected_state$county_sf[,c("NAME","GEOID","geometry")], by = c("fips" = "GEOID")))
    
    if (input$State == "All") {
      lookup_table <- st_drop_geometry(selected_state$df_sf) %>%
        filter(!is.na(state_abbr)) %>%
        mutate(fips_prefix = substr(fips, 1, 2)) %>%
        select(fips_prefix, state_abbr) %>%
        mutate(state_abbr = ifelse(state_abbr == "District of Columbia", "DC", state_abbr)) %>%
        distinct()
      
      selected_state$df_sf = selected_state$df_sf %>%
        mutate(fips_prefix = substr(fips, 1, 2)) %>%
        left_join(lookup_table, by = "fips_prefix", suffix = c("", "_lookup")) %>%
        mutate(state_abbr = ifelse(is.na(state_abbr), state_abbr_lookup, state_abbr)) %>%
        mutate(NAME = paste(NAME, state_abbr, sep = ", ")) %>%
        select(-fips_prefix, -state_abbr_lookup)
    }
    
    # get non-null rows for analysis
    df_sf_non_null = selected_state$df_sf[!is.na(selected_state$df_sf$county),]
    
    if (nrow(df_sf_non_null) > 0) {
      # Compute local moran df
      pts <- st_centroid(st_transform(df_sf_non_null, 3857))
      #> Warning: st_centroid assumes attributes are constant over geometries of x
      #nb <- dnearneigh(pts, 0, 100000)
      
      # Convert to a matrix of coordinates
      coords <- st_coordinates(pts)
      
      # Find k nearest neighbors
      k <- 20
      k_actual = min(k, nrow(df_sf_non_null)-1)
      knn <- knearneigh(coords, k = k_actual)
      nb <- knn2nb(knn)
      
      # Moran's I with Inverse Distance Weighting with alpha(exponent) = 1
      listw <- nb2listwdist(nb, as(pts, "Spatial"), type = "idw",
                            alpha = 1, zero.policy = TRUE, style = "raw")
      
      # check for 0 counties in each of the bins: alerting, increasing
      if (nlevels(factor(df_sf_non_null$alert_percent_factor)) > 1) {
        JoinCount_alert <- joincount.test(factor(df_sf_non_null$alert_percent_factor), listw, alternative = 'greater')
      } 
      
      if (nlevels(factor(df_sf_non_null$warning_or_alert_percent_factor)) > 1) {
        JoinCount_warning_or_alert <- joincount.test(factor(df_sf_non_null$warning_or_alert_percent_factor), listw, alternative = 'greater')
      } 
      
      if (nlevels(factor(df_sf_non_null$inc_factor)) > 1) {
        JoinCount_increasing <- joincount.test(factor(df_sf_non_null$inc_factor), listw, alternative = 'greater')
      }
      
      globalMoran <- tryCatch(
        moran.test(df_sf_non_null$p, listw, alternative = 'greater'),
        error = function(e) NULL
      )
    }
    
    stat_test$JoinCount_alert = ifelse(is.null(JoinCount_alert), "NA", round(JoinCount_alert[[2]]$p.value[[1]],3))
    stat_test$JoinCount_warning_or_alert = ifelse(is.null(JoinCount_warning_or_alert), "NA", round(JoinCount_warning_or_alert[[2]]$p.value[[1]],3))
    stat_test$JoinCount_increasing = ifelse(is.null(JoinCount_increasing), "NA", round(JoinCount_increasing[[2]]$p.value[[1]],3))
    stat_test$globalMoran = ifelse(is.null(globalMoran$p.value), "NA", round(globalMoran$p.value,3))
    
    output$globalmoran <- renderText({paste0("Global Moran's I p-value: ", stat_test$globalMoran)})
    output$joincount_alert <- renderText({paste0("Join Count 'Alert' p-value: ", stat_test$JoinCount_alert)})
    output$joincount_warning_or_alert <- renderText({paste0("Join Count 'Alert' or 'Warning' p-value: ", stat_test$JoinCount_warning_or_alert)})
    output$joincount_inc <- renderText({paste0("Join Count 'Increasing' p-value: ", stat_test$JoinCount_increasing)})
  })
  
  plotly_plot = reactive({
    color_scale <- c('NA' = 'gray', 'None' = 'blue', 'Warning' = 'yellow', 'Alert' = 'red')
    
    # Adding annotations as sub-legends
    topy = 0.9
    middley = 0.55
    bottomy = 0.125
    
    annotations <- list(
      # annotations for 1st legendgroup
      list(x = 1.1, y = topy, xref = 'paper', yref = 'paper', showarrow = FALSE, text = 'None', xanchor = 'left', align = 'left'),
      list(x = 1.1, y = topy, xref = 'paper', yref = 'paper', showarrow = FALSE, text = '', bgcolor = 'blue', bordercolor = 'black', borderwidth = 1, borderpad = 1, height = 10, width = 10),
      list(x = 1.1, y = topy-0.05, xref = 'paper', yref = 'paper', showarrow = FALSE, text = 'Warning', xanchor = 'left', align = 'left'),
      list(x = 1.1, y = topy-0.05, xref = 'paper', yref = 'paper', showarrow = FALSE, text = '', bgcolor = 'yellow', bordercolor = 'black', borderwidth = 1, borderpad = 1, height = 10, width = 10),
      list(x = 1.1, y = topy-0.1, xref = 'paper', yref = 'paper', showarrow = FALSE, text = 'Alert', xanchor = 'left', align = 'left'),
      list(x = 1.1, y = topy-0.1, xref = 'paper', yref = 'paper', showarrow = FALSE, text = '', bgcolor = 'red', bordercolor = 'black', borderwidth = 1, borderpad = 1, height = 10, width = 10),
      # annotations for 2nd legendgroup
      list(x = 1.1, y = middley, xref = 'paper', yref = 'paper', showarrow = FALSE, text = 'None', xanchor = 'left', align = 'left'),
      list(x = 1.1, y = middley, xref = 'paper', yref = 'paper', showarrow = FALSE, text = '', bgcolor = 'blue', bordercolor = 'black', borderwidth = 1, borderpad = 1, height = 10, width = 10),
      list(x = 1.1, y = middley-0.05, xref = 'paper', yref = 'paper', showarrow = FALSE, text = 'Warning', xanchor = 'left', align = 'left'),
      list(x = 1.1, y = middley-0.05, xref = 'paper', yref = 'paper', showarrow = FALSE, text = '', bgcolor = 'yellow', bordercolor = 'black', borderwidth = 1, borderpad = 1, height = 10, width = 10),
      list(x = 1.1, y = middley-0.1, xref = 'paper', yref = 'paper', showarrow = FALSE, text = 'Alert', xanchor = 'left', align = 'left'),
      list(x = 1.1, y = middley-0.1, xref = 'paper', yref = 'paper', showarrow = FALSE, text = '', bgcolor = 'red', bordercolor = 'black', borderwidth = 1, borderpad = 1, height = 10, width = 10),
      # annotations for 3rd legendgroup
      list(x = 1.1, y = bottomy, xref = 'paper', yref = 'paper', showarrow = FALSE, text = 'Decreasing', xanchor = 'left', align = 'left'),
      list(x = 1.1, y = bottomy, xref = 'paper', yref = 'paper', showarrow = FALSE, text = '', bgcolor = 'blue', bordercolor = 'black', borderwidth = 1, borderpad = 1, height = 10, width = 10),
      list(x = 1.1, y = bottomy-0.05, xref = 'paper', yref = 'paper', showarrow = FALSE, text = 'Stable', xanchor = 'left', align = 'left'),
      list(x = 1.1, y = bottomy-0.05, xref = 'paper', yref = 'paper', showarrow = FALSE, text = '', bgcolor = 'yellow', bordercolor = 'black', borderwidth = 1, borderpad = 1, height = 10, width = 10),
      list(x = 1.1, y = bottomy-0.1, xref = 'paper', yref = 'paper', showarrow = FALSE, text = 'Increasing', xanchor = 'left', align = 'left'),
      list(x = 1.1, y = bottomy-0.1, xref = 'paper', yref = 'paper', showarrow = FALSE, text = '', bgcolor = 'red', bordercolor = 'black', borderwidth = 1, borderpad = 1, height = 10, width = 10)
    )
    
    percent_plot <- plot_ly(data = Reactive_dfs$df_1, source='plotlyts', x = ~date, y = ~percent, type = "scatter", mode = "lines+markers",
                            marker = list(
                              color = ~alert_percent,
                              size=25/log(dim(Reactive_dfs$df_1)[1])),
                            line = list(color="gray", width=12/log(dim(Reactive_dfs$df_1)[1])),
                            hoverinfo = "text",
                            text = ~ paste(
                              "<br>Date:</b>", date,
                              "<br>%:</b>", format(percent, big.mark = ",")
                            ),
                            name = "% CC & DD", legendgroup="1st")
    
    alert_plot <- plot_ly(data = Reactive_dfs$df_1, source='plotlyts', x = ~date, y = ~count, type = 'scatter', mode = 'lines+markers',
                          marker = list(color = ~alert_alert, size=25/log(dim(Reactive_dfs$df_1)[1])),
                          line = list(color = "rgb(22, 96, 167)", width = 12/log(dim(Reactive_dfs$df_1)[1])),
                          hoverinfo = "text",
                          text = ~ paste(
                            "<br>Date:</b>", date,
                            "<br>Count:</b>", format(count, big.mark = ",")
                          ),
                          name = "Count of Alerting Regions", legendgroup="2nd")
    
    trending_plot <- plot_ly(data = Reactive_dfs$df_1, source='plotlyts', x = ~date) %>%
      add_trace(y = ~fitted, type = 'scatter', mode = 'lines',
                line = list(color = "rgb(60, 0, 155)", width = 12/log(dim(Reactive_dfs$df_1)[1])),
                hoverinfo = "text",
                text = ~ paste(
                  "<br>Date:</b>", date,
                  "<br>fitted:</b>", format(fitted, big.mark = ",")
                ),
                name = "GAM-estimated trend", legendgroup="3rd"
      ) %>%
      add_trace(y = ~n_increasing, type = 'scatter', mode = 'markers',
                marker = list(color = ~alert_trend, size = 25/log(dim(Reactive_dfs$df_1)[1]), opacity = 0.5),
                hoverinfo = "text",
                text = ~ paste(
                  "<br>Date:</b>", date,
                  "<br># Increasing:</b>", format(n_increasing, big.mark = ",")
                ),
                name = "Count of Regions with Increasing Trend", legendgroup="3rd")
    
    # subplots object
    plt <- subplot(percent_plot, alert_plot, trending_plot, nrows = 3, shareX = TRUE) %>%
      layout(
        title = list(text = paste0(selected$state,': ', selected$CCDD), x = 0.4),
        hovermode = "x unified",
        xaxis = list(
          title = "<b>Date<b>",
          showspikes = TRUE,
          spikemode = "across",
          ticks = "outside",
          spikedash = "dot",
          spikecolor = "black",
          spikethickness = -2,
          tickformat="%Y-%m-%d",
          ticklabelmode="period",
          tickangle = 0
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
          title = list(text = "<b>Increasing<br>Trend<b>",
                       font = list(
                         color = "rgb(60, 0, 155)")),
          tickfont = list(color = "rgb(60, 0, 155)"),
          showline = TRUE,
          showgrid = TRUE,
          rangemode = "tozero",
          ticks = "outside"
        ),
        shapes = list(
          type = "line",
          x0 = selected$maps_date,
          x1 = selected$maps_date,
          y0 = 0,
          y1 = 1,
          yref = 'paper',
          line = list(color = "black", dash='dash', width = 2)
        ), 
        annotations = annotations,
        legend = list(tracegroupgap = 93)
      ) %>% 
      event_register(.,'plotly_click')
    
    plt %>%
      config(modeBarButtons = list(list("toImage"), list("autoScale2d")))
  })
  
  observe({
    plotly_dims$width <- session$clientData$output_tsPlotly_width
    plotly_dims$height <- session$clientData$output_tsPlotly_height
  })
  
  ##------------Plot Leaflet Choropleths---------------------------
  
  leaflet_p_choropleth = reactive({
    labels_p <- sprintf(
      "<strong>County: </strong>%s<br/>
        <strong>P-value: </strong>%s<br/>
        <strong>Percent: </strong>%s<br/>",
      selected_state$df_sf$NAME,
      selected_state$df_sf$p,
      selected_state$df_sf$percent
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
      leaflet.extras::addFullscreenControl() %>%  # Add fullscreen control
      leaflet.extras::addResetMapButton() %>%  # Add reset button
      addPolylines(
        data = selected_state$state_sf,
        opacity = 1,
        fillOpacity = 0,
        color = "black", 
        weight = 1.2
      ) %>%
      addPolygons(
        data = selected_state$df_sf,
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
      )
  })
  
  leaflet_alerts_choropleth = reactive({
    # leaflet alerts plot
    labels_alerts <- sprintf(
      "<strong>County: </strong>%s<br/>
         <strong>Alert: </strong>%s<br/>
         <strong>p-value: </strong>%s<br/>", 
      selected_state$df_sf$NAME,
      selected_state$df_sf$alert_percent,
      selected_state$df_sf$p
    ) %>%
      lapply(htmltools::HTML)
    
    pal_alerts <- colorFactor(
      palette = c('blue','yellow','red'), 
      levels = c('None', 'Warning', 'Alert'),
      na.color = "black")
    
    alert_leaf <- leaflet() %>%
      leaflet.extras::setMapWidgetStyle(list(background = "#FFFFFF")) %>%
      leaflet.extras::addFullscreenControl() %>%  # Add fullscreen control
      leaflet.extras::addResetMapButton() %>%  # Add reset button
      addPolylines(
        data = selected_state$state_sf,
        opacity = 1,
        fillOpacity = 0,
        color = "black", 
        weight = 1.2
      ) %>%
      addPolygons(
        data = selected_state$df_sf,
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
        values = selected_state$df_sf$alert_percent,
        fillOpacity = 0.5
      )
  })
  
  leaflet_increasing_choropleth = reactive({
    labels_inc <- sprintf(
      "<strong>County: </strong>%s<br/>
        <strong>Slope: </strong>%s<br/>",
      selected_state$df_sf$NAME,
      round(selected_state$df_sf$deriv, digits = 3)
    ) %>%
      lapply(htmltools::HTML)
    
    pal_inc <- colorFactor(
      palette = c('blue','yellow','red','lightgray'),
      levels = c('Decreasing', 'Stable', 'Increasing', 'Sparse'),
      na.color = "black")
    
    inc_leaf <-
      leaflet() %>%
      leaflet.extras::setMapWidgetStyle(list(background = "#FFFFFF")) %>%
      leaflet.extras::addFullscreenControl() %>%  # Add fullscreen control
      leaflet.extras::addResetMapButton() %>%  # Add reset button
      addPolylines(
        data = selected_state$state_sf,
        opacity = 1,
        fillOpacity = 0,
        color = "black", 
        weight = 1.2
      ) %>%
      addPolygons(
        data = selected_state$df_sf,
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
        values = selected_state$df_sf$trajectory,
        fillOpacity = 0.5
      )
  })
  
  # Alerts time series Plotly time series
  output$tsPlotly <- renderPlotly({
    if (!is.null(Reactive_dfs$df_1)) {
      plotly_plot()
    }
  })
  
  # P-value choropleth map
  output$p_choropleth <- renderLeaflet({
    if (!is.null(selected_state$df_sf)) {
      leaflet_p_choropleth()
    }
  })
  
  # Alerts choropleth map
  output$alerts_choropleth <- renderLeaflet({
    if (!is.null(selected_state$df_sf)) {
      leaflet_alerts_choropleth()
    }
  })
  
  # Smoothed slope bins choropleth map
  output$increasing_choropleth <- renderLeaflet({
    if (!is.null(selected_state$df_sf)) {
      leaflet_increasing_choropleth()
    }
  })
  
  observeEvent(input$go, {
    
    dfs <- get_and_mutate_dfs(input)
    
    # Update the reactive values
    Reactive_dfs$df_1 <- dfs[[1]]
    Reactive_dfs$df_2 <- dfs[[2]]
    
    # Assign selected reactives for plotly title
    selected$state = input$State
    selected$CCDD = input$CCDD
    
    # Get selected state geometry
    if (input$State == 'All'){
      selected_state$fp = state_helper$state_fips
      selected_state$state_sf = state_sf
      selected_state$county_sf = county_sf
    } else {
      selected_state$fp = state_helper[state_helper$state_name == input$State,]$state_fips
      selected_state$state_sf = state_sf[state_sf$STATEFP == selected_state$fp,]
      selected_state$county_sf = county_sf[county_sf$STATEFP == selected_state$fp,]
    }
    selected$startDate = as.character(input$StartDate)
    selected$endDate = as.character(input$EndDate)
    selected$maps_date = as.character(input$EndDate)
    
    compute_and_assign_mapping_output_reactives()
  })
  
  observeEvent(event_data("plotly_click", source = 'plotlyts'), {
    selected$maps_date <- event_data("plotly_click", source = 'plotlyts')$x[1]
    compute_and_assign_mapping_output_reactives()
  })
  
  #------------------ADD COUNTY-WISE TIME SERIES POPUP MODAL-----------------#
  
  # Reactive element to plot time series from p_choropleth on click
  observeEvent(input$p_choropleth_shape_click, {
    p_choropleth_click <- input$p_choropleth_shape_click
    selected_county <- p_choropleth_click$id
    if (input$State == "All") {
      state = substr(selected_county, nchar(selected_county)-2+1, nchar(selected_county))
      selected_county = substr(selected_county, 1, nchar(selected_county) - 4)
    }
    
    time_series_data <- st_as_sf(right_join(Reactive_dfs$df_2, selected_state$county_sf[,c("NAME","GEOID")], by = c("fips" = "GEOID"))) %>%
      select(state_abbr,
             fips,
             NAME,
             date,
             percent,
             color) %>%
      as.data.frame() %>%
      mutate(NAME = as.character(NAME))
    
    df_temp = time_series_data[time_series_data$NAME == selected_county,]
    if (input$State == "All") {
      df_temp = df_temp[df_temp$state_abbr == state,]
    }
    
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
      
      df_temp_before = df_temp %>% filter(date <= selected$maps_date)
      df_temp_after = df_temp %>% filter(date >= selected$maps_date)
      
      plot_ly() %>%
        add_trace(data=df_temp_before, x=~date, y=~percent, type='scatter', mode='lines+markers',
                  marker = list(color = ~color),
                  hoverinfo = "text",
                  name = paste0("Before ", selected$maps_date),
                  text = ~ paste(
                    "<br>Date:</b>", date,
                    "<br>%:</b>", format(percent, big.mark = ",")
                  )) %>%
        add_trace(data=df_temp_after, x=~date, y=~percent, type='scatter', mode='lines+markers',
                  line = list(color = 'gray', dash = 'dot'),
                  marker = list(color = ~color),
                  hoverinfo = "text",
                  name = paste0("After ", selected$maps_date),
                  text = ~ paste(
                    "<br>Date:</b>", date,
                    "<br>%:</b>", format(percent, big.mark = ",")
                  )) %>%
        layout(shapes = list(
          type = "line",
          x0 = selected$maps_date,
          x1 = selected$maps_date,
          y0 = min(df_temp$percent),
          y1 = max(df_temp$percent),
          line = list(color = "black", dash="dash", width = 2)
        ),
        title=paste0("<b>", selected_county, ": ", input$CCDD,"<b>"),
        yaxis = list(title=paste0("<b>Percent (%)<b>")),
        xaxis = list(title=paste0("<b>Date<b>"))) %>%
        config(modeBarButtons = list(list("toImage"), list("autoScale2d")))
    })
  })
  
  # Reactive element to plot time series from alerts_choropleth on click
  observeEvent(input$alerts_choropleth_shape_click, {
    alerts_choropleth_click <- input$alerts_choropleth_shape_click
    selected_county <- alerts_choropleth_click$id
    if (input$State == "All") {
      state = substr(selected_county, nchar(selected_county)-2+1, nchar(selected_county))
      selected_county = substr(selected_county, 1, nchar(selected_county) - 4)
    }
    
    time_series_data <- st_as_sf(right_join(Reactive_dfs$df_2, selected_state$county_sf[,c("NAME","GEOID")], by = c("fips" = "GEOID"))) %>%
      select(state_abbr,
             fips,
             NAME,
             date,
             percent,
             color) %>%
      as.data.frame() %>%
      mutate(NAME = as.character(NAME))
    
    df_temp = time_series_data[time_series_data$NAME == selected_county,]
    if (input$State == "All") {
      df_temp = df_temp[df_temp$state_abbr == state,]
    }
    
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
      
      df_temp_before = df_temp %>% filter(date <= selected$maps_date)
      df_temp_after = df_temp %>% filter(date >= selected$maps_date)
      
      plot_ly() %>%
        add_trace(data=df_temp_before, x=~date, y=~percent, type='scatter', mode='lines+markers',
                  marker = list(color = ~color),
                  hoverinfo = "text",
                  name = paste0("Before ", selected$maps_date),
                  text = ~ paste(
                    "<br>Date:</b>", date,
                    "<br>%:</b>", format(percent, big.mark = ",")
                  )) %>%
        add_trace(data=df_temp_after, x=~date, y=~percent, type='scatter', mode='lines+markers',
                  line = list(color = 'gray', dash = 'dot'),
                  marker = list(color = ~color),
                  hoverinfo = "text",
                  name = paste0("After ", selected$maps_date),
                  text = ~ paste(
                    "<br>Date:</b>", date,
                    "<br>%:</b>", format(percent, big.mark = ",")
                  )) %>%
        layout(shapes = list(
          type = "line",
          x0 = selected$maps_date,
          x1 = selected$maps_date,
          y0 = min(df_temp$percent),
          y1 = max(df_temp$percent),
          line = list(color = "black", dash="dash", width = 2)
        ),
        title=paste0("<b>", selected_county, ": ", input$CCDD,"<b>"),
        yaxis = list(title=paste0("<b>Percent (%)<b>")),
        xaxis = list(title=paste0("<b>Date<b>"))) %>%
        config(modeBarButtons = list(list("toImage"), list("autoScale2d")))
    })
  })
  
  # Reactive element to plot time series from increasing_choropleth on click
  observeEvent(input$increasing_choropleth_shape_click, {
    increasing_choropleth_click <- input$increasing_choropleth_shape_click
    selected_county <- increasing_choropleth_click$id
    if (input$State == "All") {
      state = substr(selected_county, nchar(selected_county)-2+1, nchar(selected_county))
      selected_county = substr(selected_county, 1, nchar(selected_county) - 4)
    }
    
    time_series_data <- st_as_sf(right_join(Reactive_dfs$df_2, selected_state$county_sf[,c("NAME","GEOID")], by = c("fips" = "GEOID"))) %>%
      select(state_abbr,
             fips,
             NAME,
             date,
             percent,
             fitted,
             deriv,
             color) %>%
      as.data.frame() %>%
      mutate(NAME = as.character(NAME))
    
    df_temp = time_series_data[time_series_data$NAME == selected_county,]
    if (input$State == "All") {
      df_temp = df_temp[df_temp$state_abbr == state,]
    }
    
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
      
      df_temp_before = df_temp %>% filter(date <= selected$maps_date)
      df_temp_after = df_temp %>% filter(date >= selected$maps_date)
      df_gam = subset(df_temp_before, df_temp_before$date <= as.Date(selected$maps_date))
      
      plot_ly() %>%
        add_trace(data=df_temp_before, x=~date, y=~percent, type='scatter', mode='lines+markers',
                  marker = list(color = ~color),
                  hoverinfo = "text",
                  name = paste0("Before ", selected$maps_date),
                  text = ~ paste(
                    "<br>Date:</b>", date,
                    "<br>%:</b>", format(percent, big.mark = ",")
                  )) %>%
        add_trace(data=df_temp_after, x=~date, y=~percent, type='scatter', mode='lines+markers',
                  line = list(color = 'gray', dash = 'dot'),
                  marker = list(color = ~color),
                  hoverinfo = "text",
                  name = paste0("After ", selected$maps_date),
                  text = ~ paste(
                    "<br>Date:</b>", date,
                    "<br>%:</b>", format(percent, big.mark = ",")
                  )) %>%
        add_trace(data=df_gam, x=~date, y=~fitted, type='scatter', mode='lines',
                  line = list(color = "rgb(60, 0, 155)", width=5),
                  #marker = list(color = ~color),
                  hoverinfo = "text",
                  name = paste0("GAM-based trend estimate "),
                  text = ~ paste(
                    "<br>Date:</b>", date,
                    "<br>%:</b>", format(fitted, big.mark = ","),
                    "<br>slope:</b>", format(deriv, big.mark = ",")
                  )) %>%
        layout(shapes = list(
          type = "line",
          x0 = selected$maps_date,
          x1 = selected$maps_date,
          y0 = min(df_temp$percent),
          y1 = max(df_temp$percent),
          line = list(color = "black", dash="dash", width = 2)
        ),
        title=paste0("<b>", selected_county, ": ", input$CCDD,"<b>"),
        yaxis = list(title=paste0("<b>Percent (%)<b>")),
        xaxis = list(title=paste0("<b>Date<b>"))) %>%
        config(modeBarButtons = list(list("toImage"), list("autoScale2d")))
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
  
  inc_progress <- function(msg = "", amnt = 0){
    shiny::incProgress(detail = msg, amount = amnt)
  }
  
  output$report <- downloadHandler(
    filename = paste0("Alerts_of_Alerts_report_", Sys.Date(), ".html"),
    content = function(file) {
      withProgress(message = "Downloading report...", value = 0, {
        temp_dir <- tempdir()
        tempReport <- file.path(temp_dir, "AoA_report.Rmd")
        file.copy("AoA_report.Rmd", tempReport, overwrite = TRUE)
        logo_file <- file.path(temp_dir, "logo.png")
        file.copy("logo.png", logo_file, overwrite = TRUE)
        params <- list(
          inc_progress = inc_progress,
          selected_state = selected$state,
          selected_ccdd = selected$CCDD,
          selected_startDate = selected$startDate,
          selected_endDate = selected$endDate,
          plotly_object = plotly_plot(),
          plotly_width = plotly_dims$width,
          plotly_height = plotly_dims$height,
          maps_date = selected$maps_date,
          leaflet_object_p = leaflet_p_choropleth(),
          leaflet_object_alerts = leaflet_alerts_choropleth(),
          leaflet_object_increasing = leaflet_increasing_choropleth(),
          globalMoran = stat_test$globalMoran,
          joincount_alert = stat_test$JoinCount_alert,
          joincount_warningoralert = stat_test$JoinCount_warning_or_alert,
          joincount_inc = stat_test$JoinCount_increasing
        )
        rmarkdown::render(tempReport, output_file = file,
                          params = params,
                          envir = new.env(parent = globalenv()))
      })
    }
  )
}

shinyApp(ui, server)