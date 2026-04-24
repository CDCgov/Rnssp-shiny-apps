# © 2025 The Johns Hopkins University Applied Physics Laboratory LLC
# Development of this software was sponsored by the U.S. Government under
# contract no. 75D30124C19958

label_list_dl <- list(
  geo_res = list( 
    l = "Geographic Resolution",
    m = "Select geographic resolution for ESSENCE query used to retrieve training data."
  ),
  date_range = list(
    l = "Date Range",
    m = "Select a start and end date (inclusive) for the ESSENCE query. For weekly queries, start dates should fall on a Sunday and end dates should fall on a Saturday to avoid partial weeks."
  ),
  temporal_res = list(
    l = "Time Resolution",
    m = "Select a temporal resolution for the ESSENCE query."
  ),
  target_type = list(
    l = "Select Type",
    m = "Select diagnostic criteria type (CCDD, syndrome or subsyndrome) for filtering records in the ESSENCE query."
  ),
  target_code = list(
    l = "Target Outcome",
    m = "Select the specific diagnostic category or code to use when filtering records in the ESSENCE query.")
)

button_list_dl <-list(
  run_query = "Submit ESSENCE query to load data.",
  load_query = "Load a saved query from file.",
  query_essence = "Generate a query to download data from ESSENCE.",
  select_saved = paste0(
    "Open file browser to select a saved query on your local machine. ",
    "Saved queries are zip files containing json and rds objects with the ",
    "file suffix .bsm_query."),
  clear_filters = "Reset all table filters and search text.",
  download_csv = "Download retrieved data and save as a csv file on your local machine.",
  save_query = "Save the query to a bsm_query file so that it can be reloaded later.",
  add_covariates = "Add covariates by loading another data file. These covariates can be included in the model later."
)

data_loader_ui <- function(id) {

  ns <- NS(id)
  
  ########################
  # Input Widgets
  ########################
  
  # syndrome selection accordion panel
  synd_panel <- create_syndrome_inputs(ns=ns, cats = NULL)
  
  # geographic resolution: zip or county
  geo = selectInput(
    ns("geo_res"),
    choices=c("County" = "county"), # "Zip Code" = "zip", 
    selected="county",
    label=labeltt(label_list_dl[["geo_res"]])
  )
  
  # state selection - use module - call ui
  states = state_selector_ui(ns("state_selector"))
  # small ui to render with warnings/invalid messages
  county_validation <- uiOutput(ns("county_validation"))
  
  # date range
  offset = (as.POSIXlt(Sys.Date())$wday -6)%%7
  end = Sys.Date()-offset
  drange = dateRangeInput(
    ns("drange"),
    label=labeltt(label_list_dl[["date_range"]]),
    start = end - 60,
    end = end
  )
  
  # time resolution
  time_res <- selectInput(
    ns("time_res"),
    label=labeltt(label_list_dl[["temporal_res"]]),
    choices = c("Weekly" = "weekly", "Daily" = "daily"), #"Monthly" = "monthly"),
    selected = "weekly"
  )
  
  
  ########################
  # Nav Panel to Return
  ########################
  nav_panel(
    title = "Data Loader",
    layout_sidebar(
      sidebar = sidebar(
        id = ns('data_loader_sidebar'),
        open = FALSE,
        navset_tab(
          nav_panel(
            title = tagList(
              "Query ESSENCE",
              labeltt(list("", button_list_dl["query_essence"]))
            ),
            br(),
            id = ns("config_sidebar"),
            width = SIDEBAR_WIDTH*2,
            geo, 
            states,
            county_validation,
            drange,
            time_res,
            synd_panel,
            add_button_hover(
              title = button_list_dl[["run_query"]],
              hidden(input_task_button(ns("load_data_btn"), "Query ESSENCE"))
            )
          ),
          nav_panel(
            title = tagList(
              "Load Saved Query",
              labeltt(list("", button_list_dl["load_query"]))
            ),
            
            br(),
            add_button_hover(
              title = button_list_dl[["select_saved"]], 
              fileInput(ns("zipfile"), "Select Saved Query", accept = ".bsm_query")
            )
          )
      ),
      width = 450
      ),
      card(
        card_header("Data", class="card-header-accent"),
        card_body(
          uiOutput(ns("data_card_body"))
        ),
        card_footer(
          uiOutput(ns("download_ui"))
        )
      )
    )
  )

}



data_loader_server <- function(id, dc, results, profile, valid_profile, cache_transitions) {
  moduleServer(
    id,
    function(input, output, session) {
      ns <- session$ns
      data <- reactiveVal(NULL)
      data_state <- reactiveVal("idle") 
      # observe for transition changes
      observe(update_cache_data_loader(session, cache_transitions)) |>
        bindEvent(reactiveValuesToList(cache_transitions))
      
      # Initiate the adjacency_matrix object in the dc reactives
      dc$physical_adj <- load_adj_matrix(PHYS_ADJ_MATRIX)
      dc$mobility_adj <- load_adj_matrix(MOB_ADJ_MATRIX)
      
      # Call the state selector server
      state_selector_server("state_selector", dc, cache_transitions)
      
      # Render the validation on the selected counties
      output$county_validation <- renderUI({
        validate_county_selection(
          dc$states, dc$selected_counties, dc$physical_adj
        )
      })
      
      # Monitor to fill global reactives
      observe(results$data <- data()$data)
      observe(dc$time_res <- input$time_res)
      observe(dc$geo_res <- input$geo_res)
      observe(dc$drange <- input$drange)
      observe(dc$synd_cat <- input$synd_cat)
      observe(dc$synd_drop_menu <- input$synd_drop_menu)
      
      cat_values <- reactive(get_categorical_values(profile()))
      
      observe({
        toggle("load_data_btn", condition=req(cat_values()))
        toggle_sidebar(id = "data_loader_sidebar", open=TRUE)
      }) |> bindEvent(cat_values())
      
      observe({
        if (is.null(data()$data)) {
          shinyjs::disable("cov_button_ui")
        } else {
          shinyjs::enable("cov_button_ui")
        }
      })
      
      # Update the choices for syndromic categories
      observe({
        
        req(cat_values())
        req(input$synd_cat)
        cat_values <- cat_values()
        # get the set of choices
        sc = list(ccdd=cat_values$ccdd_cats,
                  synd=cat_values$syndromes,
                  subsynd=cat_values$subsyndromes)[[input$synd_cat]]
        if (!is.null(cache_transitions$synd_drop_menu) && cache_transitions$synd_drop_menu %in% sc) {
          selected <- cache_transitions$synd_drop_menu
        } else if (input$synd_cat == "ccdd") {
          selected <- "CDC COVID-Specific DD v1"
        } else {
          selected <- NULL
        }

        updateSelectInput(
          session = session,
          inputId = "synd_drop_menu",
          choices = sc,
          selected = selected
        )
      })
      
      query_data <- reactive({
        # stop if not profile
        req(profile())
      
        # stop if no counties have been selected
        req(!is.null(dc$selected_counties), length(dc$selected_counties) > 0)
        
        # stop if no states have been selected
        req(!is.null(dc$states), length(dc$states) > 0)
        
        data_state("loading")
        on.exit(data_state("idle"),add=TRUE)
        
        # --------------------------
        # Syndromic categories
        # --------------------------
        synd_bits <- list(
          "ccdd" = c("mgs" = "chiefcomplaintsubsyndromes", "cat" = "ccddCategory"),
          "synd" = c("mgs" = "essencesyndromes", "cat" = "medicalGrouping"),
          "subsynd" = c("mgs" = "chiefcomplaintsubsyndromes", "cat" = "medicalGrouping")
        )
        
        med_group_sys = synd_bits[[input$synd_cat]]["mgs"]
        categ_info = list(cat_class = synd_bits[[input$synd_cat]]["cat"],
                          cat_value = xml2::url_escape(tolower(input$synd_drop_menu)))
        
        # if url hasn't changed, no need to repull
        url_single <- make_table_builder_url(
          start_date=input$drange[1],
          end_date=input$drange[2],
          time_resolution=input$time_res,
          geo_resolution=input$geo_res,
          state_filter=dc$states,
          county_filter=dc$selected_counties,
          med_group_sys = med_group_sys,
          categ_info = categ_info
        )
        
        # Only pull data if the proposed url differs from an existing one
        if(is.null(data()$url_single) || url_single != data()$url_single) {
          api_data<- withProgress(
            message = "Pulling data via API",
            detail = "Please wait ...", 
            value = 0.2,
            get_data(
              sd=input$drange[1],
              ed=input$drange[2],
              time_res=input$time_res,
              geo_res=input$geo_res,
              state_filter=dc$states,
              county_filter=dc$selected_counties,
              med_group_sys = med_group_sys,
              categ_info = categ_info, 
              profile = profile()
            )
          )
          if (input$time_res=="weekly"){
            api_data$data <- wk_to_date(api_data$data, "date")
          } else if (input$time_res=="daily"){
            api_data$data$date<-as.Date(api_data$data$date)
          }
          
        } else {
          api_data <- data()
        }
        
        
        api_data
        
      }) |> bindEvent(input$load_data_btn)
      
      
      # set up a reactive to hold user-recalled data (previously saved)
      loaded_data <- reactiveVal(NULL)
      
      observe({
        req(input$zipfile)
        validate(need(file.exists(input$zipfile$datapath), "Upload did not complete yet"))
        
        data_state("loading")
        on.exit(data_state("idle"), add=TRUE)
        
        saved_query_info <- withProgress(
          message =  "Loading saved query", 
          detail = "Please wait ...",
          value = 0.2,
          load_saved_query_file(input$zipfile$datapath)
        )
        
        vals <- saved_query_info[["query_values"]]
        for (nm in names(vals)) {
          if (nm %in% names(cache_transitions)) {
            cache_transitions[[nm]] <- vals[[nm]]
          }
        }
        cache_transitions[["synd_drop_menu"]] <- vals$synd_val
        loaded_data(list(data = saved_query_info[["data"]]))
      }) |> bindEvent(input$zipfile)
      
      
      observe(data(query_data())) |> bindEvent(query_data())
      observe(data(loaded_data())) |> bindEvent(loaded_data())  
      
      cov <- add_covariate_loader(input, 
                                  output, 
                                  session, 
                                  get_base_dt = function() data()$data,
                                  set_base_dt = function(dt) {
                                    x <- data()
                                    x$data <- dt
                                    data(x)
                                  },
                                  button_id = "cov_button_ui",
                                  button_label = "Add Covariates"
                                  )

      output$download_ui <- renderUI({
        req(!is.null(data()))
        tagList(
          layout_column_wrap(
            add_button_hover(title = button_list_dl[["add_covariates"]],
                input_task_button(ns("cov_button_ui"), "Add Covariates")),
            add_button_hover(title = button_list_dl[["download_csv"]],
                actionButton(
                  ns("download_data"),
                  "Download to CSV",
                  class = "btn-primary",
                  onclick = sprintf(
                    "if (window.Reactable) Reactable.downloadDataCSV('%s', 'data.csv');",
                    ns("ingested_data")
                  )
                )),
            add_button_hover(title = button_list_dl[["save_query"]], 
                downloadButton(ns("save_query"),
                           "Save Query",
                           class = "btn-primary"))
          )
        )
      })
      
      
      output$save_query <- downloadHandler(
        filename = function() {
          paste0("query-", Sys.Date(), ".bsm_query")
        },
        content = function(file) {
          # describe saved query
          vals <- list(
            time_res    = input$time_res,
            drange = input$drange,
            geo_res = input$geo_res,
            states    = dc$states,
            selected_counties = dc$selected_counties,
            synd_cat = input$synd_cat,
            synd_val = input$synd_drop_menu
          )
          
          json_name <- tempfile(fileext = ".json")
          rds_name  <- tempfile(fileext = ".rds")
          jsonlite::write_json(vals, json_name, pretty = TRUE, auto_unbox = TRUE)
          saveRDS(data()$data, rds_name)
          zip::zipr(file, files = c(rds_name,json_name))
        },
        contentType = "application/zip"
        ) 
      
      
      output$ingested_data <- reactable::renderReactable({
        req(data()$data)
        d <- data()$data[, .SD, .SDcols = patterns("^(?!countyfips).*$", perl=T)]
        build_standard_reactable(
          d,
          table_id = session$ns("ingested_data"),
          digits = 4,
          searchable = FALSE
        )
      })
      
      observe({
        session$sendCustomMessage(
          "set-reactable-search",
          list(
            id = session$ns("ingested_data"),
            value = input$ingested_data_search %||% ""
          )
        )
      }) |> bindEvent(input$ingested_data_search, ignoreInit = FALSE)
      
      observe({
        session$sendCustomMessage(
          "clear-reactable-filters",
          list(id = session$ns("ingested_data"))
        )
        updateTextInput(session, "ingested_data_search", value = "")
      }) |> bindEvent(input$clear_filters, ignoreInit = TRUE)
      
      
      output$data_card_body <- renderUI({
        if (identical(data_state(), "loading") && (is.null(data()) || is.null(data()$data))) {
          tags$div(
            style = "min-height: 250px;"
          )
        } else if (is.null(data()) || is.null(data()$data)) {
          tagList(
            h4("How to load data"),
            tags$ol(
              tags$li("Either create a new query or load a save query from file."),
              tags$li("When loading completes, the table will appear here.")
            ),
            tags$p(class = "text-muted", "Nothing has been loaded yet.")
          )
        } else {
          tagList(
            div(
              class = "reactable-top-controls",
              add_button_hover(
                title = button_list_dl[["clear_filters"]],
                actionButton(ns("clear_filters"), "Clear Filters", class = "btn-primary")
              ),
              div(
                class = "reactable-search-wrap",
                tags$label(`for` = ns("ingested_data_search"), "Search"),
                textInput(ns("ingested_data_search"), NULL, placeholder = "Search displayed data")
              )
            ),
            reactable::reactableOutput(ns("ingested_data"), width = "100%")
          )
        }
      })
      
    }
  )
}

### data loader module helper functions ###
create_syndrome_inputs <- function(ns, cats) {
  tagList(
    radioButtons(
      inputId = ns("synd_cat"),
      label = labeltt(label_list_dl[["target_type"]]),
      choices = c(
        "Chief Complaint and Discharge Diagnosis Category" = "ccdd",
        "Syndrome" = "synd",
        "Sub-Syndrome" = "subsynd"
      )
    ),
  
    selectInput(
      inputId = ns("synd_drop_menu"),
      label = labeltt(label_list_dl[["target_code"]]),
      choices = cats
    )
  )
}

# Helper function to validate and return message
validate_county_selection <- function(states, ctys, mat) {
  msg = character(0)
  if(is.null(ctys) || length(ctys)<=1) {
    if(!is.null(states)) {
      msg = "Select counties manually"
    } else msg = "Please select states/counties"
  } else {
    n = length(ctys)
    if(n>300) {
      msg = c(msg, "Warning: more than 300 counties selected")
    }
    if(!selection_is_connected(ctys, mat)) {
      msg = c(msg, "Warning: selected counties are not connected")
    }
  }
  
  if(length(msg)>0) cl = "shiny-output-error-validation"
  else {
    msg = paste0(length(ctys), " connected counties selected")
    cl = "p-2"
  }
  
  return(
    div(
      class = cl,
      htmltools::HTML(
        paste(msg, collapse = "<br>")
      )
    )
  )
  
}

# Small data loader helper function that will update the widgets
# if the cache transitions reactives have been updated, for example
# when loaded a previously saved model.
update_cache_data_loader <- function(session, cache_transitions) {
  
  updateSelectInput(session=session, "time_res", selected=cache_transitions$time_res)
  updateSelectInput(session=session, "geo_res", selected=cache_transitions$geo_res)
  updateDateRangeInput(session=session, "drange", start = cache_transitions$drange[1],end = cache_transitions$drange[2])
  updateRadioButtons(session=session, "synd_cat", selected = cache_transitions$synd_cat)
  updateSelectInput(session=session, "synd_drop_menu", selected = cache_transitions$synd_drop_menu)
}




