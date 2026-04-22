# © 2025 The Johns Hopkins University Applied Physics Laboratory LLC
# Development of this software was sponsored by the U.S. Government under
# contract no. 75D30124C19958
label_list_im <- list(
  num_forecasts_day = list(
    l = "Number of forecasts (days)",
    m = "Select the number of future days where estimates will be generated."
  ),
  num_forecasts_week = list(
    l = "Number of forecasts (weeks)",
    m = "Select the number of future weeks where estimates will be generated."
  ),
  num_forecasts_month = list(
    l = "Number of forecasts (months)",
    m = "Select the number of future months where estimates will be generated."
  ),
  num_forecasts_year = list(
    l = "Number of forecasts (years)",
    m = "Select the number of future years where estimates will be generated."
  ),
  distribution = list(
    l = "Distributional Family",
    m = "Select the probability distribution family used to model the observed counts. Poisson and Negative Binomial are recommended for modeling counts. Binomial and Beta Binomial are recommended for modeling proportions."
  ),
  model_spec = list(
    l = "Model Specification",
    m = "Select whether to use the default model or to customize model components (advanced users only)."
  ),
  formula_spec = list(
    l = "Enter Model Formula",
    m = "Enter the formula for the model you would like to fit. The formula should start with 'target ~' and should can reference the available features listed in the box above."
  )
)
button_list_im <-list(
  fit_new_model = "Define a new model to fit to the data.",
  run_model = "Fit selected INLA model to data.",
  load_saved_model = "Load a saved model from file.",
  select_saved = paste0(
    "Open file browser to select a saved model on your local machine. ",
    "Saved models are zip files containing json and rds objects with the ",
      "file suffix .bsm_model."),
  download_csv = "Download processed data and save as a csv file on your local machine.",
  save_model = "Save the query to a bsm_model file so that it can be reloaded later.",
  actual_formula = "Show formula as parsed during model fitting."
)

inla_model_ui <- function(id) {
  
  ns <- NS(id)
  
  ########################
  # Input Widgets
  ########################
  
  # Number of forecasts
  forecasts = uiOutput(ns("nforecasts_ui"))
  
  # small ui to render with warnings/invalid messages for model run button
  model_run_validator <- uiOutput(ns("model_run_validator"))
  
  family = selectInput(
    ns("dist_family"),
    label=labeltt(label_list_im[["distribution"]]),
    choices = c(
      "Poisson" = "poisson",
      "Negative Binomial"="nbinomial",
      "Binomial" = "binomial",
      "Beta-Binomial" = "betabinomial"
    ),
    selected = "binomial"
  )
  
  region_re_options <- tagList(
    div(
      class = "well",
      HTML("Precision Hyper-parameters:"),
      numericInput(ns("rre_prec_pc_param"), "PC Prior Sigma Threshold", value=.2, step = .001),
      sliderInput(ns("rre_prec_pc_alpha"), "PC Prior Probability", value=0.01, min=1e-5, max = 1)
    )
  )
  
  spatial_component_options <- tagList(
    radioButtons(
      ns("sco_adjacency_type"), "Neighborhood Basis",
      choices=c(
        "Mobility" = "mobility_adj_mat",
        "Proximity" = "physical_adj_mat"
      ),
      inline=TRUE
    ),
    selectInput(
      ns("sco_model_type"), "Spatial Model",
      choices = c(
        "Proper Besag Model" = "besagproper",
        "Besag Area Model" = "besag",
        "Besag-York-Mollier" = "bym"
      )
    ),
    selectInput(
      ns("sco_control_group_model"), "Temporal Model",
      choices = c(
        "Autoregressive" = "ar",
        "Random Walk (Order 2)" = "rw2",
        "Random Walk (Order 1)" = "rw1"
      )
    ),
    conditionalPanel(
      condition = "input.sco_control_group_model == 'ar'",
      numericInput(ns("sco_control_group_ar_order"), "Order", value = 1, min=1, max=5), 
      ns=ns
    ),
    div(
      class = "well",
      HTML("Precision Hyper-parameters:"),
      numericInput(ns("sco_prec_pc_param"), "PC Prior Sigma Threshold", value=.2, step = .001),
      sliderInput(ns("sco_prec_pc_alpha"), "PC Prior Probability", value=0.01, min=1e-5, max = 1)
    )
  )
  
  weekly_temporal_component_options <- tagList(
    hidden(checkboxInput(ns("weekly_tco_chkbx"), "Weekly Component",value = TRUE)),
    conditionalPanel(
      condition = "input.weekly_tco_chkbx",
      selectInput(
        ns("tco_model"), "Model (Weekly)",
        choices = c(
          "Random Walk (Order 2)-Cyclical" = "rw2",
          "Random Walk (Order 1)-Cyclical" = "rw1",
          "Autoregressive-Cyclical" = "ar1",
          "Autoregressive - Temporal" = "ar"
        ),
        selected = "rw2"
      ),
      conditionalPanel(
        condition = "input.tco_model == 'ar'",
        numericInput(ns("tco_model_ar_order"), "Order", value = 1, min=1, max=5), 
        ns=ns
      ),
      ns=ns
    )
  )
  
  daily_temporal_component_options <- tagList(
    checkboxInput(ns("daily_tco_chkbx"), "Daily Component",value = TRUE),
    conditionalPanel(
      condition = "input.daily_tco_chkbx",
      selectInput(
        ns("tco_model_d"), "Model (Daily)",
        choices = c(
          "Random Walk (Order 2)-Cyclical" = "rw2",
          "Random Walk (Order 1)-Cyclical" = "rw1",
          "Autoregressive-Cyclical" = "ar1",
          "Autoregressive - Temporal" = "ar"
        ),
        selected = "rw2"
      ),
      conditionalPanel(
        condition = "input.tco_model_d == 'ar'",
        numericInput(ns("tco_model_ar_order_d"), "Order", value = 1, min=1, max=5), 
        ns=ns
      ),
      ns=ns
    )
  )
  
  # Custom model panel
  model_component_custom_panel = tagList(
    div(
      class="well", 
      layout_columns(
        checkboxInput(ns("rre_component_chkbx"), "Region Random Effect",value = TRUE),
        conditionalPanel(
          condition = "input.rre_component_chkbx",
          input_switch(ns("customize_rre"),label ="Advanced Customization",value = FALSE),  
          ns = ns
        ),
        col_widths = c(6,6)
      ),
      conditionalPanel(
        condition = "input.customize_rre & input.rre_component_chkbx",
        region_re_options,
        ns=ns
      )
    ),
    div(
      class = "well",
      layout_columns(
        checkboxInput(ns("spatial_component_chkbx"), "Spatio-Temporal Component",value = FALSE),
        conditionalPanel(
          condition = "input.spatial_component_chkbx",
          input_switch(ns("customize_spatial_component"),label ="Advanced Customization",value = FALSE),
          ns = ns
        ),
        col_widths = c(6,6)
      ),
      conditionalPanel(
        condition = "input.customize_spatial_component & input.spatial_component_chkbx",
        spatial_component_options,
        ns=ns
      )
    ),
    div(
      class="well",
      layout_columns(
        checkboxInput(ns("temporal_component_chkbx"), "Seasonal/Temporal Component",value = FALSE),
        conditionalPanel(
          condition = "input.temporal_component_chkbx",
          input_switch(ns("customize_temporal_component"),label ="Advanced Customization",value = FALSE),
          ns = ns
        ),
        col_widths = c(6,6)
      ),
      conditionalPanel(
        condition = "input.customize_temporal_component & input.temporal_component_chkbx",
        hidden(div(id = ns("daily_tco_div"), daily_temporal_component_options)),
        weekly_temporal_component_options,
        ns=ns
      )
    )
  )
  
  formula_panel = tagList(
    radioButtons(
      ns("formula_type"),
      label=labeltt(label_list_im[["model_spec"]]),
      choices = c(
        "Default Model"="default",
        "Customize Components" = "custom_components",
        "Custom Model Formula" = "custom_formula"
      ),
      selected = "default",
      inline = TRUE
    ),
    conditionalPanel(
      condition = "input.formula_type == 'custom_components'",
      model_component_custom_panel,
      ns = ns
    ),
    conditionalPanel(
      condition = "input.formula_type == 'custom_formula'",
      uiOutput(ns("available_features_box")),
      textAreaInput(ns("custom_formula"), labeltt(label_list_im[["formula_spec"]])),
      uiOutput(ns("formula_error_box")), # box to enter custom formula
      ns = ns
    ),
    conditionalPanel(
      condition = "input.custom_formula && /graph/i.test(input.custom_formula)",
      radioButtons(
        ns("sco_adjacency_type_custom"), "Adjacency Matrix",
        choices=c(
          "Mobility" = "mobility_adj_mat",
          "Proximity" = "physical_adj_mat"
        ),
        inline=TRUE
      ),
      ns=ns
    ),
    tags$details(
      tags$summary("Show Generic Formula"),
      verbatimTextOutput(ns("inla_model_formula_r")) |>
        tagAppendAttributes(style = css("white-space" = "pre-wrap"))
    ) # the above displays the formula currently selected.
  )
  
  
  ## Output cards:
  model_card <- card(
    #card_header("INLA Estimation Summary", class="bg-primary"),
    # card_body(withSpinner(
    #   verbatimTextOutput(ns("inla_model_object")),
    #   caption = "Estimating Model ... please wait",
    #   color = bs_get_variables(theme=THEME,"primary")
    # )),
    card_body(
      uiOutput(ns("model_card_body"))
    ),
    card_footer(
      layout_columns(
        uiOutput(ns("download_model_ui")),
        widths=c(6,6)
      )
    )
  )
  
  
  model_formula_card <- card(
    #card_header("Model/Formula", class="bg-primary"),
    card_body(withSpinner(
      verbatimTextOutput(ns("inla_model_formula")) |>
        tagAppendAttributes(style = css("white-space" = "pre-wrap")),
      caption = "Estimating Model ... please wait",
      color = bs_get_variables(theme=THEME,"primary")
    ))
  )
  

  
  
  ########################
  # Nav Panel to Return
  ########################
  
  nav_panel(
    title = "INLA Model",
    layout_sidebar(
      sidebar = sidebar(
        navset_tab(
          nav_panel(
            title = tagList(
              "Fit New Model",
              labeltt(list("", button_list_im["fit_new_model"]))
            ),
            br(),
            id = ns("model_sidebar"),
            width = SIDEBAR_WIDTH*2,
            forecasts,
            family,
            formula_panel,
            add_button_hover(title = button_list_im[["run_model"]],
                             input_task_button(ns("estimate_model_btn"), "Run Model")),
            # small ui to render with warnings/invalid messages
            model_run_validator
          ),
          nav_panel(
            title = tagList(
              "Load Saved Model",
              labeltt(list("", button_list_im["load_saved_model"]))
            ),
            br(),
            add_button_hover(
              title = button_list_im[["select_saved"]],
              fileInput(ns("zipfile_model"), "Select Saved Model", accept = ".bsm_model")
            )
          )
        ),
        width = 450
      ),
      card(
        card_header("Model Summary", class = "card-header-accent"),
        model_card
      )
    )
  )
}

inla_model_server <- function(id, dc, im, results, cache_transitions) {
  moduleServer(
    id,
    function(input, output, session) {
      
      model_state <- reactiveVal("idle")
      feat_store  <- init_feature_df()
      
      im$feature_store   <- feat_store
      im$feature_choices <- feat_store$choices
      im$features_df     <- feat_store$features_df
      
      current_model_res <- reactiveVal(NULL)
      
      observe({
        im$nforecasts <- input$nforecasts
      })
      
      # update global reactive model object
      observe({
        res <- current_model_res()
        req(res, isTRUE(res$ok), !is.null(res$model))
        im$model <- res$model
      })
      
      # update global reactive data_class object for current model result
      observe({
        res <- current_model_res()
        req(res, isTRUE(res$ok), !is.null(res$data_class))
        im$data_cls <- res$data_class
      })
      
      # update global reactive posteriors for current model
      observe({
        res <- current_model_res()
        req(res, isTRUE(res$ok), !is.null(res$model), !is.null(res$data_class))
        
        model_state("postprocessing")
        feat_store$register_default_calculated_features()
        
        # Calculate the builtin model-based features once at fit time so viz
        # tabs only need to read stored columns.
        calculated_feature_ids <- feat_store$rv$order[vapply(feat_store$rv$order, function(fid) {
          f <- feat_store$get_feature(fid)
          !is.null(f) && (f$feature_type %||% "") %in% c("mean", "quantile", "confidence_interval", "exceedance_probability")
        }, logical(1))]
        calculated_features <- lapply(calculated_feature_ids, feat_store$get_feature)
        
        res$data_class$data <- calculate_and_store_calculated_features(
          out = res$data_class$data,
          features = calculated_features,
          model = res$model,
          data_cls = res$data_class
        )
        res$data_class <- sort_data_class_data(res$data_class)
        
        im$data_cls <- res$data_class
        im$posterior <- res$data_class$data
        
        feat_store$sync_base_columns(
          data = res$data_class$data,
          data_cls = res$data_class,
          exclude_cols = unique(unlist(lapply(calculated_features, function(f) f$out_cols %||% character(0))))
        )
        
        model_state("ready")
      }) |> bindEvent(current_model_res())
      
      # observe the time_res and update the forecasts label and 
      observe({
        req(dc$time_res)
          lv = update_n_forecast_widget(dc$time_res)
        output$nforecasts_ui <- renderUI({
          numericInput(
            inputId =  session$ns("nforecasts"), 
            label = labeltt(label_list_im[[lv[["label"]]]]),
            value = lv[["value"]]
          )
        })
      })
      
      # hide/show the daily temporal div
      observe({
        # the daily temporal component should be visible only if daily
        toggle(id = "daily_tco_div", condition = dc$time_res == "daily")
        # the weekly temporal component checkbox should be visible only if daily
        toggle(id = "weekly_tco_chkbx", condition = dc$time_res == "daily")
        
        # the weekly temporal component checkbox should be FALSE (by default) if daily
        updateCheckboxInput(inputId = "weekly_tco_chkbx", value = dc$time_res == "weekly")
        # the daily temporal component checkbox should be FALSE (by default) if weekly
        updateCheckboxInput(inputId = "daily_tco_chkbx", value = dc$time_res != "weekly")
      }) |> bindEvent(dc$time_res)
      
      # Track formula state      
      state <- reactiveValues(valid = TRUE, errors = character(), features = c("r_id","d_id"))
      
      # Get formula from input
      formula_r <- reactive({
        req(results$data, input$nforecasts, input$formula_type)
        
        data_cls <- pre_process_data(results$data, input$nforecasts)
        num_cols <- names(data_cls$data)[vapply(data_cls$data, is.numeric, logical(1L))]
        features <- setdiff(num_cols, c("target","denominator_source"))
        
        state$features <-  unique(c("r_id","d_id",features))
        
        tmp <- get_formula(
          formula_type = input$formula_type,
          input = setNames(
            lapply(names(input), \(n) input[[n]]),
            names(input)
          ),
          dc =  data_cls,
          time_res = dc$time_res
        )
        
        state$valid  <- isTRUE(tmp$valid)
        state$errors <- if (!is.null(tmp$errors)) tmp$errors else character()
        
        tmp$formula
      })
      # Observe changes to formula to update state
      observe({ formula_r() })
      
      
      output$available_features_box <- renderUI({
        feats <- state$features
        req(length(feats) > 0)
        
        tags$div(
          style = "margin-bottom: 0.5rem;",
          tags$strong("Available features:"),
          tags$div(
            style="max-height: 140px; overflow-y: auto; border: 1px solid #ddd; padding: 0.5rem; border-radius: 4px;",
            tags$ul(
              style = "margin: 0; padding-left: 1.2rem;",
              lapply(feats, tags$li)
            )
          )
        )
      })
      
      # Disable button if formula is invalid
      observe({
        shinyjs::toggleState("estimate_model_btn", condition = isTRUE(state$valid))
      })
      
      # Parse formula for display
      output$inla_model_formula_r <- renderText({
        f <- formula_r()
        req(f)
        
        # If formula_r() returns an actual formula/expression, just deparse it
        if (!is.character(f)) {
          return(paste(deparse(f, width.cutoff = 500), collapse = "\n"))
        }
        
        # If it's an R-printed string like: " ... \"iid\" ... "
        # evaluate it as a string literal to remove the backslashes + outer quotes
        f2 <- tryCatch({
          if (grepl('^".*"$', f) && grepl('\\\\\"', f)) eval(parse(text = f)) else f
        }, error = function(e) f)
        
        # Now try to parse as an expression and re-render nicely
        expr <- tryCatch(rlang::parse_expr(f2), error = function(e) NULL)
        if (!is.null(expr)) return(rlang::expr_text(expr))
        
        # If it still can't parse, show raw
        f2
      })
      
      # Display any errors from validating formula
      output$formula_error_box <- renderUI({
        # Only show if invalid AND there are messages
        if (isTRUE(state$valid) || length(state$errors) == 0) return(NULL)
        
        div(
          class = "alert alert-danger",
          tags$strong("Formula issues:"),
          tags$ul(lapply(state$errors, tags$li))
        )
      })
      
      
      # On click of "Estimate Model", we will want to
      # 1. Pre-process transform the data
      # 2. get adjacency matrice(s)
      # 3. Create the formula 
      # 4. Run the model
      
      # Render the validation on the selected counties
      
      output$model_run_validator <- renderUI(
        model_ready_to_run(results$data, formula_r(), input)[["msg"]]
      ) |> bindEvent(input$estimate_model_btn)
      
      
      inla_model_new <- reactive({
        
        # is model ready to run?
        model_ready <- model_ready_to_run(results$data, formula_r(), input)
        model_state("fitting")
        req(model_ready[["valid"]])
        
        
        # Preprocess Data:
        data_cls <- pre_process_data(results$data, input$nforecasts)
        
        # Set adjacency matrix
        if(input$formula_type == "default") {
          adj_mat_raw <- dc$physical_adj
        } else if (input$formula_type == "custom_formula"){ 
          if(input$sco_adjacency_type_custom == "mobility_adj_mat") {
            adj_mat_raw <- dc$mobility_adj
          } else if (input$sco_adjacency_type_custom == "physical_adj_mat"){
            adj_mat_raw <- dc$physical_adj
          } else {
            adj_mat_raw <- NULL
          }
        } else if (isTRUE(input$spatial_component_chkbx)) {
          if(input$sco_adjacency_type == "mobility_adj_mat") {
            adj_mat_raw <- dc$mobility_adj
          } else {
            adj_mat_raw <- dc$physical_adj
          }
        } else {
          adj_mat_raw <- NULL
        }
        
        # Create the formula
        tmp <- get_formula(
          formula_type = input$formula_type,
          input = setNames(
            lapply(names(input), \(n) input[[n]]),
            names(input)
          ),
          dc =  data_cls,
          time_res = dc$time_res
        )
        valid <- tmp$valid
        formula <- tmp$formula
        
        # fit if valid.
        if (valid){
          formula = eval(formula)
          
          fit_res <- tryCatch(
            {
              # fit the model
              before_fit_cols <- names(data_cls$data)
              
              model <- epistemic::fit_model(
                data_cls=data_cls,
                formula=formula,
                family = input$dist_family,
                reformulate = TRUE,
                adjacency_matrix = adj_mat_raw
              )
              # detect failure mode: model$inla_model is a character error message
              if (!is.null(model$data) && !is.null(model$data$data)) {
                after_fit_cols <- names(model$data$data)
                
                model$data$added_by_fit_model <- setdiff(after_fit_cols, before_fit_cols)
                
                model$data$id_columns <- unique(c(
                  data_cls$id_columns %||% character(0),
                  model$data$region_identifiers %||% character(0),
                  model$data$date_identifiers %||% character(0),
                  model$data$other_ids %||% character(0)
                ))
                
                model$data$covariate_columns <- unique(c(
                  data_cls$covariate_columns %||% character(0),
                  model$data$feature_columns %||% character(0)
                ))
                
                model$data$core_columns <- unique(data_cls$core_columns %||% character(0))
                model$data$other_columns <- unique(data_cls$other_columns %||% character(0))
                
                model$data$core_columns <- unique(data_cls$core_columns %||% character(0))
                
                model$data$column_sources <- c(
                  data_cls$column_sources %||% list(),
                  list(fit_model = model$data$added_by_fit_model)
                )
              }
              
              err_msg <- NULL
              ok <- TRUE
              
              if (is.null(model$inla_model)) {
                ok <- FALSE
                err_msg <- "INLA model did not converge (inla_model is NULL)."
                model_state("error")
              } else if (is.character(model$inla_model)) {
                ok <- FALSE
              err_msg <- model$inla_model  # <-- preserve the exact error string
                model_state("error")
              }
            if (ok){
                model_state("ready")
              }
              
              list(
                model = if (ok) model$inla_model else NULL,
                data_class = model$data,
                formula = deparse1(model$formula),
                ok = ok,
                msg = err_msg
              )
            },
            error = function(e) {
              list(model = NULL, data_class = data_cls, formula = NULL, ok = FALSE, msg = conditionMessage(e))
            }
          )
          return(fit_res)
        } else {
          #This case should be prevented since the run button is disabled when invalid.
          print("Invalid formula!")
          model_state("error")
        }

      }) |> bindEvent(input$estimate_model_btn)
    
      
      ns <- session$ns
      
      observe({
        res <- inla_model_new()
        
        if (!isTRUE(res$ok)) {
          
          msg <- res$msg
          if (!is.null(msg) && nzchar(msg)) {
            # collapse multi-line messages
            msg <- paste(strsplit(msg, "\n")[[1]], collapse = " ")
          } else {
            msg <- "Unknown error"
          }
          
          showNotification(
            paste0(
              "Error! INLA model fitting failed with message: \n",
              msg,
              "\nTry modifying the formula and rerunning."
            ),
            type = "error",
            duration = 10
          )
          
          return(invisible(NULL))
        }
      }) |> bindEvent(inla_model_new())
      
      
      
      # reactive to store a loaded model
      loaded_model <- reactiveVal(NULL)
      
      # observe for zip_file model (i.e. user uploads a saved model)
      observe({
        # model file required
        req(input$zipfile_model)
        model_state("loading")
        validate(need(file.exists(input$zipfile_model$datapath), "Upload did not complete yet"))
        
        # get the model object and the model values from the zip file path
        saved_model_info <- load_saved_model_file(
          path = input$zipfile_model$datapath
        )
      
        
        updateNumericInput(
          session = session,
          inputId = "nforecasts",
          value = saved_model_info[["model_values"]]$nforecasts
        )
        updateSelectInput(
          session = session,
          inputId = "dist_family",
          selected = saved_model_info[["model_values"]]$dist_family
        )
        
        # update other key global reactives from saved_model_info[["model_values"]]
        dc$includes_alaska_hawaii <- saved_model_info[["model_values"]]$includes_alaska_hawaii
        
        # Now, load the cache transitions reactives with any model values
        for(n in names(saved_model_info[["model_values"]])) {
          cache_transitions[[n]] <- saved_model_info[["model_values"]][[n]]
        }
        
        # update the reactive
        loaded_model(saved_model_info[["model_object"]])
        current_model_res(saved_model_info[["model_object"]])
        
      }) |> bindEvent(input$zipfile_model)
      
      observe({
        res <- inla_model_new()
        req(res)
        current_model_res(res)
      }) |> bindEvent(inla_model_new())
      
      inla_model <- reactiveVal(NULL)
      
      observe(inla_model(loaded_model()))
      observe(inla_model(inla_model_new()))
      
      output$download_model_ui <- renderUI({
        req(!is.null(inla_model()))
        tagList(
          layout_columns(
            add_button_hover(
              title = button_list_im[["download_csv"]],
              downloadButton(ns("download_data"),
                             "Download to CSV",
                             class = "btn-primary")),
            add_button_hover(
              title = button_list_im[["save_model"]],
              downloadButton(ns("save_model"),
                             "Save Model",
                             class = "btn-primary")),
            add_button_hover(
              title = button_list_im[["actual_formula"]],
            actionButton(ns("actual_formula"), 
                         "Show Actual Formula", 
                         class = "btn-primary btn-sm")),
            
            widths=c(6,6)
          )
        )
      })
      
      
      output$save_model <- downloadHandler(
        filename = function() {
          paste0("model-", Sys.Date(), ".bsm_model")
        },
        content = function(file) {
          # describe saved query
          model_vals <- list(
            nforecasts = input$nforecasts,
            dist_family = input$dist_family,
            # also save key dc components
            states = dc$states,
            selected_counties = dc$selected_counties,
            includes_alaska_hawaii = dc$includes_alaska_hawaii,
            geo_res = dc$geo_res,
            drange = dc$drange,
            time_res = dc$time_res,
            synd_cat = dc$synd_cat,
            synd_drop_menu = dc$synd_drop_menu
          )
          json_name <- tempfile(fileext = ".json")
          rds_name  <- tempfile(fileext = ".rds")
          jsonlite::write_json(model_vals, 
            json_name,
            pretty = TRUE,
            auto_unbox = TRUE)
          saveRDS(inla_model(), rds_name)
          zip::zipr(file, files = c(rds_name,json_name))
        },
        contentType = "application/zip"
      )
      
      output$inla_model_object <- renderPrint({
        req(inla_model())
        summary(inla_model()$model)
      })
      
      output$inla_model_formula <- renderPrint({
        req(inla_model())
        inla_model()$formula |> cat()
      })
      
      output$inla_model_data <- renderDT({
        req(inla_model())
        dat <- inla_model()$data_class$data
        # Ensure it's a proper data.frame, even if tibble/list/etc.
        dat <- tryCatch(as.data.frame(dat), error = function(e) NULL)
        
        validate(
          need(!is.null(dat), "No table available in model"),
          need(ncol(dat) > 0, "Model table has no columns")
        )
        
        # identify columns to round
        cols_to_round <- non_integer_cols_to_round(dat)
        
        datatable(
          dat,
          colnames = map_table_names_to_display(colnames(dat)),
          rownames=FALSE
        ) |>
          DT::formatRound(columns=cols_to_round, digits=2)

      })
      
      output$download_data <- downloadHandler(
        filename = "processed_data.csv" ,
        content = \(file) data.table::fwrite(inla_model()$data_class$data, file)
      )
      
      
      # show modal box if actual formula button is pressed
      observe({
        
        req(inla_model())
        
        showModal(
          modalDialog(
            title = "Actual Formula Ingested by INLA",
            easyClose = TRUE,
            size = "l",
            card(div(inla_model()$formula, style="font-size:80%"))
          )
        )
      }) |> bindEvent(input$actual_formula)
      
      
      
      output$model_card_body <- renderUI({
        s <- model_state()
        
        if (s == "idle") {
          tagList(
            h4("Run the model"),
            tags$ol(
              tags$li("Set parameters in the sidebar or select a saved model to load."),
              tags$li("Click “Estimate Model”."),
              tags$li("When complete, the model summary will appear here.")
            ),
            tags$p(class = "text-muted", "No model has been estimated yet.")
          )
        } else if ((s == "loading") || (s == "fitting") || (s == "postprocessing")) {
          label_txt <- if (identical(s, "postprocessing")) {
            "Model fit complete. Registering processed data and features ..."
          } else {
            "Estimating Model ... please wait"
          }
          
          div(
            style = "min-height: 220px; display:flex; align-items:center; gap:.75rem;",
            tags$div(
              class = "spinner-border",
              role = "status",
              tags$span(class = "visually-hidden", "Loading...")
            ),
            div(label_txt)
          )
        } else if (s == "error") {
          tagList(
            tags$div(class = "text-danger fw-semibold", "Model estimation failed. Please update the model and rerun.")
          )
        } else { # "ready"
          verbatimTextOutput(ns("inla_model_object"))
        }
      })
    }
  )
}

update_n_forecast_widget <- function(res) {
  lu <- list(
    "daily" = list(label = "num_forecasts_day", value = 28),
    "weekly" = list(label = "num_forecasts_week", value = 3),
    "monthly" = list(label = "num_forecasts_month", value = 1),
    "yearly" = list(label = "num_forecasts_year", value = 1)
  )
  
  lu[[res]]
}

pre_process_data <- function(data, nforecasts) {
  
  req_cols <- c("countyfips", "date", "target", "overall")
  missing_cols <- setdiff(req_cols, names(data))
  if (length(missing_cols)) {
    stop(
      sprintf(
        "Input data is missing required columns: %s. Current columns are: %s",
        paste(missing_cols, collapse = ", "),
        paste(names(data), collapse = ", ")
      )
    )
  }
  
  if (length(nforecasts) != 1 || is.na(nforecasts)) {
    stop("nforecasts must be a single non-missing value.")
  }
  
  data$date <- as.IDate(as.Date(data$date))
  
  if (any(is.na(data$date))) {
    stop("Column `date` contains invalid or missing dates after conversion.")
  }
  
  data_cls <- epistemic::data_class(
    data = data,
    region_column = "countyfips",
    date_column = "date",
    numerator_column = "target",
    denominator_column = "overall",
    generate_expected = TRUE
  )
  
  cols_after_data_class <- names(data_cls$data)
  
  data_cls <- epistemic::add_missing_and_future_dates(
    num_future_steps = as.integer(nforecasts),
    dc = data_cls,
    forward_fill = TRUE,
    den = 1
  )
  cols_after_missing_future <- names(data_cls$data)
  
  epistemic::add_mmwr_week(data_cls)
  cols_after_mmwr <- names(data_cls$data)
  
  epistemic::add_day_of_week(data_cls)
  cols_after_dow <- names(data_cls$data)
  
  data_cls$added_by_data_class <- setdiff(cols_after_data_class, names(data))
  data_cls$added_by_missing_future <- setdiff(cols_after_missing_future, cols_after_data_class)
  data_cls$added_by_mmwr_week <- setdiff(cols_after_mmwr, cols_after_missing_future)
  data_cls$added_by_day_of_week <- setdiff(cols_after_dow, cols_after_mmwr)
  
  # Explicit ID buckets from data_class internals + known preprocessing steps
  data_cls$id_columns <- unique(c(
    setdiff(data_cls$region_identifiers %||% character(0), data_cls$region_column),
    setdiff(data_cls$date_identifiers %||% character(0), data_cls$date_column),
    data_cls$other_ids %||% character(0),
    setdiff(data_cls$added_by_missing_future, c("denominator_source")),
    data_cls$added_by_mmwr_week,
    data_cls$added_by_day_of_week
  ))
  
  data_cls$core_columns <- unique(c(
    data_cls$numerator_column,
    data_cls$denominator_column,
    data_cls$expected_column
  ))
  
  data_cls$other_columns <- unique(c(
    data_cls$region_column,
    data_cls$date_column,
    "region",
    "denominator_source"
  ))
  
  known_non_covariates <- unique(c(
    data_cls$id_columns %||% character(0),
    data_cls$core_columns %||% character(0),
    data_cls$other_columns %||% character(0)
  ))
  
  inferred_covariates <- setdiff(names(data_cls$data), known_non_covariates)
  
  data_cls$covariate_columns <- unique(c(
    data_cls$feature_columns %||% character(0),
    inferred_covariates
  ))

  sort_data_class_data(data_cls)
}

get_formula <- function(formula_type, input, dc, time_res) {
  # if this is custom, return the custom input
  if (formula_type == "custom_formula") {
    f <- input[["custom_formula"]]
    if (!is.null(dc)) {
      res <- epistemic:::validate_inla_formula(f, dc)
      if (!res$ok) {
        return(list(valid = FALSE, formula = pretty_formula(f), errors = res$errors))
      }
    }
    return(list(valid = TRUE,formula = as.formula(f)))
  }
  
  region_random_effect=build_region_random_effect(
    input,
    # use default if formula type is default, or if advance customization toggle is off
    use_default = formula_type == "default" || input[["customize_rre"]] == FALSE
  )
  
  temporal_component=build_temporal_component(
    input,
    time_res = time_res,
    # use default if formula type is default, or if advance customization toggle is off
    use_default = formula_type == "default" || input[["customize_temporal_component"]] == FALSE
  )
  
  spatial_component=build_spatial_component(
    input,
    # use default if formula type is default, or if advance customization toggle is off
    use_default = formula_type == "default" || input[["customize_spatial_component"]] == FALSE
  )
  
  components = list(
    "intercept" = parse_expr("1"),
    "region_re" = region_random_effect,
    "spatial" = spatial_component,
    "temporal" = unlist(temporal_component)
  )
  
  # reduce the components to those that are requested:
  requested = c("intercept")
  
  # if default, or if random regional effect custom selected, add region_re
  if(input$rre_component_chkbx == TRUE || formula_type == "default") requested = c(requested, "region_re")
  
  # if default, or if spatial component custom selected, add spatial
  if(input$spatial_component_chkbx == TRUE || formula_type == "default") requested = c(requested, "spatial")
  
  # when do we add temporal:
  # 1) if temporal component checked & not (default & weekly) OR
  # 2) if default & daily
  add_temporal <- any(
   (input$temporal_component_chkbx && !(formula_type=="default" & time_res=="weekly")),
   (formula_type == "default" & time_res == "daily")
  )
  # if the temporal component condition has been met, add this to the growing
  # list of components
  if(add_temporal) requested = c(requested, "temporal")
  
  # now, make sure this is an unnested set of expressions, using `unlist()`
  formula <- purrr::reduce(
    components[requested] |> unlist(), ~call2('+', .x, .y)
  )
  
  list(valid = TRUE, formula = expr(target~!!formula))
  
}

build_region_random_effect <- function(input, use_default=FALSE) {
  
  if(use_default == TRUE) {
    for(n in names(MODEL_COMPONENT_DEFAULTS)) {
      input[[n]] = MODEL_COMPONENT_DEFAULTS[[n]]
    }
  }
  
  check_names(input, c("rre_prec_pc_param", "rre_prec_pc_alpha"))
  
  rlang::parse_expr(
    paste(
      "f(",
      "r_id,",
      "model='iid',",
      "hyper=list(prec = list(prior = 'pc.prec', param =c(",
      input[["rre_prec_pc_param"]], ",", input[["rre_prec_pc_alpha"]], ")))",
      ")"
    )
  )
}

build_temporal_component <- function(input, time_res, use_default = FALSE) {
  
  if(use_default == TRUE) {
    for(n in names(MODEL_COMPONENT_DEFAULTS)) {
      input[[n]] = MODEL_COMPONENT_DEFAULTS[[n]]
    }
  }
  
  check_names(input, c("tco_model", "tco_model_ar_order"))
  if(time_res == "daily") check_names(input, c("tco_model_d", "tco_model_ar_order_d"))
  
  # set some placeholders for the daily (dc) and weekly (wc) components
  # Note, when we pass empty string to parse_exprs, we get empty 
  # list (which is what we would want!)
  dc = "";wc = ""
  
  # if daily component checkbox is clicked we need to replace dc with actual expr
  if(input[["daily_tco_chkbx"]]) {
    dc = paste0("f(dow_id, model = '", input[["tco_model_d"]], "' ")
    if(input[["tco_model_d"]] == "ar") {
      dc = paste0(dc, ", order=", input[["tco_model_ar_order_d"]], ")")
    } else {
      dc <-paste0(dc,",cyclic=TRUE",")")
    }
  }
  
  # if weekly component checkbox and not default! we need to add this
  if(input[["weekly_tco_chkbx"]] & use_default==FALSE) {
    wc = paste0("f(week_id, model = '", input[["tco_model"]], "' ")
    if(input[["tco_model"]] == "ar") {
      wc = paste0(wc, ", order=", input[["tco_model_ar_order"]], ")")
    } else {
      wc <-paste0(wc,",cyclic=TRUE",")")
    }
  }
  
  parse_exprs(c(dc,wc))
  
}

build_spatial_component <- function(input, use_default = FALSE) {
  
  if(use_default == TRUE) {
    for(n in names(MODEL_COMPONENT_DEFAULTS)) {
      input[[n]] = MODEL_COMPONENT_DEFAULTS[[n]]
    }
  }
  
  check_names(input, c("sco_model_type", "sco_control_group_model", "sco_control_group_ar_order"))
  
  sc = paste0(
    "f(",
    "r_id, ",
    "graph = adjacency_matrix,",
    "model='", input[["sco_model_type"]], "',",
    "group = d_id,",
    "control.group = list(model='", input[["sco_control_group_model"]], "'"
  )
  if(input[["sco_control_group_model"]] == "ar") {
    sc = paste0(
      sc,
      ", order=", input[["sco_control_group_ar_order"]], "), "
    )
  } else {
    sc <- paste0(sc, "), ")
  }
  
  prec_prior_name = fcase(
    input[["sco_model_type"]] == "besagproper", "prec",
    input[["sco_model_type"]] == "bym", "prec.spatial",
    input[["sco_model_type"]] == "besag", "prec"
  )
  sc = paste0(
    sc,
    "hyper=list(", prec_prior_name, " = list(prior = 'pc.prec', param =c(",
    input[["sco_prec_pc_param"]], ", ", input[["sco_prec_pc_alpha"]], ")))",
    ")"
  )
  
  rlang::parse_expr(sc)
  
}

MODEL_COMPONENT_DEFAULTS = list(
  # regional random effect defaults
  rre_prec_pc_param = 0.2,
  rre_prec_pc_alpha = 0.01,
  
  # spatial component defaults
  sco_model_type = "besagproper",
  sco_control_group_model = "ar",
  sco_control_group_ar_order = 1L,
  sco_prec_pc_param = 0.2,
  sco_prec_pc_alpha = 0.01,
  
  # weekly temporal component defaults
  tco_model = "rw2",
  tco_model_ar_order = 1L,
  # daily temporal component defaults
  tco_model_d = "rw2",
  tco_model_ar_order_d = 1L,
  
  custom_formula = ""
)

check_names <- function(x, n) {
  if(!all(n %in% names(x))) {
    cli::cli_abort(paste(
      "Names [", paste(n, collapse=","), " ] not found in object"
    ))
  }
}

model_ready_to_run <- function(data, formula, input) {
  
  # defaults - no message, primary class for text
  msg = character(0); cl = "p-2 text-primary"; valid=TRUE
  
  # currently, this is the only requirement  
  if(is.null(data)) msg <- "Please load data first"
  
  # TODO: check if formula is valid
  # We need to write a formula_valid function that returns
  # a message and valid flag in a list
  # formula_valid <- formula_valid(formula, input)
  # if(!formula_valid$valid) {
  #   msg = formula_valid["msg"]
  #   valid = FALSE
  # }
  
  if(length(msg)>0) {
    cl = "shiny-output-error-validation"
    valid=FALSE
  }
  
  list(
    msg = div(class = cl, htmltools::HTML(paste(msg, collapse = "<br>"))),
    valid = valid
  )
      
}

pretty_formula <- function(x) {
  # If it's already a formula/call/expression, deparse it
  if (!is.character(x)) {
    return(paste(deparse(x, width.cutoff = 500), collapse = "\n"))
  }
  
  # Raw text from textAreaInput
  if (length(x) == 0 || is.na(x)) return("")
  x <- paste(x, collapse = "\n")
  
  parsed <- tryCatch(rlang::parse_expr(x), error = function(e) NULL)
  
  # If it doesn't parse, show exactly what the user typed (no extra quoting)
  if (is.null(parsed)) return(x)
  
  if (rlang::is_scalar_atomic(parsed)) {
    return(as.character(parsed))
  }
  
  # Otherwise, show as code
  rlang::expr_text(parsed)
}

`%||%` <- function(x, y) if (is.null(x) || length(x) == 0) y else x

# Create the shared feature-store object used across modeling and
# visualization. It tracks feature definitions, ordering, and the flattened
# metadata table consumed by the viz modules.
init_feature_df <- function() {
  rv <- reactiveValues(
    next_id  = 0L,
    features = list(),
    order    = character(0),
    last_id  = NULL,
    probs    = 0.5,
    refresh  = 0L,
    force_select_fid = NULL
  )
  
  add_to_order <- function(id) {
    if (!id %in% rv$order) {
      rv$order <- c(rv$order, id)
      TRUE
    } else {
      FALSE
    }
  }
  
  # Classify a stored column into the app's base feature categories so it can
  # be registered with the correct type/scale metadata.
  get_column_meta <- function(col, data_cls) {
    core_cols  <- unique(data_cls$core_columns %||% character(0))
    id_cols    <- unique(data_cls$id_columns %||% character(0))
    cov_cols   <- unique(data_cls$covariate_columns %||% character(0))
    other_cols <- unique(data_cls$other_columns %||% character(0))
    
    is_id <- col %in% id_cols
    is_core <- (col %in% core_cols) && !is_id
    is_cov <- (col %in% cov_cols) && !(is_id || is_core)
    is_pred <- grepl("^predicted_", col)
    is_other <- (col %in% other_cols) && !(is_id || is_core || is_cov || is_pred)
    
    list(
      is_core = is_core,
      is_id = is_id,
      is_pred = is_pred,
      is_cov = is_cov,
      is_other = is_other
    )
  }
  
  sync_base_columns <- function(data, data_cls, exclude_cols = character(0)) {
    if (is.null(data) || is.null(data_cls)) return(invisible(NULL))
    
    dt <- data.table::as.data.table(data)
    # Calculated features are registered separately so they can keep their own
    # feature types and grouping metadata.
    cols <- names(dt)
    if (length(exclude_cols)) cols <- setdiff(cols, unique(exclude_cols))
    if (!length(cols)) return(invisible(NULL))
    
    pretty_map <- map_table_names_to_display(cols, quantile_suffix = NULL, keep_names = TRUE)
    pretty_labs <- cols
    if (!is.null(names(pretty_map))) {
      pretty_labs <- unname(pretty_map[cols])
      pretty_labs[is.na(pretty_labs) | pretty_labs == ""] <- cols[is.na(pretty_labs) | pretty_labs == ""]
    }
    
    changed <- FALSE
    
    for (i in seq_along(cols)) {
      col <- cols[[i]]
      lab <- pretty_labs[[i]]
      meta <- get_column_meta(col, data_cls)
      
      if (meta$is_cov) {
        fid <- paste0("cov__", col)
        ftype <- "covariate"
        fscale <- "other"
      } else if (meta$is_id) {
        fid <- paste0("id__", col)
        ftype <- "id"
        fscale <- "other"
      } else if (meta$is_core) {
        fid <- paste0("col__", col)
        ftype <- "core"
        fscale <- "other"
      } else {
        fid <- paste0("col__", col)
        ftype <- "other"
        fscale <- "other"
      }
      
      rv$features[[fid]] <- modifyList(
        rv$features[[fid]] %||% list(),
        list(
          id = fid,
          label = lab,
          description = "",
          feature_type = ftype,
          feature_scale = fscale,
          out_cols = col,
          params = list(source_col = col)
        )
      )
      
      changed <- add_to_order(fid) || changed
    }
    
    if (changed) rv$refresh <- rv$refresh + 1L
    invisible(NULL)
  }
  
  register_calculated_features <- function(
    ci_widths = c(0.5, 0.9, 0.95, 0.99),
    include_mean = TRUE,
    include_median = TRUE,
    include_scales = c("counts", "proportion")
  ) {
    # Builtin CIs are stored as composite features, while their endpoint
    # quantiles are also registered as separate selectable features.
    changed <- FALSE
    
    ci_group_id <- function(scale, ci) {
      paste0("builtin__ci_group__", scale, "__ci", fmt_qname(ci))
    }
    
    quantile_feature_id <- function(scale, q) {
      paste0("builtin__quantile__", scale, "__q", fmt_qname(q))
    }
    
    quantile_feature_label <- function(scale, q) {
      sprintf("Quantile q=%s (%s)", fmt_num(q), scale)
    }
    
    add_feature_def <- function(
      fid, label, description, feature_type, feature_scale, out_cols, params = list(),
      feature_kind = "atomic", group_id = NA_character_, group_role = NA_character_,
      member_ids = character(0)
    ) {
      if (is.null(rv$features[[fid]])) {
        rv$features[[fid]] <- list(
          id = fid,
          label = label,
          description = description,
          feature_type = feature_type,
          feature_scale = feature_scale,
          out_cols = out_cols,
          params = params,
          feature_kind = feature_kind,
          group_id = group_id,
          group_role = group_role,
          member_ids = member_ids
        )
        changed <<- TRUE
      }
      changed <<- add_to_order(fid) || changed
    }
    
    for (sc in include_scales) {
      if (isTRUE(include_mean)) {
        add_feature_def(
          fid = paste0("builtin__mean__", sc),
          label = sprintf("Posterior Mean (%s)", sc),
          description = sprintf("Posterior mean on the %s scale.", sc),
          feature_type = "mean",
          feature_scale = sc,
          out_cols = sprintf("Posterior Mean (%s)", sc),
          params = list()
        )
      }
      
      if (isTRUE(include_median)) {
        add_feature_def(
          fid = quantile_feature_id(sc, 0.5),
          label = quantile_feature_label(sc, 0.5),
          description = sprintf("Posterior median on the %s scale.", sc),
          feature_type = "quantile",
          feature_scale = sc,
          out_cols = quantile_feature_label(sc, 0.5),
          params = list(q = 0.5)
        )
      }
      
      for (ci in ci_widths) {
        a <- (1 - ci) / 2
        q_lower <- a
        q_upper <- 1 - a
        group_id <- ci_group_id(sc, ci)
        qid_lower <- quantile_feature_id(sc, q_lower)
        qid_upper <- quantile_feature_id(sc, q_upper)
        qlab_lower <- quantile_feature_label(sc, q_lower)
        qlab_upper <- quantile_feature_label(sc, q_upper)
        nm <- sprintf("%s CI (%s)", fmt_num(ci), sc)
        
        add_feature_def(
          fid = qid_lower,
          label = qlab_lower,
          description = sprintf(
            "Posterior quantile at q=%s on the %s scale. Lower endpoint of the %s credible interval.",
            fmt_num(q_lower),
            sc,
            fmt_num(ci)
          ),
          feature_type = "quantile",
          feature_scale = sc,
          out_cols = qlab_lower,
          params = list(q = q_lower),
          feature_kind = "atomic",
          group_id = group_id,
          group_role = "lower"
        )
        
        add_feature_def(
          fid = qid_upper,
          label = qlab_upper,
          description = sprintf(
            "Posterior quantile at q=%s on the %s scale. Upper endpoint of the %s credible interval.",
            fmt_num(q_upper),
            sc,
            fmt_num(ci)
          ),
          feature_type = "quantile",
          feature_scale = sc,
          out_cols = qlab_upper,
          params = list(q = q_upper),
          feature_kind = "atomic",
          group_id = group_id,
          group_role = "upper"
        )
        
        add_feature_def(
          fid = paste0("builtin__confidence_interval__", sc, "__ci", fmt_qname(ci)),
          label = nm,
          description = sprintf("%s%% posterior credible interval on the %s scale.", 100 * ci, sc),
          feature_type = "confidence_interval",
          feature_scale = sc,
          out_cols = c(paste0(nm, " Lower"), paste0(nm, " Upper")),
          params = list(ci = ci),
          feature_kind = "composite",
          group_id = group_id,
          member_ids = c(qid_lower, qid_upper)
        )
      }
    }
    
    if (changed) rv$refresh <- rv$refresh + 1L
    invisible(NULL)
  }
  
  register_default_calculated_features <- function() {
    register_calculated_features(
      ci_widths = c(0.5, 0.9, 0.95, 0.99),
      include_mean = TRUE,
      include_median = TRUE,
      include_scales = c("counts", "proportion")
    )
  }
  
  
  list(
    rv = rv,
    sync_base_columns = sync_base_columns,
    register_calculated_features = register_calculated_features,
    register_default_calculated_features = register_default_calculated_features,
    get_feature = function(fid) {
      rv$features[[fid]]
    },
    choices = reactive({
      rv$refresh
      if (!length(rv$order)) return(stats::setNames(character(0), character(0)))
      labs <- vapply(
        rv$order,
        function(fid) rv$features[[fid]]$label %||% fid,
        character(1)
      )
      stats::setNames(rv$order, labs)
    }),
    # Flatten the feature store into the metadata table used by the selector
    # panels and viz modules.
    features_df = reactive({
      rv$refresh
      if (!length(rv$order)) {
        return(data.frame(
          id = character(0),
          label = character(0),
          description = character(0),
          feature_type = character(0),
          feature_scale = character(0),
          feature_kind = character(0),
          group_id = character(0),
          group_role = character(0),
          stringsAsFactors = FALSE
        ))
      }
      
      do.call(rbind, lapply(rv$order, function(fid) {
        f <- rv$features[[fid]]
        data.frame(
          id = f$id %||% fid,
          label = f$label %||% fid,
          description = f$description %||% "",
          feature_type = f$feature_type %||% "other",
          feature_scale = f$feature_scale %||% "other",
          feature_kind = f$feature_kind %||% "atomic",
          group_id = f$group_id %||% NA_character_,
          group_role = f$group_role %||% NA_character_,
          stringsAsFactors = FALSE
        )
      }))
    })
  )
}
