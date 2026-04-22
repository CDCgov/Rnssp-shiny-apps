# © 2025 The Johns Hopkins University Applied Physics Laboratory LLC
# Development of this software was sponsored by the U.S. Government under
# contract no. 75D30124C19958

label_list_feature_side <- list(
  selected_features = list(
    l = "Selected feature(s)",
    m = "Features in this list are currently active for this view. Click a feature to remove it from the selected set."
  ),
  selected_feature = list(
    l = "Selected feature",
    m = "This feature is currently active for this view. Click it to remove it from the selected set."
  ),
  available_features = list(
    l = "Available feature(s)",
    m = "These features are available for the current visualization after applying any filters below. Click a feature to add it to the selected set."
  ),
  feature_filters = list(
    l = "Feature Filters",
    m = "Filter the available feature list by scale or feature type. Filters only affect what can be added; they do not remove features that are already selected."
  ),
  filter_scale = list(
    l = "Scale",
    m = "Limit the available feature list to features on the selected scale. Counts are visit totals, proportion is relative to all visits, and Other includes features without one of those standard scales."
  ),
  filter_type = list(
    l = "Feature type",
    m = "Limit the available feature list to specific feature categories such as means, quantiles, intervals, probabilities, covariates, or identifier fields."
  )
)

feature_sidepanel_ui <- function(id,
                                 title = "Filters",
                                 allow_multiple = TRUE,
                                 show_select_all = FALSE) {
  ns <- NS(id)
  
  heading <- if (allow_multiple) {
    h5(labeltt(label_list_feature_side[["selected_features"]]))
  } else {
    h5(labeltt(label_list_feature_side[["selected_feature"]]))
  }
  
  tagList(
    tags$style(HTML(sprintf("
      #%s select[multiple],
      #%s select[multiple] {
        height: 15vh !important;
        overflow-y: auto;
      }
    ",
                            ns("selected_list"),
                            ns("add_list")
    ))),
    
    div(
      id = ns("wrapper"),
      heading,
      
      selectInput(
        ns("selected_list"),
        label = NULL,
        choices = character(0),
        selected = character(0),
        multiple = TRUE,
        selectize = FALSE,
        size = if (allow_multiple) NULL else 2,
        width = "100%"
      ),
      if(allow_multiple) {
        tagList(
          tags$small(class = "text-muted", "Click an item to remove it from the selected group."),
          actionButton(
            ns("clear_selected"),
            "Clear selected",
            class = "btn-primary btn-sm",
            style = "width:100%;",
            title = "Remove all currently selected features from this panel. This does not delete the features from the app."
          )
        )
      },
      tags$hr(),
      h5(labeltt(label_list_feature_side[["available_features"]])),
      if (allow_multiple && isTRUE(show_select_all)) {
        actionButton(
          ns("select_all_available"),
          "Select all available",
          class = "btn-primary btn-sm",
          style = "width:100%; margin-bottom: 8px;",
          title = "Add every feature currently shown in the available list. Hidden or filtered-out features are not added."
        )
      },
      selectInput(
        ns("add_list"),
        label = NULL,
        choices = character(0),
        selected = character(0),
        multiple = TRUE,
        selectize = FALSE,
        width = "100%"
      ),
      if(allow_multiple) {
        tags$small(class = "text-muted", "Click an item to add it to the selected group.")
      },
      tags$hr(),
      tags$details(
        style = "margin-top: 8px; margin-bottom: 12px; display: block;",
        tags$summary(
          labeltt(list(
            l = title,
            m = label_list_feature_side[["feature_filters"]][["m"]]
          )),
          style = "cursor:pointer; font-weight:600;"
        ),
        tags$div(
          style = "margin-top: 8px;",
          checkboxGroupInput(
            ns("filter_scale"),
            labeltt(label_list_feature_side[["filter_scale"]]),
            choices = c(
              "Counts" = "counts",
              "Proportion" = "proportion",
              "Other" = "other"
            ),
            selected = character(0)
          ),
          
          checkboxGroupInput(
            ns("filter_type"),
            labeltt(label_list_feature_side[["filter_type"]]),
            choices = c(
              "Mean" = "mean",
              "Posterior Quantile" = "quantile",
              "Confidence Interval" = "confidence_interval",
              "Exceedance Probability" = "exceedance_probability",
              "Change Probability" = "change_probability",
              "Covariate" = "covariate",
              "ID" = "id",
              "Other" = "other"
            ),
            selected = character(0)
          ),
          
          actionButton(
            ns("reset_filters"),
            "Reset filters",
            class = "btn-primary btn-sm",
            style = "width:100%;",
            title = "Clear the scale and feature-type filters and show the full available feature list again. Already selected features remain selected."
          )
        )
      )
    )
  )
}


feature_sidepanel_server <- function(id,
                                     feature_store,
                                     allow_multiple = TRUE,
                                     show_select_all = FALSE,
                                     reset_clears_selected = FALSE,
                                     initial_selected_id = NULL,
                                     initial_selected_ids = NULL) {
  moduleServer(id, function(input, output, session) {
    
    get_store <- function() {
      if (is.function(feature_store)) feature_store() else feature_store
    }
    
    df_all <- reactive({
      get_store()$features_df()
    })
    
        normalize_df <- function(df) {
      if (is.null(df) || nrow(df) == 0) return(df)
      
      df$feature_scale[is.na(df$feature_scale) | df$feature_scale == ""] <- "other"
      df$feature_type[is.na(df$feature_type) | df$feature_type == ""] <- "other"
      df$scale_filter <- ifelse(
        df$feature_scale %in% c("counts", "proportion"),
        df$feature_scale,
        "other"
      )
      df$.key <- paste0(df$id, "::", df$feature_scale, "::", df$feature_type)
      df
    }
    
    selected_rv <- reactiveVal(character(0))
    
    observe({
      df <- normalize_df(df_all())
      valid_keys <- if (is.null(df) || !nrow(df)) character(0) else as.character(df$.key)
      cur <- selected_rv() %||% character(0)
      next_sel <- intersect(cur, valid_keys)
      if (!identical(cur, next_sel)) selected_rv(next_sel)
    }) %>% bindEvent(df_all(), ignoreInit = FALSE)
        
        observe({
          df <- normalize_df(df_all())
          init_ids <- initial_selected_ids %||% character(0)
          if (!length(init_ids)) {
            init_ids <- initial_selected_id %||% character(0)
          }
          init_ids <- init_ids[nzchar(init_ids)]
          cur <- selected_rv() %||% character(0)
          if (!length(init_ids) || length(cur) > 0 || is.null(df) || !nrow(df)) return()
          
          init_keys <- df$.key[df$id %in% init_ids]
          if (!length(init_keys)) return()
          init_keys <- unique(as.character(init_keys))
          if (!allow_multiple) init_keys <- init_keys[[1]]
          selected_rv(init_keys)
        }) %>% bindEvent(df_all(), ignoreInit = FALSE)
    
    make_named_choices <- function(df) {
      if (is.null(df) || nrow(df) == 0) return(setNames(character(0), character(0)))
      vals <- as.character(df$.key)
      labs <- as.character(df$label)
      stats::setNames(vals, labs)
    }
    
    selected_choices_named <- reactive({
      df  <- normalize_df(df_all())
      sel <- selected_rv()
      if (is.null(df) || !nrow(df) || !length(sel)) return(setNames(character(0), character(0)))
      
      df_sel <- df[df$.key %in% sel, , drop = FALSE]
      if (!nrow(df_sel)) return(setNames(character(0), character(0)))
      df_sel <- df_sel[match(sel, df_sel$.key), , drop = FALSE]
      df_sel <- df_sel[!is.na(df_sel$.key), , drop = FALSE]
      if (!nrow(df_sel)) return(setNames(character(0), character(0)))
      make_named_choices(df_sel)
    })
    
    # Filters only modify the "can-be-added" list (ie filters won't unselect values)
    filtered_features_df <- reactive({
      df <- normalize_df(df_all())
      if (is.null(df) || nrow(df) == 0) return(df)
      
      scales <- input$filter_scale %||% character(0)
      types  <- input$filter_type %||% character(0)
      
      if (length(scales) > 0) df <- df[df$scale_filter %in% scales, , drop = FALSE]
      if (length(types)  > 0) df <- df[df$feature_type  %in% types,  , drop = FALSE]
      
      df
    })
    
    reorder_default_df <- function(df) {
      if (is.null(df) || nrow(df) == 0) return(df)
      df[order(tolower(df$label), df$label, df$id), , drop = FALSE]
    }
    
    add_choices_named <- reactive({
      df  <- normalize_df(filtered_features_df())
      sel <- selected_rv()
      if (is.null(df) || !nrow(df)) return(setNames(character(0), character(0)))
      
      df <- reorder_default_df(df)
      if (length(sel)) df <- df[!(df$.key %in% sel), , drop = FALSE]
      
      make_named_choices(df)
    })
    
    # Move items between selected and can-be-selected lists
    observe({
      updateSelectInput(session, "add_list",
                        choices  = add_choices_named(),
                        selected = character(0))
      
      updateSelectInput(session, "selected_list",
                        choices  = selected_choices_named(),
                        selected = character(0))
    }) %>% bindEvent(add_choices_named(), selected_choices_named(), ignoreInit = FALSE)
    
    observe({
      keys <- input$add_list %||% character(0)
      if (!length(keys)) return()
      
      cur <- selected_rv()
      if (!allow_multiple) {
        selected_rv(keys[[1]])
      } else {
        selected_rv(unique(c(cur, keys)))
      }
      
      updateSelectInput(session, "add_list", selected = character(0))
    }) %>% bindEvent(input$add_list, ignoreInit = TRUE)

    observe({
      if (!isTRUE(allow_multiple) || !isTRUE(show_select_all)) return()
      available_keys <- unname(add_choices_named())
      if (!length(available_keys)) return()
      cur <- selected_rv() %||% character(0)
      selected_rv(unique(c(cur, available_keys)))
      updateSelectInput(session, "add_list", selected = character(0))
    }) %>% bindEvent(input$select_all_available, ignoreInit = TRUE)
    
    # remove selected item from "can-be-selected" list
    observe({
      keys <- input$selected_list %||% character(0)
      if (!length(keys)) return()
      
      cur <- selected_rv()
      selected_rv(setdiff(cur, keys))
      
      updateSelectInput(session, "selected_list", selected = character(0))
    }) %>% bindEvent(input$selected_list, ignoreInit = TRUE)
    
    observe({
      if (!length(selected_rv() %||% character(0))) return()
      selected_rv(character(0))
    }) %>% bindEvent(input$clear_selected, ignoreInit = TRUE)
    
    observe({
      updateCheckboxGroupInput(session, "filter_scale", selected = character(0))
      updateCheckboxGroupInput(session, "filter_type", selected = character(0))
      
      if (isTRUE(reset_clears_selected)) selected_rv(character(0))
    }) %>% bindEvent(input$reset_filters, ignoreInit = TRUE)
    
    reactive({
      list(
        selected_features = selected_rv(),
        filter_scale      = input$filter_scale %||% character(0),
        filter_type       = input$filter_type %||% character(0),
        add_menu_choices  = add_choices_named()
      )
    })
  })
}
