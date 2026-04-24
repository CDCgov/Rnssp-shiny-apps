# © 2025 The Johns Hopkins University Applied Physics Laboratory LLC
# Development of this software was sponsored by the U.S. Government under
# contract no. 75D30124C19958

button_list_pi <- list(
  csv_button = "Download displayed data to a local csv file.",
  clear_filters = "Reset all data filters."
)

label_list_pi <- list(
  table_decimals = list(
    l = "Table decimals",
    m = "Set the number of decimal places shown for non-integer numeric values in the table."
  )
)


viz_posterior_ui <- function(id) {
  ns <- NS(id)
  table_id <- ns("posterior_data")
  
  nav_panel(
    title = "Posterior Data",
    layout_sidebar(
      sidebar = sidebar(
        id = ns("posterior_sidebar"),
        width = SIDEBAR_WIDTH * 2,
        open = "open",
        title = "Options",
        feature_sidepanel_ui(
          ns("feature_side"),
          title = "Feature Filters",
          allow_multiple = TRUE,
          show_select_all = TRUE
        ),
        numericInput(
          ns("dt_digits"),
          label = labeltt(label_list_pi[["table_decimals"]]),
          value = 2,
          min = 0,
          max = 10,
          step = 1
        )
      ),
      card(
        card_body(
          style = "overflow: visible;",
          div(
            id = ns("posterior_wrap"),
            style = "width: 100%;",
            div(
              class = "reactable-top-controls",
              add_button_hover(
                title = button_list_pi[["clear_filters"]],
                actionButton(ns("clear_filters"), class = "btn-primary btn-sm", "Clear Filters")
              ),
              div(
                class = "reactable-search-wrap",
                tags$label(`for` = ns("posterior_data_search"), "Search"),
                textInput(ns("posterior_data_search"), NULL, placeholder = "Search displayed data")
              )
            ),
            reactable::reactableOutput(ns("posterior_data"), width = "100%"),
            div(
              class = "reactable-bottom-controls",
              add_button_hover(
                title = button_list_pi[["csv_button"]],
                actionButton(
                  ns("download_posterior_csv"),
                  class = BUTTON_CLASS,
                  "Download CSV",
                  onclick = sprintf(
                    "if (window.Reactable) Reactable.downloadDataCSV('%s', 'posterior_data.csv');",
                    table_id
                  )
                )
              )
            )
          )
        )
      )
    )
  )
}

viz_posterior_server <- function(id, im, results, feature_store) {
  moduleServer(
    id,
    function(input, output, session) {
      get_store <- function() {
        if (is.function(feature_store)) feature_store() else feature_store
      }
      
      get_base_source <- reactive({
        req(im$data_cls)
        data.table::as.data.table(im$data_cls$data)
      })
      
      default_feature_ids <- c(
        "col__target",
        "col__overall",
        "builtin__quantile__proportion__q0.5",
        "builtin__confidence_interval__proportion__ci0.95"
      )
      required_core_cols <- c("countyfips", "date", "region")
      
      # Posterior Data can display any stored feature whose output columns are
      # already present in the shared data table.
      plottable_features_df <- reactive({
        store <- get_store()
        base_source <- get_base_source()
        req(store, base_source)
        
        df <- as.data.frame(store$features_df())
        if (!nrow(df)) return(df)
        
        keep <- vapply(df$id, function(fid) {
          f <- store$get_feature(fid)
          if (is.null(f)) return(FALSE)
          out_cols <- f$out_cols %||% character(0)
          length(out_cols) > 0 && all(out_cols %in% names(base_source))
        }, logical(1))
        
        df[keep, , drop = FALSE]
      })
      
      filtered_feature_store <- list(
        features_df = reactive(plottable_features_df())
      )
      
      feature_filters <- feature_sidepanel_server(
        "feature_side",
        feature_store = filtered_feature_store,
        allow_multiple = TRUE,
        show_select_all = TRUE,
        initial_selected_ids = default_feature_ids
      )
      
      selected_feature_ids <- reactive({
        keys <- feature_filters()$selected_features %||% character(0)
        ids <- sub("::.*$", "", keys)
        unique(ids[nzchar(ids)])
      })
      
      selected_feature_cols <- reactive({
        store <- get_store()
        req(store)
        
        unique(unlist(lapply(selected_feature_ids(), function(fid) {
          f <- store$get_feature(fid)
          if (is.null(f)) return(character(0))
          f$out_cols %||% character(0)
        })))
      })
      
      posterior_tbl <- reactive({
        req(im$data_cls)
        out <- data.table::copy(get_base_source())
        
        if ("countyfips" %in% names(out)) out[, countyfips := as.character(countyfips)]
        if ("date" %in% names(out) && !inherits(out$date, "Date")) out[, date := as.Date(date)]
        if ("countyfips" %in% names(out)) out[, countyfips := as.factor(countyfips)]
        if ("region" %in% names(out)) out[, region := as.factor(region)]
        
        id_first <- intersect(c("countyfips", "date", "region"), names(out))
        others <- setdiff(names(out), id_first)
        data.table::setcolorder(out, c(id_first, others))
        out[]
      })
      
      table_id <- session$ns("posterior_data")
      
      observe({
        session$sendCustomMessage(
          "clear-reactable-filters",
          list(id = table_id)
        )
        updateTextInput(session, "posterior_data_search", value = "")
      }) |> bindEvent(input$clear_filters, ignoreInit = TRUE)
      
      observe({
        session$sendCustomMessage(
          "set-reactable-search",
          list(
            id = table_id,
            value = input$posterior_data_search %||% ""
          )
        )
      }) |> bindEvent(input$posterior_data_search, ignoreInit = FALSE)
      
      output$posterior_data <- reactable::renderReactable({
        req(input$dt_digits)
        df <- posterior_tbl()
        req(df)
        
        keep <- unique(c(required_core_cols, selected_feature_cols()))
        keep <- intersect(keep, names(df))
        if (length(keep)) df <- df[, ..keep]
        
        build_standard_reactable(
          df,
          table_id = table_id,
          digits = input$dt_digits,
          searchable = FALSE
        )
      })
      
    }
  )
}
