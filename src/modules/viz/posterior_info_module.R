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
          tags$style(HTML(sprintf("
            #%s .dataTables_scrollBody thead,
            #%s .dataTables_scrollBody thead * {
              visibility: hidden !important;
              pointer-events: none !important;
            }
          ", ns("posterior_wrap"), ns("posterior_wrap")))),
          div(
            id = ns("posterior_wrap"),
            style = "width: 100%;",
            reactable::reactableOutput(ns("posterior_data"), width = "100%"),
            div(
              style = "display:flex; gap:10px; align-items:center;",
              add_button_hover(
                title = button_list_pi[["clear_filters"]],
                actionButton(ns("clear_filters"), class = "btn-primary btn-sm", "Clear Filters")
              ),
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
      }) |> bindEvent(input$clear_filters, ignoreInit = TRUE)
      
      output$posterior_data <- reactable::renderReactable({
        req(input$dt_digits)
        df <- posterior_tbl()
        req(df)
        
        keep <- unique(c(required_core_cols, selected_feature_cols()))
        keep <- intersect(keep, names(df))
        if (length(keep)) df <- df[, ..keep]
        
        display_names <- map_table_names_to_display(
          names(df),
          quantile_suffix = NULL,
          keep_names = TRUE
        )
        if (is.null(names(display_names))) names(display_names) <- names(df)
        
        cols_to_round <- non_integer_cols_to_round(df)
        digits <- max(0, min(10, as.integer(input$dt_digits %||% 2)))
        date_cols <- names(df)[sapply(df, inherits, "Date")]
        
        col_defs <- lapply(names(df), function(col) {
          label <- display_names[[col]] %||% col
          is_num <- is.numeric(df[[col]])
          is_rounded <- col %in% names(cols_to_round)
          is_date <- col %in% date_cols
          
          if (is_num) {
            reactable::colDef(
              name = label,
              align = "right",
              filterable = TRUE,
              filterMethod = numeric_range_filter_method,
              filterInput = function(values, name) numeric_range_filter_input(values, name, table_id),
              format = if (is_rounded) reactable::colFormat(digits = digits) else NULL
            )
          } else if (is_date) {
            reactable::colDef(
              name = label,
              filterable = TRUE,
              filterMethod = date_filter_method,
              filterInput = function(values, name) date_filter_input(values, name, table_id)
            )
          } else {
            reactable::colDef(
              name = label,
              filterable = TRUE,
              filterMethod = checkbox_filter_method,
              filterInput = function(values, name) checkbox_filter_input(values, name, table_id)
            )
          }
        })
        names(col_defs) <- names(df)
        
        page_size <- min(nrow(df), 10L)
        
        reactable::reactable(
          df,
          columns = col_defs,
          defaultPageSize = page_size,
          pageSizeOptions = c(5, 10, 15, 25, 50, 100),
          searchable = TRUE,
          filterable = TRUE,
          highlight = TRUE,
          striped = TRUE,
          bordered = TRUE,
          resizable = TRUE,
          wrap = TRUE,
          defaultColDef = reactable::colDef(
            minWidth = 120,
            headerStyle = list(
              whiteSpace = "normal",
              wordBreak = "break-word",
              lineHeight = "1.1"
            ),
            style = list(
              whiteSpace = "nowrap"
            )
          ),
          fullWidth = TRUE,
          theme = BS_REACTABLE_THEME
        )
      })
      
    }
  )
}
