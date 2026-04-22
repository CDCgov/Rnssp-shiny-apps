# © 2025 The Johns Hopkins University Applied Physics Laboratory LLC
# Development of this software was sponsored by the U.S. Government under
# contract no. 75D30124C19958

label_list_cs <- list(
  upload_covariates = list(
    l = "Upload covariates file",
    m = "Select a covariates file to upload. This should contain date and/or region columns for joining with query data."
  ),
  region_col_ui = list( 
    l = "Select the region column",
    m = "Select region column in the covariate dataset for joining with the query data. Leave as empty to join only on dates. This is assumed to be a 5 digit FIPS code."
  ),
  date_col_ui = list( 
    l = "Select the date column",
    m = "Select date column in the covariate dataset for joining with the query data. Leave as empty to join only on regions. This is assumed to have the format YYYY-MM-DD."
  ),
  feature_col_ui = list( 
    l = "Select feature columns",
    m = "Select feature columns to include when importing covariates. Unselected columns will be discarded."
  ),
  imputation_method_ui = list( 
    l = "Imputation method:",
    m = "Select the method for imputing missing values. Choices with 'neighbors' consider only covariate values in physically adjacent regions.  Choices with by date consider only covariate values for the same date."
  )
  
)

button_list_cs <-list(
  select_all = "Select all features present in the covariate data (other than the region and date columns).",
  unselect_all = "Unselect all features present in the covariate data.",
  filter = "Filter the covariate data to only the dates and/or regions present in the query data.",
  impute_missing = "Display data imputation options to fill in missing values.",
  run = "Run imputation to fill in missing values using the selected method.",
  add = "Add covariates by joining with the query data. This adds new columns to the query data that are available when fitting models.",
  cancel = "Discard changes to the covariate data without adding."
)

add_covariate_loader <- function(
    input,
    output,
    session,
    get_base_dt,
    set_base_dt,
    button_id = "cov_button_ui",
    button_label = "Add covariates",
    base_region_col = "countyfips",
    base_date_col = "date"
) {
  
  `%||%` <- function(a, b) if (!is.null(a)) a else b
  ns <- session$ns
  
  cov_dt       <- reactiveVal(NULL)
  cov_filtered <- reactiveVal(FALSE)
  
  # UI state: show/hide imputation controls
  show_impute_ui <- reactiveVal(FALSE)
  
  # cache adjacency matrix (loaded only when needed)
  adj_mat_val <- reactiveVal(NULL)
  
  # ---- Helpers ----
  normalize_fips <- function(x) {
    x <- sprintf("%05s", as.character(x))
    gsub(" ", "0", x, fixed = TRUE)
  }
  
  show_missing_warnings <- function(dt, cols) {
    cols <- intersect(cols, names(dt))
    if (length(cols) == 0) return(invisible(NULL))
    
    na_counts <- vapply(cols, function(nm) sum(is.na(dt[[nm]])), integer(1))
    na_counts <- na_counts[na_counts > 0]
    if (length(na_counts) == 0) return(invisible(NULL))
    
    for (nm in names(na_counts)) {
      showNotification(
        paste0("Warning! missing ", na_counts[[nm]], " values found in column ", nm, "."),
        type = "warning",
        duration = NULL
      )
    }
    
    invisible(NULL)
  }
  
  warn_categorical <- function(dt, cols) {
    cols <- intersect(cols, names(dt))
    bad <- cols[!vapply(cols, function(nm) is.numeric(dt[[nm]]), logical(1))]
    if (length(bad) > 0) {
      showNotification(
        paste0(
          "Warning: categorical/non-numeric columns cannot be imputed and will be skipped: ",
          paste(bad, collapse = ", ")
        ),
        type = "warning",
        duration = NULL
      )
    }
    invisible(bad)
  }
  
  get_adj_mat <- function() {
    m <- adj_mat_val()
    if (!is.null(m)) return(m)
    m <- load_adj_matrix(PHYS_ADJ_MATRIX)
    adj_mat_val(m)
    m
  }
  
  null_if_blank <- function(x) {
    if (is.null(x) || length(x) == 0) return(NULL)
    x <- as.character(x)
    if (!nzchar(x[1])) return(NULL)
    x[1]
  }
  impute_selected <- function(dt, region_col = NULL, date_col = NULL, feature_cols, method) {
    dt <- data.table::copy(dt)
    
    
    region_col <- null_if_blank(region_col)
    date_col   <- null_if_blank(date_col)

    # require at least one
    if (is.null(region_col) && is.null(date_col)) {
      cli::cli_abort("impute_selected(): must provide region_col and/or date_col (not both NULL).")
    }
    feature_cols <- intersect(feature_cols, names(dt))
    if (length(feature_cols) == 0) return(dt)
    # skip non-numeric with warning
    nonnum <- feature_cols[!vapply(feature_cols, function(nm) is.numeric(dt[[nm]]), logical(1))]
    if (length(nonnum) > 0) {
      showNotification(
        paste0(
          "Warning: categorical/non-numeric columns cannot be imputed and will be skipped: ",
          paste(nonnum, collapse = ", ")
        ),
        type = "warning",
        duration = NULL
      )
    }
    feature_cols <- setdiff(feature_cols, nonnum)
    if (length(feature_cols) == 0) return(dt)
    
    # validate provided columns exist
    if (!is.null(region_col) && !(region_col %in% names(dt))) cli::cli_abort("region_col not found in dt: ", region_col)
    if (!is.null(date_col)   && !(date_col %in% names(dt)))   cli::cli_abort("date_col not found in dt: ", date_col)
    
    # normalize region/date if present
    if (!is.null(region_col)) dt[, (region_col) := normalize_fips(get(region_col))]
    if (!is.null(date_col))   dt[, (date_col) := as.Date(get(date_col))]
    
    # NEW: method sets whether we group by date and/or use neighbors
    method_overall <- method %in% c("mean_overall", "median_overall", "mean_by_date", "median_by_date")
    method_neighbors <- method %in% c("mean_neighbors", "median_neighbors", "mean_neighbors_by_date", "median_neighbors_by_date")
    if (!method_overall && !method_neighbors) {
      cli::cli_abort("Unknown imputation method: ", method)
    }
    
    # NEW: date grouping is EXPLICIT (only for *_by_date methods)
    by_date <- method %in% c("mean_by_date", "median_by_date", "mean_neighbors_by_date", "median_neighbors_by_date")
    if (by_date && is.null(date_col)) {
      cli::cli_abort("This method requires date_col: ", method)
    }
    
    # NEW: neighbors methods require region_col
    if (method_neighbors && is.null(region_col)) {
      cli::cli_abort("Neighbor-based imputation requires region_col: ", method)
    }
    
    #  grouping columns depend on METHOD, not merely on presence of date_col
    by_cols <- if (by_date) date_col else character(0)
    
    # helper: compute fill statistic (NEW: reduces repetition)
    stat_fill <- function(x) {
      if (grepl("^mean", method)) mean(x, na.rm = TRUE) else stats::median(x, na.rm = TRUE)
    }
    
    # -------------------------
    # Overall mean/median (global or within-date)
    # -------------------------
    if (method_overall) {
      for (nm in feature_cols) {
        fills <- dt[, .(fill = stat_fill(get(nm))), by = by_cols]
        
        # join fills back by group (or single value if no grouping)
        if (length(by_cols) > 0) {
          dt[fills, fill := i.fill, on = by_cols]
        } else {
          dt[, fill := fills$fill[1]]
        }
        
        idx <- which(is.na(dt[[nm]]) & !is.na(dt[["fill"]]) & !is.nan(dt[["fill"]]))
        if (length(idx) > 0) data.table::set(dt, i = idx, j = nm, value = dt[["fill"]][idx])
        
        dt[, fill := NULL]
      }
      return(dt)
    }
    # -------------------------
    # Neighbor mean/median (overall or by-date)
    # -------------------------
    if (method_neighbors) {
      mat <- get_adj_mat()
      
      # only neighbors that exist in current dt contribute
      geoids_present <- unique(dt[[region_col]])
      
      # key for fast lookups; if by_cols empty => just region
      key_cols <- c(by_cols, region_col)
      data.table::setkeyv(dt, key_cols)
      
      for (nm in feature_cols) {
        
        # targets: unique combos of (date?, region) where nm is missing
        if (length(by_cols) > 0) {
          targets <- unique(dt[is.na(get(nm)), c(by_cols, region_col), with = FALSE])
        } else {
          targets <- data.table::data.table(tmp_region = unique(dt[is.na(get(nm)), get(region_col)]))
          data.table::setnames(targets, "tmp_region", region_col)
        }
        
        if (nrow(targets) == 0) next
        
        for (i in seq_len(nrow(targets))) {
          f <- targets[[region_col]][i]
          if (is.null(rownames(mat)) || !(f %in% rownames(mat))) next
          
          neigh <- intersect(get_neighbors(f, mat), geoids_present)
          if (length(neigh) == 0) next
          
          # restrict to same date if by_cols present
          if (length(by_cols) > 0) {
            dval <- targets[[by_cols]][i]  # since by_cols is "date"
            ivals <- data.table::data.table(tmp_date = dval, tmp_region = neigh)
            data.table::setnames(ivals, c("tmp_date", "tmp_region"), c(by_cols, region_col))
            
            vals <- dt[ivals, get(nm), on = c(by_cols, region_col)]
          } else {
            vals <- dt[.(neigh), get(nm)]
          }
          
          vals <- vals[!is.na(vals)]
          if (length(vals) == 0) next  # leave NA
          
          fill <- stat_fill(vals)
          if (is.nan(fill)) next
          
          # assign to all matching rows (same date? + same region) that are NA
          if (length(by_cols) > 0) {
            dval <- targets[[by_cols]][i]
            idx <- which(dt[[by_cols]] == dval & dt[[region_col]] == f & is.na(dt[[nm]]))
          } else {
            idx <- which(dt[[region_col]] == f & is.na(dt[[nm]]))
          }
          
          if (length(idx) > 0) data.table::set(dt, i = idx, j = nm, value = fill)
        }
      }
      
      return(dt)
    }
    
    dt
  }
  
  
  
  # ---- File reader ----
  read_to_dt <- function(path, name) {
    ext <- tolower(tools::file_ext(name))
    
    if (ext == "csv") {
      return(data.table::as.data.table(readr::read_csv(path, show_col_types = FALSE)))
    }
    if (ext %in% c("xlsx", "xls")) {
      return(data.table::as.data.table(readxl::read_excel(path)))
    }
    if (ext == "parquet") {
      return(data.table::as.data.table(arrow::read_parquet(path)))
    }
    cli::cli_abort("Unsupported file type: ", ext)
  }
  
  
  reset_cov_modal_state <- function() {
    # Reset selectors
    updateSelectInput(session, "cov_region_col", selected = "")
    updateSelectInput(session, "cov_date_col", selected = "")
    
    
    # Reset features
    updateSelectizeInput(session, "cov_features", selected = character(0))
    
    # Reset method (and/or hide impute UI via your own flag if you have one)
    updateSelectInput(session, "cov_impute_method", selected = "mean_overall")
    show_impute_ui(FALSE)
    cov_dt(NULL)
  }
  
  
  # ---- Open modal ----
  observe({
    req(input[[button_id]])
    cov_filtered(FALSE)
    show_impute_ui(FALSE)
    
    showModal(
      modalDialog(
        title = button_label,
        size = "l",
        easyClose = TRUE,
        fade = TRUE,
        fileInput(
          ns("cov_file"),
          labeltt(label_list_cs[['upload_covariates']]),
          accept = c(".csv", ".xlsx", ".xls", ".parquet")
        ),
        
        uiOutput(ns("cov_region_col_ui")),
        uiOutput(ns("cov_date_col_ui")),
        uiOutput(ns("cov_feature_cols_ui")),
        
        fluidRow(
          column(
            12,
            div(
              style = "
                margin-top: 6px;
                margin-bottom: 6px;
                display: flex;
                gap: 8px;
              ",
              add_button_hover(title = button_list_cs[["select_all"]],actionButton(ns("cov_select_all"), "Select all", class = "btn-primary")),
              add_button_hover(title = button_list_cs[["unselect_all"]],actionButton(ns("cov_unselect_all"), "Unselect all", class = "btn-primary"))
            )
          )
        ),
        
        uiOutput(ns("cov_impute_controls_ui")),
        
        tags$hr(),
        
        # --- controls above the table ---
        fluidRow(
          column(
            12,
            div(
              style = "display:flex; align-items:center; gap:10px; flex-wrap:wrap;",
              add_button_hover(title = button_list_cs[["filter"]],actionButton(ns("cov_filter"), "Filter", class = "btn-primary")),
              add_button_hover(title = button_list_cs[["impute_missing"]],actionButton(ns("cov_impute_toggle"), "Impute missing", class = "btn-primary"))
            )
          )
        ),
        
        # Impute method UI (appears after clicking Impute missing)
        uiOutput(ns("cov_impute_controls_ui")),
        
        div(style = "margin-top: 8px;"),
        strong("Preview:"),
        DT::DTOutput(ns("cov_preview_dt")),
        
        footer = tagList(
          add_button_hover(title = button_list_cs[["cancel"]],actionButton(ns("cov_cancel"), "Cancel", class = "btn-primary")),
          add_button_hover(title = button_list_cs[["add"]],actionButton(ns("cov_add"), "Add", class = "btn-primary"))
        )
      )
    )
  }) |> bindEvent(input[[button_id]])
  
  # ---- Impute controls UI (hidden until toggle) ----
  output$cov_impute_controls_ui <- renderUI({
    if (!isTRUE(show_impute_ui())) return(NULL)
    
    fluidRow(
      column(
        12,
        div(
          style = "display:flex; align-items:center; gap:12px; margin-top: 6px;",
          tags$label(labeltt(label_list_cs[['imputation_method_ui']]), style = "margin:0; font-weight:600;"),
          selectInput(
            ns("cov_impute_method"),
            label = NULL,#labeltt(label_list_cs[['imputation_method_ui']]),
            choices = c(
              "Mean (overall)" = "mean_overall",
              "Median (overall)" = "median_overall",
              "Mean (by date)" = "mean_by_date",
              "Median (by date)" = "median_by_date",
              "Mean (neighbors overall)" = "mean_neighbors",
              "Median (neighbors overall)" = "median_neighbors",
              "Mean (neighbors by date)" = "mean_neighbors_by_date",
              "Median (neighbors by date)" = "median_neighbors_by_date"
            ),
            selected = "mean_overall",
            width = "260px"
          ),
          add_button_hover(title = button_list_cs[["run"]],actionButton(ns("cov_impute_run"), "Run",class = "btn-primary"))
        )
      )
    )
  })
  
  # ---- Toggle impute controls ----
  observe({
    req(input$cov_impute_toggle)
    show_impute_ui(!isTRUE(show_impute_ui()))
  }) |> bindEvent(input$cov_impute_toggle)
  
  # ---- Load uploaded file ----
  observe({
    req(input$cov_file)
    
    dt <- tryCatch(
      read_to_dt(input$cov_file$datapath, input$cov_file$name),
      error = function(e) {
        showNotification(paste("Read failed:", e$message), type = "error", duration = NULL)
        NULL
      }
    )
    req(dt)
    
    if (anyDuplicated(names(dt)) > 0) {
      data.table::setnames(dt, make.unique(names(dt)))
    }
    
    cov_dt(dt)
    cov_filtered(FALSE)
    
    cols <- names(cov_dt())
    
    output$cov_region_col_ui <- renderUI({
      selectInput(
        ns("cov_region_col"),
        label = labeltt(label_list_cs[["region_col_ui"]]),
        choices = c(
          "None" = "",
          cols
        ),
        selected = if ("countyfips" %in% cols) "countyfips" else ""
      )
    })
    
    
    output$cov_date_col_ui <- renderUI({
      selectInput(
        ns("cov_date_col"),
        label = labeltt(label_list_cs[["date_col_ui"]]),
        choices = c(
          "None" = "",
          cols
        ),
        selected = if ("date" %in% cols) "date" else ""
      )
    })
    
    
    output$cov_feature_cols_ui <- renderUI({
      req(cov_dt())
      
      cols2 <- names(cov_dt())
      region_col <- input$cov_region_col %||% ""
      date_col <- input$cov_date_col %||% ""
      feat_choices <- setdiff(cols2, c(region_col,date_col))
      
      current <- isolate(input$cov_features) %||% feat_choices
      selected <- intersect(current, feat_choices)
      
      selectizeInput(
        ns("cov_features"),
        labeltt(label_list_cs[["feature_col_ui"]]),
        choices  = feat_choices,
        selected = selected,
        multiple = TRUE,
        width    = "100%",
        options  = list(
          plugins = list("remove_button"),
          placeholder = "Type to search columns…"
        )
      )
    })
  }) |> bindEvent(input$cov_file)
  
  # ---- Keep feature choices synced when fips changes ----
  observe({
    req(cov_dt())
    
    cols <- names(cov_dt())
    region_col <- null_if_blank(input$cov_region_col)
    date_col   <- null_if_blank(input$cov_date_col)
    
    region_ok <- !is.null(region_col) && nzchar(region_col) && (region_col %in% cols)
    date_ok   <- !is.null(date_col)   && nzchar(date_col)   && (date_col %in% cols)
    
    req(region_ok || date_ok)
    
    join_cols <- c(
      if (region_ok) region_col else NULL,
      if (date_ok)   date_col   else NULL
    )
    
    feat_choices <- setdiff(cols, join_cols)
    current <- isolate(input$cov_features) %||% feat_choices
    current <- current[!is.na(current) & nzchar(current)]
    selected <- intersect(current, feat_choices)

    updateSelectizeInput(
      session,
      "cov_features",
      choices = feat_choices,
      selected = selected,
      server = TRUE
    )
    
    if (is.null(region_col)) return()
    
    dt <- data.table::copy(cov_dt())
    tryCatch({
      dt[, (region_col) := normalize_fips(get(region_col))]
    }, error = function(msg){"Error parsing region column. Skipping."}
    )
    cov_dt(dt)
  }) |> bindEvent(input$cov_region_col)
  
  
  observe({
    req(cov_dt())
    cols <- names(cov_dt())
    region_col <- null_if_blank(input$cov_region_col)
    date_col   <- null_if_blank(input$cov_date_col)
    
    region_ok <- !is.null(region_col) && nzchar(region_col) && (region_col %in% cols)
    date_ok   <- !is.null(date_col)   && nzchar(date_col)   && (date_col %in% cols)
    
    req(region_ok || date_ok)
    
    join_cols <- c(
      if (region_ok) region_col else NULL,
      if (date_ok)   date_col   else NULL
    )
    feat_choices <- setdiff(cols, join_cols)
    current <- isolate(input$cov_features) %||% feat_choices
    current <- current[!is.na(current) & nzchar(current)]
    selected <- intersect(current, feat_choices)
    
    updateSelectizeInput(
      session,
      "cov_features",
      choices = feat_choices,
      selected = selected,
      server = TRUE
    )
    
    if (is.null(date_col)) return()
    
    dt <- data.table::copy(cov_dt())
    tryCatch({
      dt[, (date_col) :=as.Date(get(date_col))]
    }, error = function(msg){"Error parsing date column. Skipping"}
    )
    cov_dt(dt)
  })|> bindEvent(input$cov_date_col)
  
  
  # ---- Select all / unselect all ----
  observe({
    req(input$cov_select_all)
    req(cov_dt())
    
    cols <- names(cov_dt())
    region_col <- null_if_blank(input$cov_region_col)
    date_col   <- null_if_blank(input$cov_date_col)
    
    feats <- setdiff(cols, c(region_col,date_col))
    
    updateSelectizeInput(
      session,
      "cov_features",
      choices = feats,
      selected = feats,
      server = TRUE
    )
  }) |> bindEvent(input$cov_select_all)
  
  observe({
    req(input$cov_unselect_all)
    req(cov_dt())
    
    cols <- names(cov_dt())
    region_col <- null_if_blank(input$cov_region_col)
    date_col   <- null_if_blank(input$cov_date_col)
    
    feats <- setdiff(cols, c(region_col, date_col))
    
    updateSelectizeInput(
      session,
      "cov_features",
      choices = feats,
      selected = character(0),
      server = TRUE
    )
  }) |> bindEvent(input$cov_unselect_all)
  
  # ---- Preview ----
  preview_dt <- reactive({
    req(cov_dt())
    dt <- cov_dt()
    
    cols <- names(dt)
    region_col <- null_if_blank(input$cov_region_col)
    date_col   <- null_if_blank(input$cov_date_col)
    
    feats <- input$cov_features %||% character(0)
    
    keep <- unique(c(date_col,region_col, feats))
    keep <- intersect(keep, cols)
    
    dt[, keep, with = FALSE]
  })
  
  output$cov_preview_dt <- DT::renderDT({
    DT::datatable(
      preview_dt(),
      rownames = FALSE,
      options = list(
        scrollX = TRUE,
        scrollY = "45vh",
        pageLength = 10,
        lengthMenu = c(10, 25, 50, 100)
      )
    )
  }, server = TRUE)
  
  # ---- Filter ----
  observe({
    req(input$cov_filter)
    req(cov_dt())
    
    base_dt0 <- get_base_dt()
    req(!is.null(base_dt0))
    req(base_region_col %in% names(base_dt0))
    
    up_dt <- data.table::copy(cov_dt())
    
    region_col <- input$cov_region_col
    date_col   <- input$cov_date_col
    
    # Treat "" as missing (common with selectInput)
    region_col <- if (!is.null(region_col) && nzchar(region_col)) region_col else NULL
    date_col   <- if (!is.null(date_col)   && nzchar(date_col))   date_col   else NULL

    # Require at least one of region/date
    req(!is.null(region_col) || !is.null(date_col))
    
    # Validate provided columns exist
    if (!is.null(region_col)) req(region_col %in% names(up_dt))
    if (!is.null(date_col))   req(date_col %in% names(up_dt))
    
    before <- nrow(up_dt)
    # ---- Region filter (if provided) ----
    if (!is.null(region_col)) {
      up_dt[, (region_col) := normalize_fips(get(region_col))]
      base_fips <- normalize_fips(base_dt0[[base_region_col]])
      up_dt <- up_dt[get(region_col) %in% base_fips]
    }
    # ---- Date filter (if provided) ----
    if (!is.null(date_col)) {
      up_dt[, (date_col) := as.Date(get(date_col))]
      base_dates <- as.Date(base_dt0[[base_date_col]])
      
      up_dt <- up_dt[get(date_col) %in% base_dates]
    }
    
    after <- nrow(up_dt)
    
    cov_dt(up_dt)
    cov_filtered(TRUE)
    
    feats_now <- input$cov_features %||% character(0)
    show_missing_warnings(up_dt, feats_now)
    
    showNotification(
      paste("Filtered:", after, "of", before, "rows kept."),
      type = "message"
    )
  }) |> bindEvent(input$cov_filter)
  
  

  # ---- Impute run ----
  observe({
    req(input$cov_impute_run)
    req(cov_dt())
    
    dt_before <- data.table::copy(cov_dt())
    dt_before[, .__row_id__ := .I]   # stable alignment for comparison
    
    # CHANGED: both can be NULL individually, but not both
    region_col <- input$cov_region_col
    date_col   <- input$cov_date_col
    
    # NEW: Treat "" as missing (common with selectInput)
    region_col <- if (!is.null(region_col) && nzchar(region_col)) region_col else NULL
    date_col   <- if (!is.null(date_col)   && nzchar(date_col))   date_col   else NULL
    
    # NEW: require at least one of them
    req(!is.null(region_col) || !is.null(date_col))
    
    # NEW: validate provided columns exist
    if (!is.null(region_col)) req(region_col %in% names(dt_before))
    if (!is.null(date_col))   req(date_col %in% names(dt_before))
    
    feats <- input$cov_features %||% character(0)
    feats <- intersect(feats, names(dt_before))
    
    warn_categorical(dt_before, feats)
    
    method <- input$cov_impute_method
    dt_after <- tryCatch(
      impute_selected(
        data.table::copy(dt_before[, .__row_id__ := NULL]),
        region_col   = region_col,
        date_col     = date_col,
        feature_cols = feats,
        method       = method
      ),
      error = function(e) {
        showNotification(
          paste("Impute failed:", conditionMessage(e)),
          type = "error",
          duration = NULL
        )
        NULL
      }
    )
    req(!is.null(dt_after))
    
    # Add row_id back for alignment
    dt_after <- data.table::copy(dt_after)
    dt_after[, .__row_id__ := .I]
    
    # Ensure tracking column exists
    if (!("imputed_features" %in% names(dt_after))) {
      dt_after[, imputed_features := ""]
    } else {
      dt_after[is.na(imputed_features), imputed_features := ""]
    }
    
    # Keep only numeric features (safety)
    feats_num <- feats[vapply(feats, function(nm) is.numeric(dt_before[[nm]]), logical(1))]
    if (length(feats_num) == 0) {
      cov_dt(dt_after[, .__row_id__ := NULL])
      showNotification("Imputation applied (no numeric features selected).", type = "message")
      return()
    }
    
    # Compare before vs after: NA -> non-NA
    before_mat <- as.matrix(dt_before[, ..feats_num])
    after_mat  <- as.matrix(dt_after[,  ..feats_num])
    
    imputed_mask <- is.na(before_mat) & !is.na(after_mat)
    n_imputed_cells <- sum(imputed_mask)
    
    
    
    # Build per-row list of imputed feature names
    if (n_imputed_cells > 0) {
      imp_by_row <- apply(imputed_mask, 1, function(rowmask) paste(feats_num[rowmask], collapse = ","))
      # imp_by_row is "" when nothing was imputed in that row
      
      idx <- which(nzchar(imp_by_row))
      if (length(idx) > 0) {
        dt_after[idx, imputed_features := ifelse(
          imputed_features == "",
          imp_by_row[idx],
          paste0(imputed_features, ",", imp_by_row[idx])
        )]
      }
    }
    
    # Clean up temp id
    dt_after[, .__row_id__ := NULL]
    
    
    cov_dt(dt_after)
    
    show_missing_warnings(dt_after, feats)
    
    showNotification(
      paste0(
        "Imputation applied (", method, "): ",
        format(n_imputed_cells, big.mark = ","), " cell", if (n_imputed_cells == 1) "" else "s",
        " filled."
      ),
      type = "message"
    )
  }) |> bindEvent(input$cov_impute_run)
  
  
  # ---- Add ----
  observe({
    req(input$cov_add)
    req(cov_dt())
    
    base_dt <- data.table::copy(get_base_dt())
    req(!is.null(base_dt))
    req(data.table::is.data.table(base_dt))
    
    up_dt <- data.table::copy(cov_dt())
    
    # NEW: allow either/both keys
    region_col <- input$cov_region_col
    date_col   <- input$cov_date_col
    
    region_col <- if (!is.null(region_col) && nzchar(region_col)) region_col else NULL
    date_col   <- if (!is.null(date_col)   && nzchar(date_col))   date_col   else NULL
    
    req(!is.null(region_col) || !is.null(date_col))
    
    # NEW: validate key columns exist in both tables (as applicable)
    if (!is.null(region_col)) {
      req(region_col %in% names(up_dt))
      req(base_region_col %in% names(base_dt))
    }
    
    if (!is.null(date_col)) {
      req(date_col %in% names(up_dt))
    }
    
    feats <- input$cov_features %||% character(0)
    feats <- intersect(feats, names(up_dt))
    
    # NEW/CHANGED: normalize join keys on both sides
    if (!is.null(region_col)) {
      up_dt[, (region_col) := normalize_fips(get(region_col))]
    }
    if (!is.null(date_col)) {
      up_dt[, (date_col) := as.Date(get(date_col))]
    }
    
    # Build join_dt with keys + features
    key_cols_up <- c(region_col, date_col)
    key_cols_up <- key_cols_up[!is.null(key_cols_up)]
    
    join_cols <- unique(c(key_cols_up, feats))
    join_cols <- intersect(join_cols, names(up_dt))
    join_dt <- up_dt[, join_cols, with = FALSE]
    
    # Rename keys in join_dt to match base_dt key names
    if (!is.null(region_col) && region_col != base_region_col) {
      data.table::setnames(join_dt, region_col, base_region_col)
    }
    if (!is.null(date_col) && date_col != base_date_col) {
      data.table::setnames(join_dt, date_col, base_date_col)
    }
    
    # Preserve original base_dt row order
    base_dt[, .__row_id__ := .I]
    
    # NEW: merge by whichever keys exist
    by_keys <- c(
      if (!is.null(region_col)) base_region_col,
      if (!is.null(date_col))   base_date_col
    )
    
    merged <- merge(
      base_dt,
      join_dt,
      by = by_keys,
      all.x = TRUE,
      sort = FALSE,
      allow.cartesian = TRUE
    )
    
    data.table::setorder(merged, .__row_id__)
    merged[, .__row_id__ := NULL]
    
    new_cols <- setdiff(names(merged), names(base_dt))
    show_missing_warnings(merged, new_cols)
    
    set_base_dt(merged)
    reset_cov_modal_state()
    removeModal()
    showNotification("Covariates added to base table.", type = "message")
  }) |> bindEvent(input$cov_add)
  
  observe({
    reset_cov_modal_state()
    removeModal()
  }) |> bindEvent(input$cov_cancel)
  
}

