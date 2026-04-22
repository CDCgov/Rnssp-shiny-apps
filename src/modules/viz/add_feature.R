# © 2025 The Johns Hopkins University Applied Physics Laboratory LLC
# Development of this software was sponsored by the U.S. Government under
# contract no. 75D30124C19958

label_list_add_feature <- list(
  feature_type = list(
    l = "Select Feature Type",
    m = "Choose the type of derived posterior feature to create and store for later display."
  ),
  feature_scale = list(
    l = "Scale",
    m = "Choose whether the feature should be calculated on the count scale or the proportion scale."
  ),
  quantile = list(
    l = "Quantile (0-1)",
    m = "Enter the posterior quantile to calculate. For example, 0.5 is the median."
  ),
  confidence_level = list(
    l = "Confidence level (0-1)",
    m = "Enter the central credible interval level to calculate. For example, 0.95 gives the 95% interval."
  ),
  exceedance_threshold = list(
    l = "Exceedance threshold",
    m = "Enter the threshold value for calculating the posterior probability that the feature exceeds it."
  ),
  change_mode = list(
    l = "Change type",
    m = "Choose whether the change threshold is interpreted as an absolute count increase, an absolute proportion increase, or a relative count increase."
  ),
  change_threshold = list(
    l = "Change threshold",
    m = "Enter the minimum increase required when calculating the posterior probability of change."
  ),
  change_normal_approx = list(
    l = "Use normal approximation (faster)",
    m = "Use the faster normal-approximation method when computing change probabilities. Leave this off to use the full numerical integration."
  ),
  lookback_value = list(
    l = "Lookback",
    m = "Enter the number of time steps between the target date and the reference date. The label updates automatically to match the temporal resolution of the data."
  ),
  auto_labels = list(
    l = "Auto-generate name/description",
    m = "Automatically fill the feature name and description based on the selected feature type, scale, and parameter values."
  ),
  feature_name = list(
    l = "Feature name",
    m = "Name shown in feature lists, plots, and tables."
  ),
  feature_desc = list(
    l = "Feature description",
    m = "Short description of what this derived feature represents."
  ),
  displayed_columns = list(
    l = "Displayed columns",
    m = "Choose which stored feature columns are shown in the Add Feature table."
  ),
  table_decimals = list(
    l = "Table decimals",
    m = "Set the number of decimal places shown for non-integer numeric values in the table."
  )
)

add_feature_ui <- function(id) {
  ns <- NS(id)
  feature_id <- ns("feature")
  nav_panel(
    title = "Add Feature",
    value = "add_feature",
    tags$style(HTML(sprintf("
      #%s .bslib-sidebar-layout > .sidebar {
        transition: width 150ms ease, padding 150ms ease, margin 150ms ease;
      }
      #%s.sidebar-collapsed .bslib-sidebar-layout > .sidebar {
        width: 0;
        padding: 0;
        margin: 0;
        overflow: hidden;
      }
      #%s.sidebar-collapsed .bslib-sidebar-layout > .main {
        width: 100%%;
      }
    ", ns("viz_wrap"), ns("viz_wrap"), ns("viz_wrap")))) ,
    div(
      id = ns("viz_wrap"),
      # Add feature LHS collapsible panel
      bslib::layout_sidebar(
        sidebar = bslib::sidebar(
          id    = ns("config_sidebar"),
          width = SIDEBAR_WIDTH * 2,
          div(
            style = "display:flex; align-items:center; justify-content:space-between; gap:.5rem;",
            strong("Controls / Filters"),
            actionButton(ns("toggle_sidebar"), "Hide", class = "btn btn-sm btn-outline-secondary")
          ),
          # Dropdown tied to conditional panels
          selectInput(
            inputId = ns("feature"),
            label = labeltt(label_list_add_feature[["feature_type"]]),
            choices = c(
              "Mean" = "mean",
              "Quantile" = "quantile",
              "Confidence Interval" = "confidence_interval",
              "Exceedance Probability" = "exceedance_probability",
              "Change Probability" = "change_probability"
            ),
            selected = "mean"
          ),
          # Scale selection
          conditionalPanel(
            condition = sprintf("input['%s'] != 'change_probability'", feature_id),
            radioButtons(
              inputId = ns("feature_scale"),
              label = labeltt(label_list_add_feature[["feature_scale"]]),
              choices = c("Counts" = "counts", "Proportion" = "proportion"),
              selected = "counts",
              inline = TRUE
            )
          ),
          
          # Conditional panels
          conditionalPanel(
            condition = sprintf("input['%s'] == 'quantile'", feature_id),
            numericInput(ns("q_val"), labeltt(label_list_add_feature[["quantile"]]), value = 0.20, min = 0, max = 1, step = 0.01)
          ),
          conditionalPanel(
            condition = sprintf("input['%s'] == 'confidence_interval'", feature_id),
            numericInput(ns("ci_val"), labeltt(label_list_add_feature[["confidence_level"]]), value = 0.90, min = 0, max = 1, step = 0.01)
          ),
          conditionalPanel(
            condition = sprintf("input['%s'] == 'exceedance_probability'", feature_id),
            numericInput(ns("exceed_val"), labeltt(label_list_add_feature[["exceedance_threshold"]]), value = 0, min = 0, step = 0.01)
          ),
          conditionalPanel(
            condition = sprintf("input['%s'] == 'change_probability'", feature_id),
            tagList(
              uiOutput(ns("change_mode_ui")),
              numericInput(ns("change_threshold"), labeltt(label_list_add_feature[["change_threshold"]]), value = 0, min = 0, step = 0.01),
              checkboxInput(ns("change_use_normal_approx"), labeltt(label_list_add_feature[["change_normal_approx"]]), value = FALSE),
              uiOutput(ns("change_lookback_ui"))
            )
          ),
          # Free text fields
          checkboxInput(ns("auto_labels"), labeltt(label_list_add_feature[["auto_labels"]]), value = TRUE),
          textInput(ns("feature_name"), labeltt(label_list_add_feature[["feature_name"]]), value = ""),
          textInput(ns("feature_desc"), labeltt(label_list_add_feature[["feature_desc"]]), value = ""),
          # Add feature + select feature(s)
          add_button_hover(
            title = "Add the selected feature to the table",
            actionButton(ns("add_feature"), "Add Feature", class = "btn-primary btn-sm")
          ),
          tags$hr(),
          selectizeInput(
            inputId = ns("display_cols"),
            label = labeltt(label_list_add_feature[["displayed_columns"]]),
            choices = character(0),
            selected = character(0),
            multiple = TRUE,
            options = list(
              placeholder = "Select columns to display…",
              plugins = list("remove_button")
            )
          ),
          add_button_hover(
            title = "Open delete feature window",
            actionButton(ns("delete_feature"), "Open delete feature window", class = "btn-primary btn-sm")
          ),
          # Decimal selection
          numericInput(ns("dt_digits"), labeltt(label_list_add_feature[["table_decimals"]]), value = 2, min = 0, max = 10, step = 1)
        ),
        # Reactable Table RHS
        bslib::card(
          bslib::card_header(div(
              style = "display:flex; align-items:center; justify-content:space-between; width:100%;",
              span("Posterior Data")
            ),
          class = "bg-primary"),
          bslib::card_body(
            reactable::reactableOutput(ns("posterior_data"), width = "100%"),
            div(style="margin-top:10px;",
              downloadButton(ns("download_posterior_csv"), "Download CSV", class = "btn-primary btn-sm")
            )
          )
        )
      )
    )
  )
}


add_feature_server <- function(id, dc = NULL, im = NULL, results = NULL, feature_store) {
  moduleServer(id, function(input, output, session) {
    options(shiny.fullstacktrace = TRUE)
    ns <- session$ns
    
    store <- reactive({
      if (is.function(feature_store)) feature_store() else feature_store
    })
    
    rvr <- reactive({
      store()$rv
    })
    
    feature_tbl <- reactive({
      s <- store()
      req(s)
      as.data.frame(s$features_df())
    })
    
    refresh_r <- reactive({
      rvr()$refresh
    })
    
    order_r <- reactive({
      rvr()$order
    })
    
    is_user_feature <- function(fid) grepl("^usr__", fid)
    
    is_calculated_feature <- function(f) {
      !is.null(f$feature_type) &&
        f$feature_type %in% c("mean", "quantile", "confidence_interval", "exceedance_probability", "change_probability")
    }
    
    quantile_feature_id <- function(scale, q) {
      paste0("builtin__quantile__", scale, "__q", fmt_qname(q))
    }
    
    quantile_feature_label <- function(scale, q) {
      sprintf("Quantile q=%s (%s)", fmt_num(q), scale)
    }
    
    change_scale_mode <- reactive({
      input$change_mode %||% "absolute_count"
    })

    model_scale_key <- reactive({
      req(im$model)
      sc <- tryCatch(epistemic::get_model_scale(im$model), error = function(e) NULL)
      if (is.null(sc) || !nzchar(sc)) return("count")
      sc
    })

    relative_change_label <- reactive({
      if (identical(model_scale_key(), "prop")) "Relative proportion" else "Relative count"
    })

    time_resolution <- reactive({
      tr <- dc$time_res %||% NULL
      if (is.null(tr) || !nzchar(tr)) return("daily")
      tr
    })

    lookback_unit_word <- reactive({
      if (identical(time_resolution(), "weekly")) "week" else "day"
    })

    lookback_unit_word_plural <- reactive({
      base <- lookback_unit_word()
      function(n) if (identical(as.integer(n), 1L)) base else paste0(base, "s")
    })

    change_feature_scale <- reactive({
      switch(
        change_scale_mode(),
        absolute_count = "counts",
        absolute_prop = "proportion",
        relative = if (identical(model_scale_key(), "prop")) "proportion" else "counts",
        "counts"
      )
    })

    lookback_label <- reactive({
      val <- as.integer(input$lookback_value %||% 1L)
      sprintf("%s %s", val, lookback_unit_word_plural()(val))
    })

    lookback_dt_days <- reactive({
      val <- max(1L, as.integer(input$lookback_value %||% 1L))
      if (identical(time_resolution(), "weekly")) val * 7L else val
    })

    change_threshold_label <- reactive({
      mode <- change_scale_mode()
      thr <- as.numeric(input$change_threshold %||% 0)
      if (identical(mode, "relative")) {
        sprintf("relative increase of at least %s", fmt_percent(thr))
      } else if (identical(mode, "absolute_prop")) {
        sprintf("%s proportion", fmt_num(thr))
      } else {
        sprintf("%s counts", fmt_num(thr))
      }
    })

    change_approx_suffix <- reactive({
      if (isTRUE(input$change_use_normal_approx)) " (approx)" else ""
    })

    output$change_mode_ui <- renderUI({
      selectInput(
        ns("change_mode"),
        labeltt(label_list_add_feature[["change_mode"]]),
        choices = stats::setNames(
          c("absolute_count", "absolute_prop", "relative"),
          c("Absolute count", "Absolute proportion", relative_change_label())
        ),
        selected = {
          cur <- isolate(input$change_mode %||% "absolute_count")
          if (cur %in% c("absolute_count", "absolute_prop", "relative")) cur else "absolute_count"
        }
      )
    })

    output$change_lookback_ui <- renderUI({
      numericInput(
        ns("lookback_value"),
        labeltt(list(
          l = sprintf("Lookback (%s)", paste0(lookback_unit_word(), if (identical(lookback_unit_word(), "day")) "s" else "s")),
          m = label_list_add_feature[["lookback_value"]][["m"]]
        )),
        value = 1,
        min = 1,
        step = 1
      )
    })

    observe({
      if (!identical(input$feature %||% "", "change_probability")) return()
      target_scale <- change_feature_scale()
      if (!identical(input$feature_scale %||% "", target_scale)) {
        updateRadioButtons(session, "feature_scale", selected = target_scale)
      }
    })
    
    # Sidebar open/close
    sidebar_open <- reactiveVal(TRUE)
    observe({
      sidebar_open(!sidebar_open())
      shinyjs::toggleClass(ns("viz_wrap"), "sidebar-collapsed", !sidebar_open())
      updateActionButton(session, "toggle_sidebar", label = if (sidebar_open()) "Hide" else "Show")
    }) |> bindEvent(input$toggle_sidebar, ignoreInit = TRUE)
    
    front_cols <- reactive({
      req(im$data_cls)
      d <- im$data_cls
      unique(c("region", d$region_column, d$date_column, d$numerator_column, d$denominator_column))
    })
    
    ordered_choices_named <- reactive({
      ft <- feature_tbl()
      if (nrow(ft) == 0) return(stats::setNames(character(0), character(0)))
      stats::setNames(ft$id, ft$label)
    })
    
    default_display_feature_ids <- reactive({
      r <- rvr()
      req(front_cols())
      fids <- r$order %||% character(0)
      if (!length(fids)) return(character(0))
      
      keep <- vapply(fids, function(fid) {
        f <- r$features[[fid]]
        if (is.null(f)) return(FALSE)
        any((f$out_cols %||% character(0)) %in% front_cols())
      }, logical(1))
      
      fids[keep]
    })
    
    # Build the default label/description shown in the Add Feature form for
    # the currently selected feature type and parameters.
    default_feature_spec <- reactive({
      ft <- input$feature
      feature_scale <- if (identical(ft, "change_probability")) change_feature_scale() else (input$feature_scale %||% "counts")
      ft_name <- switch(
        ft,
        mean = sprintf("Posterior Mean (%s)", feature_scale),
        quantile = sprintf("Quantile q=%s (%s)",
          fmt_num(input$q_val %||% 0.50),
                           feature_scale),
        confidence_interval = sprintf("%s CI (%s)",
          fmt_num(input$ci_val %||% 0.90),
                                      feature_scale),
        exceedance_probability = sprintf("Exceedance probability with threshold %s (%s)",
          fmt_num(input$exceed_val %||% 0.0),
                                         feature_scale),
        change_probability = sprintf(
          "Change probability: %s over %s%s",
          change_threshold_label(),
          lookback_label(),
          change_approx_suffix()
        ),
        "Feature"
      )
      
      ft_desc <- switch(
        ft,
        mean = sprintf("Posterior mean of the fitted distribution on the %s scale.",
                       feature_scale),
        quantile = sprintf("Posterior quantile at q=%s on the %s scale.", 
          fmt_num(input$q_val %||% 0.50),
                           feature_scale),
        confidence_interval = sprintf("%s%% posterior credible interval on the %s scale.", 
          fmt_num(100 * (input$ci_val %||% 0.90)),
                                      feature_scale),
        exceedance_probability = sprintf(
          "Posterior probability that the value exceeds %s on the %s scale.",
          fmt_num(input$exceed_val %||% 0.0),
          feature_scale
        ),
        change_probability = switch(
          change_scale_mode(),
          absolute_count = sprintf(
            "Posterior probability that the count at the target date exceeds the count %s earlier by at least %s.",
            lookback_label(),
            fmt_num(input$change_threshold %||% 0)
          ),
          absolute_prop = sprintf(
            "Posterior probability that the proportion at the target date exceeds the proportion %s earlier by at least %s.",
            lookback_label(),
            fmt_num(input$change_threshold %||% 0)
          ),
          relative = sprintf(
            "Posterior probability that the %s at the target date is at least %s greater than the %s %s earlier.",
            if (identical(change_feature_scale(), "proportion")) "proportion" else "count",
            fmt_percent(input$change_threshold %||% 0),
            if (identical(change_feature_scale(), "proportion")) "proportion" else "count",
            lookback_label()
          ),
          ""
        ),
        ""
      )
      
      list(name = ft_name, desc = ft_desc)
    })
    
    observe({
      req(input$auto_labels)
      if (!isTRUE(input$auto_labels)) return()
      
      spec <- default_feature_spec()
      updateTextInput(session, "feature_name", value = spec$name)
      updateTextInput(session, "feature_desc", value = spec$desc)
    }
    )|>
      bindEvent(list(input$feature, input$q_val, input$ci_val, input$exceed_val, input$feature_scale,
                     input$change_mode, input$change_threshold, input$change_use_normal_approx,
                     input$lookback_value, time_resolution(), model_scale_key()),
                ignoreInit = FALSE)
    
    observe({
      # When switching auto back on, immediately sync to current params
      if (isTRUE(input$auto_labels)) {
        spec <- default_feature_spec()
        updateTextInput(session, "feature_name", value = spec$name)
        updateTextInput(session, "feature_desc", value = spec$desc)
      }
    }) |> bindEvent(input$auto_labels, ignoreInit = TRUE)
    
    # Return any posterior quantile probabilities needed to calculate the
    # selected feature type.
    need_probs <- function(ft) {
      if (ft == "quantile") return(round(input$q_val %||% 0.50, 3))
      if (ft == "confidence_interval") {
        lvl <- input$ci_val %||% 0.90
        a <- (1 - lvl) / 2
        return(round(c(a, 1 - a), 3))
      }
      numeric(0)
    }
    # all quantiles collected together
    all_probs <- reactive({
      r <- rvr()
      p <- sort(unique(c(0.5, r$probs)))
      pmin(pmax(p, 0), 1)
    })
    
    # Cache for features
    feature_cache <- reactiveVal(list())
    cache_key <- function(...) paste(..., sep = "||")
    cache_get <- function(k) feature_cache()[[k]]
    cache_set <- function(k, v) {
      x <- feature_cache()
      x[[k]] <- v
      feature_cache(x)
    }
    cache_clear <- function() feature_cache(list())
    observe({
      cache_clear()
    }) |> bindEvent(list(im$model, im$data_cls), ignoreInit = TRUE)
    
    # Add a user-defined calculated feature and persist its stored columns so
    # the rest of the viz tabs can read them without recalculating.
    observe({
      r <- rvr()
      ft <- input$feature
      sc <- if (identical(ft, "change_probability")) change_feature_scale() else (input$feature_scale %||% "counts")
      
      spec <- default_feature_spec()
      nm <- trimws(input$feature_name %||% "")
      ds <- trimws(input$feature_desc %||% "")
      if (!nzchar(nm)) nm <- spec$name
      if (!nzchar(ds)) ds <- spec$desc
      
      reserved <- character(0)
      if (length(r$order)) {
        reserved <- unique(unlist(lapply(r$order, function(fid) {
          f <- r$features[[fid]]
          c(f$label, f$out_cols)
        })))
      }
      reserved <- reserved[nzchar(reserved)]
      
      out_cols <- switch(
        ft,
        confidence_interval = c(paste0(nm, " Lower"), paste0(nm, " Upper")),
        nm
      )
      
      proposed <- unique(c(nm, out_cols))
      block <- intersect(proposed, reserved)
      if (length(block)) {
        showNotification(
          sprintf('A feature/column name "%s" conflicts with an existing name.', block[1]),
          type = "warning",
          duration = 6
        )
        return(NULL)
      }
      
      withProgress(
        message = sprintf("Adding feature: %s", nm),
        detail = "Registering feature metadata...",
        value = 0.1,
        {
          calculated_feature_ids <- character(0)
          
          params <- switch(
            ft,
            mean = list(),
            quantile = list(q = input$q_val %||% 0.50),
            confidence_interval = list(ci = input$ci_val %||% 0.90),
            exceedance_probability = list(threshold = input$exceed_val %||% 0),
            change_probability = list(
              threshold = input$change_threshold %||% 0,
              dt = lookback_dt_days(),
              dt_input = as.integer(input$lookback_value %||% 1L),
              dt_units = if (identical(time_resolution(), "weekly")) "weeks" else "days",
              scale_mode = change_scale_mode(),
              use_normal_approx = isTRUE(input$change_use_normal_approx)
            ),
            list()
          )
          
          fid <- paste0(
            "usr__", ft, "__", sc, "__", slugify(nm),
            if (ft == "quantile") paste0("__q", fmt_qname(params$q %||% 0.5)) else "",
            if (ft == "confidence_interval") paste0("__ci", fmt_qname(params$ci %||% 0.9)) else "",
            if (ft == "exceedance_probability") paste0("__thr", slugify(as.character(params$threshold %||% 0))) else "",
            if (ft == "change_probability") paste0(
              "__mode", slugify(params$scale_mode %||% "absolute_count"),
              "__dt", slugify(as.character(params$dt_input %||% 1)),
              "__", slugify(params$dt_units %||% "days"),
              "__thr", slugify(as.character(params$threshold %||% 0)),
              "__normal", as.integer(isTRUE(params$use_normal_approx))
            ) else ""
          )
          
          r$features[[fid]] <- list(
            id = fid,
            label = nm,
            description = ds,
            feature_type = ft,
            feature_scale = sc,
            out_cols = out_cols,
            params = params,
            feature_kind = if (ft == "confidence_interval") "composite" else "atomic"
          )
          if (!fid %in% r$order) r$order <- c(r$order, fid)
          calculated_feature_ids <- fid
          
          if (ft == "confidence_interval") {
            # A user-created CI is stored both as a composite interval feature and
            # as its endpoint quantile features so each can be filtered/displayed.
            ci <- params$ci %||% 0.90
            a <- (1 - ci) / 2
            q_specs <- list(
              list(q = a, role = "lower"),
              list(q = 1 - a, role = "upper")
            )
            q_ids <- vapply(q_specs, function(x) quantile_feature_id(sc, x$q), character(1))
            group_id <- paste0("usr__ci_group__", sc, "__ci", fmt_qname(ci), "__", slugify(nm))
            
            for (i in seq_along(q_specs)) {
              q <- q_specs[[i]]$q
              q_role <- q_specs[[i]]$role
              qid <- q_ids[[i]]
              
              if (is.null(r$features[[qid]])) {
                q_label <- quantile_feature_label(sc, q)
                r$features[[qid]] <- list(
                  id = qid,
                  label = q_label,
                  description = sprintf(
                    "Posterior quantile at q=%s on the %s scale. %s endpoint of the %s credible interval.",
                    fmt_num(q),
                    sc,
                    tools::toTitleCase(q_role),
                    fmt_num(ci)
                  ),
                  feature_type = "quantile",
                  feature_scale = sc,
                  out_cols = q_label,
                  params = list(q = q),
                  feature_kind = "atomic",
                  group_id = group_id,
                  group_role = q_role
                )
                if (!qid %in% r$order) r$order <- c(r$order, qid)
              }
            }
            
            r$features[[fid]]$group_id <- group_id
            r$features[[fid]]$member_ids <- q_ids
            calculated_feature_ids <- unique(c(q_ids, fid))
          }
          
          r$last_id <- fid
          
          qs <- need_probs(ft)
          if (length(qs)) r$probs <- sort(unique(c(r$probs, qs)))
          
          incProgress(0.2, detail = "Calculating feature values...")
          calculate_err <- tryCatch({
            calculate_and_store_feature_ids(calculated_feature_ids)
            NULL
          }, error = function(e) conditionMessage(e))
          
          incProgress(0.6, detail = "Refreshing feature lists...")
          r$force_select_fid <- fid
          r$refresh <- r$refresh + 1L
          
          if (!is.null(calculate_err)) {
            showNotification(
              paste("Feature metadata was added, but the calculated feature values could not be stored:", calculate_err),
              type = "warning",
              duration = 8
            )
          } else {
            showNotification(sprintf("Added feature: %s", nm), type = "message")
          }
        }
      )
    }) |> bindEvent(input$add_feature, ignoreInit = TRUE)
    
    feature_choice_map <- reactive({
      r <- rvr()
      fids <- r$order %||% character(0)
      fids <- fids[is_user_feature(fids)]
      if (!length(fids)) return(setNames(character(0), character(0)))
      labs <- vapply(fids, function(fid) r$features[[fid]]$label %||% fid, character(1))
      stats::setNames(fids, labs)
    })
    
    observe({
      ch <- feature_choice_map()
      if (!length(ch)) {
        showNotification("No user-added features to delete yet.", type = "warning")
        return()
      }
      
      showModal(modalDialog(
        title = "Delete a feature",
        selectInput(
          inputId = ns("delete_feature_pick"),
          label = "Select feature to delete",
          choices = ch,
          selected = unname(ch[[length(ch)]])
        ),
        footer = tagList(
          modalButton("Cancel"),
          actionButton(ns("confirm_delete_prompt"), "Delete…", class = "btn btn-danger")
        ),
        easyClose = TRUE
      ))
    }) |> bindEvent(input$delete_feature, ignoreInit = TRUE)
    
    observe({
      r <- rvr()
      fid <- input$delete_feature_pick
      if (is.null(fid) || !nzchar(fid)) return()
      
      if (is.null(r$features[[fid]])) {
        showNotification("That feature no longer exists.", type = "warning")
        removeModal()
        return()
      }
      
      removeModal()
      
      lbl <- r$features[[fid]]$label %||% fid
      
      showModal(modalDialog(
        title = "Confirm deletion",
        tags$div(
          class = "alert alert-warning",
          tags$strong("This action cannot be undone."),
          tags$div(sprintf('You are about to delete: "%s".', lbl))
        ),
        footer = tagList(
          modalButton("Cancel"),
          actionButton(ns("confirm_delete_ok"), "Yes, delete", class = "btn btn-danger")
        ),
        easyClose = FALSE
      ))
    }) |> bindEvent(input$confirm_delete_prompt, ignoreInit = TRUE)
    
    observe({
      r <- rvr()
      fid <- input$delete_feature_pick
      removeModal()
      
      if (is.null(fid) || !nzchar(fid) || is.null(r$features[[fid]])) {
        showNotification("Feature not found.", type = "error")
        return()
      }
      
      cols_to_maybe_drop <- r$features[[fid]]$out_cols %||% character(0)
      lbl <- r$features[[fid]]$label %||% fid
      r$features[[fid]] <- NULL
      r$order <- setdiff(r$order, fid)
      if (identical(r$last_id, fid)) r$last_id <- tail(r$order, 1L)
      drop_feature_cols_from_stored_data(cols_to_maybe_drop)
      
      cur_sel <- isolate(input$display_cols %||% character(0))
      new_sel <- setdiff(cur_sel, fid)
      
      updateSelectizeInput(
        session,
        "display_cols",
        choices = ordered_choices_named(),
        selected = new_sel,
        server = TRUE
      )
      
      r$refresh <- r$refresh + 1L
      showNotification(sprintf("Deleted feature: %s", lbl), type = "message")
    }) |> bindEvent(input$confirm_delete_ok, ignoreInit = TRUE)
    
    observe({
      r <- rvr()
      ch <- ordered_choices_named()
      vals <- unname(ch)
      
      cur <- input$display_cols %||% character(0)
      cur <- intersect(cur, vals)
      
      if (!length(cur)) cur <- intersect(default_display_feature_ids(), vals)
      
      if (!is.null(r$force_select_fid) && r$force_select_fid %in% vals) {
        cur <- unique(c(cur, r$force_select_fid))
        r$force_select_fid <- NULL
      }
      
      updateSelectizeInput(
        session,
        "display_cols",
        choices = ch,
        selected = cur,
        server = TRUE
      )
    }) |> bindEvent(list(refresh_r(), order_r(), ordered_choices_named()), ignoreInit = FALSE)
    
    # Posterior
    get_mean_dt <- function(use_count_scale = FALSE) {
      req(im$model, im$data_cls)
      
      dcls <- im$data_cls
      reg_col   <- dcls$region_column
      date_col  <- dcls$date_column
      
      dt <- get_posterior_means(
        im$model,
        dcls,
        use_suffix = FALSE,
        use_count_scale = use_count_scale
      )
      data.table::setDT(dt)

      mean_col <- intersect(c("predicted_mean", "mean"), names(dt))[1]
      if (is.na(mean_col) || is.null(mean_col)) return(data.table::data.table())
      keep <- intersect(unique(c(reg_col, date_col, mean_col)), names(dt))
      if (length(keep) == 0) return(data.table::data.table())
      
      dt <- dt[, ..keep]
      if (reg_col %in% names(dt))  dt[, (reg_col) := as.character(get(reg_col))]
      
      out_col <- if (use_count_scale) "mean_count" else "mean_prop"
      data.table::setnames(dt, mean_col, out_col)
      dt[]
    }
    
    qdf_props <- reactive({
      req(im$model, im$data_cls, length(all_probs()) > 0)
      qdf <- get_posterior_quantiles(im$model, im$data_cls, probs = all_probs(), use_count_scale = FALSE)
      qdf <- normalize_qdf_names(qdf)
      data.table::setDT(qdf)
      qdf
    })
    
    qdf_counts <- reactive({
      req(im$model, im$data_cls, length(all_probs()) > 0)
      qdf <- get_posterior_quantiles(im$model, im$data_cls, probs = all_probs(), use_count_scale = TRUE)
      qdf <- normalize_qdf_names(qdf)
      
      dcls <- im$data_cls
      reg_col <- dcls$region_column
      date_col <- dcls$date_column
      data.table::setDT(qdf)
      q_only <- setdiff(names(qdf), c(reg_col, date_col))
      if (length(q_only)) data.table::setnames(qdf, q_only, paste0(q_only, "_count"))
      qdf
    })
    
    # Calculate one feature's stored output columns from the fitted model and
    # merge them into the current shared data table.
    apply_feature <- function(out, f, dcls) {
      ft <- f$feature_type
      sc <- f$feature_scale
      out_cols <- f$out_cols %||% character(0)
      
      if (!length(out_cols)) return(out)
      
      for (cc in out_cols) {
        if (!cc %in% names(out)) out[, (cc) := NA_real_]
      }
      
      if (ft == "mean") {
        use_count <- (sc == "counts")
        key <- cache_key("mean", use_count)
        res <- cache_get(key)
        if (is.null(res)) {
          res <- get_mean_dt(use_count)
          cache_set(key, res)
        }
        mcol <- if (use_count) "mean_count" else "mean_prop"
        out2 <- merge_by_region_date(out, res, dcls)
        out2[, (out_cols[[1]]) := as.numeric(get(mcol))]
        out2[, (mcol) := NULL]
        return(out2[])
      }
      
      if (ft == "quantile") {
        qn <- fmt_qname(f$params$q %||% 0.5)
        if (sc == "counts") {
          qdf <- qdf_counts()
          src <- paste0(qn, "_count")
        } else {
          qdf <- qdf_props()
          src <- qn
        }
        
        qdf_one <- slice_qdf(qdf, dcls, src)
        out2 <- merge_by_region_date(out, qdf_one, dcls)
        out2[, (out_cols[[1]]) := as.numeric(get(src))]
        out2[, (src) := NULL]
        return(out2[])
      }
      
      if (ft == "confidence_interval") {
        ci <- f$params$ci %||% 0.90
        a <- (1 - ci) / 2
        qL <- fmt_qname(a)
        qU <- fmt_qname(1 - a)
        
        if (sc == "counts") {
          qdf <- qdf_counts()
          srcL <- paste0(qL, "_count")
          srcU <- paste0(qU, "_count")
        } else {
          qdf <- qdf_props()
          srcL <- qL
          srcU <- qU
        }
        
        qdf_two <- slice_qdf(qdf, dcls, c(srcL, srcU))
        out2 <- merge_by_region_date(out, qdf_two, dcls)
        out2[, (out_cols[[1]]) := as.numeric(get(srcL))]
        out2[, (out_cols[[2]]) := as.numeric(get(srcU))]
        out2[, c(srcL, srcU) := NULL]
        return(out2[])
      }
      
      if (ft == "exceedance_probability") {
        thr <- as.numeric(f$params$threshold %||% 0)
        use_count <- (sc == "counts")
        key <- cache_key("exceed", thr, use_count)
        res <- cache_get(key)
        if (is.null(res)) {
          res <- get_exceedance_probs(
            inla_model = im$model,
            data_cls = im$data_cls,
            threshold = thr,
            use_suffix = TRUE,
            use_count_scale = use_count
          )
          cache_set(key, res)
        }
        out2 <- merge_by_region_date(out, res, dcls)
        ex_col <- grep("^exceedance_prob", names(out2), value = TRUE)[1]
        out2[, (out_cols[[1]]) := as.numeric(get(ex_col))]
        out2[, (ex_col) := NULL]
        return(out2[])
      }

      if (ft == "change_probability") {
        thr <- as.numeric(f$params$threshold %||% 0)
        dt_days <- as.integer(f$params$dt %||% 1L)
        scale_mode <- as.character(f$params$scale_mode %||% "absolute_count")
        use_normal_approx <- isTRUE(f$params$use_normal_approx)
        key <- cache_key("change", thr, dt_days, scale_mode, use_normal_approx)
        res <- cache_get(key)
        if (is.null(res)) {
          res <- epistemic::get_probability_of_increase(
            inla_model = im$model,
            data_cls = im$data_cls,
            threshold = thr,
            dt = dt_days,
            scale_mode = scale_mode,
            use_suffix = TRUE,
            use_normal_approx = use_normal_approx
          )
          cache_set(key, res)
        }
        out2 <- merge_by_region_date(out, res, dcls)
        chg_col <- grep("^change_prob", names(out2), value = TRUE)[1]
        out2[, (out_cols[[1]]) := as.numeric(get(chg_col))]
        out2[, (chg_col) := NULL]
        return(out2[])
      }
      out
    }
    
    calculate_and_store_feature_ids <- function(feature_ids) {
      # This is the session-level storage path for user-added calculated
      # features; values are computed here and then kept in data_cls/posterior.
      req(im$data_cls)
      base_source <- im$data_cls$data
      req(base_source)
      
      out <- data.table::as.data.table(base_source)
      dcls <- im$data_cls
      r <- rvr()
      feature_ids <- unique(feature_ids %||% character(0))
      
      for (fid in feature_ids) {
        f <- r$features[[fid]]
        if (is.null(f) || !is_calculated_feature(f)) next
        out <- apply_feature(out, f, dcls)
      }
      
      im$data_cls$data <- out[]
      im$posterior <- out[]
      invisible(out[])
    }
    
    drop_feature_cols_from_stored_data <- function(cols) {
      cols <- unique(cols %||% character(0))
      if (!length(cols)) return(invisible(NULL))
      req(im$data_cls)
      
      r <- rvr()
      remaining_cols <- unique(unlist(lapply(r$order %||% character(0), function(fid) {
        f <- r$features[[fid]]
        f$out_cols %||% character(0)
      })))
      cols_to_drop <- setdiff(cols, remaining_cols)
      if (!length(cols_to_drop)) return(invisible(NULL))
      
      out <- data.table::as.data.table(im$data_cls$data)
      keep <- setdiff(names(out), cols_to_drop)
      out <- out[, ..keep]
      im$data_cls$data <- out[]
      im$posterior <- out[]
      invisible(out[])
    }
    
    # The Add Feature table now only reads stored columns; calculation happens
    # at model fit or when a user explicitly adds a calculated feature.
    posterior_tbl <- reactive({
      req(im$data_cls)
      r <- rvr()
      
      base_source <- if (!is.null(im$posterior)) im$posterior else im$data_cls$data
      req(base_source)
      
      out <- data.table::as.data.table(base_source)
      
      sel <- input$display_cols %||% character(0)
      if (!length(sel)) sel <- default_display_feature_ids()
      sel <- intersect(sel, r$order %||% character(0))
      
      stored_keep <- unique(unlist(lapply(sel, function(fid) {
        f <- r$features[[fid]]
        if (is.null(f)) return(character(0))
        f$out_cols %||% character(0)
      })))
      
      base_front <- intersect(front_cols(), names(out))
      cols_present <- unique(c(base_front, intersect(stored_keep, names(out))))
      out <- out[, ..cols_present]
      
      out[, c(base_front, setdiff(names(out), base_front)), with = FALSE][]
    })
    
    table_id <- session$ns("posterior_data")
    
    output$posterior_data <- reactable::renderReactable({
      req(input$dt_digits)
      df <- posterior_tbl()
      req(df)
      
      df <- tryCatch(as.data.frame(df), error = function(e) NULL)
      validate(
        need(!is.null(df), "No table available"),
        need(ncol(df) > 0, "No columns available")
      )
      
      if (is.null(names(df))) {
        names(df) <- paste0("V", seq_len(ncol(df)))
      }
      
      front <- intersect(front_cols(), names(df))
      df <- df[, c(front, setdiff(names(df), front)), drop = FALSE]
      
      display_names <- tryCatch(
        map_table_names_to_display(names(df), quantile_suffix = NULL, keep_names = TRUE),
        error = function(e) {
          stats::setNames(names(df), names(df))
        }
      )
      if (is.null(names(display_names))) names(display_names) <- names(df)
      
      # Get the columns to round
      digits <- max(0, min(10, as.integer(input$dt_digits %||% 2)))
      date_cols <- names(df)[sapply(df, inherits, "Date")]
      
      feature_cols <- setdiff(names(df), front)
      cols_to_round <- feature_cols[vapply(df[, feature_cols, drop = FALSE], is.numeric, logical(1))]
      
      # column defs
      col_defs <- lapply(names(df), function(col) {
        label <- display_names[[col]] %||% col
        # get column types 
        is_num <- is.numeric(df[[col]])
        is_date <- col %in% date_cols
        is_rounded <- col %in% cols_to_round
        # define column filters
        
        if (is_num) {
          reactable::colDef(
            name = label,
            align = "right",
            filterable = TRUE,
            filterMethod = if (exists("numeric_range_filter_method", envir = globalenv())) numeric_range_filter_method else NULL,
            filterInput = if (exists("numeric_range_filter_input", envir = globalenv()))
              function(values, name) numeric_range_filter_input(values, name, table_id) else NULL,
            format = if (is_rounded) reactable::colFormat(digits = digits) else NULL
          )
        } else if (is_date) {
          reactable::colDef(
            name = label,
            filterable = TRUE,
            filterMethod = if (exists("date_filter_method", envir = globalenv())) date_filter_method else NULL,
            filterInput = if (exists("date_filter_input", envir = globalenv()))
              function(values, name) date_filter_input(values, name, table_id) else NULL
          )
        } else {
          reactable::colDef(
            name = label,
            filterable = TRUE,
            filterMethod = if (exists("checkbox_filter_method", envir = globalenv())) checkbox_filter_method else NULL,
            filterInput = if (exists("checkbox_filter_input", envir = globalenv()))
              function(values, name) checkbox_filter_input(values, name, table_id) else NULL
          )
        }
      })
      names(col_defs) <- names(df)
      
      page_size <- min(nrow(df), 10L)
      if (page_size < 1) page_size <- 1L
      
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
          style = list(whiteSpace = "nowrap")
        ),
        fullWidth = TRUE,
        theme = BS_REACTABLE_THEME
      )
    })
    
    outputOptions(output, "posterior_data", suspendWhenHidden = TRUE)
    
    output$download_posterior_csv <- downloadHandler(
      filename = function() paste0("posterior_table_", Sys.Date(), ".csv"),
      content = function(file) {
        df <- posterior_tbl()
        req(df)
        df <- as.data.frame(df)
        front <- intersect(front_cols(), names(df))
        df <- df[, c(front, setdiff(names(df), front)), drop = FALSE]
        data.table::fwrite(df, file)
      }
    )
    
    return(list(
      feature_choices = reactive(store()$choices()),
      features_df = reactive(store()$features_df()),
      refresh = reactive(rvr()$refresh)
    ))
  })
}

# Format quantile columns
fmt_qname <- function(q, digits = 3) {
  q <- suppressWarnings(as.numeric(q))
  out <- prettyNum(round(q, digits), digits = 12, drop0trailing = TRUE)
  sub("^\\.", "0.", out)
}

normalize_qdf_names <- function(qdf) {
  data.table::setDT(qdf)
  old <- names(qdf)
  new <- sub("^(props_|counts?_)", "", old)
  num_like <- !is.na(suppressWarnings(as.numeric(new)))
  if (any(num_like)) new[num_like] <- fmt_qname(new[num_like], digits = 12)
  data.table::setnames(qdf, old, new, skip_absent = TRUE)
  qdf
}

slice_qdf <- function(qdf, data_cls, cols_keep) {
  # isolate relevant columns of returned qdf to merge
  data.table::setDT(qdf)
  reg_col  <- data_cls$region_column
  date_col <- data_cls$date_column
  if (reg_col %in% names(qdf)) qdf[, (reg_col) := as.character(get(reg_col))]
  keep <- intersect(c(reg_col, date_col, cols_keep), names(qdf))
  if (length(keep) == 0) return(data.table::data.table())
  qdf[, ..keep]
}

pretty_base_choices <- function(cols) {
  if (length(cols) == 0) return(setNames(character(0), character(0)))
  pretty_map <- map_table_names_to_display(cols, quantile_suffix = NULL, keep_names = TRUE)
  labels <- cols
  if (!is.null(names(pretty_map))) {
    labels <- unname(pretty_map[cols])
    labels[is.na(labels) | labels == ""] <- cols[is.na(labels) | labels == ""]
  }
  stats::setNames(cols, labels)
}

merge_by_region_date <- function(x, y, data_cls) {
  data.table::setDT(x); data.table::setDT(y)
  reg_col <- data_cls$region_column
  date_col <- data_cls$date_column
  
  x[, (reg_col) := as.character(get(reg_col))]
  y[, (reg_col) := as.character(get(reg_col))]
  
  overlap <- setdiff(intersect(names(x), names(y)), c(reg_col, date_col))
  if (length(overlap)) x[, (overlap) := NULL]
  
  data.table::setkeyv(x, c(reg_col, date_col))
  data.table::setkeyv(y, c(reg_col, date_col))
  y[x]
}

# format decimals
fmt_num <- function(x, digits = 6) {
  if (is.null(x) || length(x) == 0) return("")
  x_num <- suppressWarnings(as.numeric(x[[1]]))
  if (is.na(x_num)) return("")
  trimws(formatC(x_num, format = "fg", digits = digits))
}

fmt_percent <- function(x, digits = 6) {
  if (is.null(x) || length(x) == 0) return("")
  x_num <- suppressWarnings(as.numeric(x[[1]]))
  if (is.na(x_num)) return("")
  paste0(fmt_num(100 * x_num, digits = digits), "%")
}
