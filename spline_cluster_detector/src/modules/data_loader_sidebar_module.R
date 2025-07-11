# © 2025 The Johns Hopkins University Applied Physics Laboratory LLC
# Development of this software was sponsored by the U.S. Government under
# contract no. 75D30124C19958


## Set up label tool tips lookup
sb_ll <- list(
  res = list(
    l = "Resolution:",
    m = "Choose the resolution of clustering (zip or county)"
  ),
  outcome_annotation = list(
    l = "Optional: Indicated Outcome/Category",
    m = "Provide optional text/description of the outcome/category characterized in local file"
  ),
  state = list(
    l = "State",
    m = "Select a single state (allows zip code or county clustering), or United States (county only)"
  ),
  data_type = list(
    l = HTML("<h6>Data Type</>"),
    m = "Choose between TableBuilder api query vs. Data Details (slower, but can allow\n
      for de-duplication. Note this is only available for single state calls, not national"
  ),
  data_source = list(
    l = HTML("<h6>Data Source</>"),
    m = "Choose between Patient Level or Facility Level; The former will assign encounters
      to the patient (home) location, while the latter assigns the encounter to the
      location where the encounter occured (At this time, Facility is not available for Table Builder queries
      at the Zip Code Level)."
  ),
  local_or_nssp = list(
    l = HTML("<h6>Data Source</>"),
    m = "Load a local file with counts by location and date, or construct an NSSP call using the API"
  ),
  local_file_upload = list(
    l = "Upload File",
    m = "Load a csv or cas file; csv files must include column names 'location', 'date', 'count';
      cas file must NOT include a header, have three columns, and the columns are assumed to be location, count, date (in that order)"
  ),
  ad_hoc_vs_built = list(
    l = "URL Option:",
    m = "If you have your own custom built URL, choose 'Ad-hoc URL', else choose 'URL Builder' to
      interacively build an API Call"
  ),
  url_date_change = list(
    l = "Update Custom URL Dates",
    m = "Choose date range to change start and end date in url. Note, the url will
      not update if you choose a start date after an end date"
  ),
  data_load_drange = list(
    l = "Data Date Range",
    m = "Choose a date range for api call. Default is 120 days. We recommend a date
      range sufficient to allow a 7 date test interval preceded by 90 days of baseline"
    ),
    end_date = list(
      l = "End Date",
      m = "Final/Last date of the test period"
    ),
    test_length = list(
      l = "Max Cluster Days",
      m = "Length in days of the test interval; equivalently, the maximum number of days for the cluster"
    ),
    baseline_length = list(
      l = "Baseline Length",
      m = paste0("Number of days in the baseline interval (min 1, max ", MAX_DATE_RANGE, ")")
    ),
    dedup = list(
      l = "De-duplicate?",
      m = "De-duplicates encounters by taking the first (chronological) encounter within VisitID and Hospital.
      The default behavior is to NOT de-duplicate."
    )
)


dl_sidebar_ui <- function(id) {
  ns <- NS(id)

  outcome_annotation <- textInput(
    ns("outcome_annotation"),
    label = labeltt(sb_ll[["outcome_annotation"]])
  )

  pt_acc_panel <- accordion_panel(
    value = "pt_acc_panel",
    title = "Location and Time",
    radioButtons(
      inputId = ns("res"),
      label = labeltt(sb_ll[["res"]]),
      choices = c("Zip code" = "zip", "County" = "county"),
      selected = character(0),
      inline = TRUE
    ),
    selectInput(
      inputId = ns("state"),
      label = labeltt(sb_ll[["state"]]),
      choices = c(sort(c(state.name, "District of Columbia")), "United States"),
      selected = "Alabama"
    ),
    dateRangeInput(
      inputId = ns("data_load_drange"),
      label = labeltt(sb_ll[["data_load_drange"]]),
      end = Sys.Date() - 7,
      start = Sys.Date() - 126
      # min = Sys.Date()-MAX_DATE_RANGE,
      # max = Sys.Date()
    )
  )

  data_type_button <- tagList(
    radioButtons(
      inputId = ns("data_type"),
      label = labeltt(sb_ll[["data_type"]]),
      choices = c("Table Builder" = "table", "Data Details" = "details"),
      selected = "table",
      inline = TRUE
    ),
    conditionalPanel(
      condition = "input.data_type == 'details'",
      input_switch(id = ns("dedup"), label = labeltt(sb_ll[["dedup"]]), value=FALSE),
      ns=ns
    )
  )

  data_source_button <- radioButtons(
    inputId = ns("data_source"),
    label = labeltt(sb_ll[["data_source"]]),
    choices = c("Patient" = "patient", "Facility" = "facility"),
    selected = "patient",
    inline = TRUE
  )
  # us_matrix_checkbox <- checkboxInput(
  #   inputId = ns("us_matrix"),
  #   label = labeltt(sb_ll[["us_matrix"]]),
  #   value=F
  # )

  options_card <- card(
    card_body(
      hidden(accordion(
        id = ns("options_accordion"),
        multiple = FALSE, open = FALSE,
        accordion_panel(
          "Advanced Options",
          data_type_button,
          data_source_button
          # us_matrix_checkbox
        )
      )),
    ),
    class = "bg-transparent border-0"
  )

  ad_hoc_vs_built_rb <- radioButtons(
    inputId = ns("ad_hoc_vs_built"),
    label = labeltt(sb_ll[["ad_hoc_vs_built"]]),
    choices = c("Ad-hoc URL" = "ad_hoc", "URL Builder" = "built"),
    selected = "built",
    inline = TRUE
  )

  # TURN OFF THE custom url, if NOT ALLOWED GLOBALLY
  if (!ALLOW_CUSTOM_URL) {
    ad_hoc_vs_built_rb <- hidden(ad_hoc_vs_built_rb)
  }

  local_or_nssp_ui <- tagList(
    radioButtons(
      inputId = ns("local_or_nssp"),
      label = labeltt(sb_ll[["local_or_nssp"]]),
      choices = c("Local File" = "local", "NSSP API Call" = "nssp"),
      selected = "nssp",
      inline = TRUE
    ),
    conditionalPanel(
      condition = "input.local_or_nssp == 'nssp'",
      ad_hoc_vs_built_rb,
      conditionalPanel(
        condition = "input.ad_hoc_vs_built == 'ad_hoc'",
        card(
          textAreaInput(ns("custom_url"), "Custom URL", height = "200px"),
          htmlOutput(ns("url_validity_message")),
          dateRangeInput(
            ns("url_date_change"),
            label = labeltt(sb_ll[["url_date_change"]])
          ),
          full_screen = F,
          class = "bg-transparent border-0"
        ),
        ns = ns
      ),
      ns = ns
    ),
    hidden(fileInput(
      inputId = ns("local_file_upload"),
      label = labeltt(sb_ll[["local_file_upload"]]),
      multiple = FALSE,
      accept =  c(".csv", ".cas")
    )),
    hidden(uiOutput(ns("file_type_error_message"))),
    hidden(outcome_annotation)
  )


  tagList(
    local_or_nssp_ui,
    card(
      card_body(
        hidden(accordion(
          id = ns("main_accordion"),
          h6("Data Specifications"),
          multiple = FALSE,
          pt_acc_panel
        )),
        style = "overflow: visible"
      ),
      class = "bg-transparent border-0",
      style = "overflow: visible"
    ),
    options_card
  )
}

dl_sidebar_server <- function(id, dc, cc, profile, valid_profile) {
  moduleServer(
    id,
    function(input, output, session) {

      local_file_valid <- reactiveVal("valid")
      custom_url_valid <- reactiveVal("TRUE")

      output$url_validity_message <- renderText({
        if (custom_url_valid() != "TRUE") {
          HTML(paste0("<p style='color:red'>", custom_url_valid(), "</p>"))
        }
      })

      observe(if (!valid_profile()) updateRadioButtons(inputId = "local_or_nssp", selected = "local"))

      ns <- session$ns

      # Set Data Config Global Reactive Values
      observe(dc$USE_NSSP <- input$local_or_nssp == "nssp")
      observe(dc$custom_url_valid <- custom_url_valid() == "TRUE")
      observe(dc$data_type <- input$data_type)
      observe(dc$data_source <- input$data_source)
      observe(dc$res <- input$res)
      observe(dc$dedup <- input$dedup)
      observe(dc$state <- input$state)
      observe(dc$state2 <- state2())
      observe(dc$data_load_start <- input$data_load_drange[1])
      observe(dc$data_load_end <- input$data_load_drange[2])
      observe(dc$ad_hoc <- input$ad_hoc_vs_built == "ad_hoc")
      observe(dc$custom_url <- input$custom_url)
      observe(dc$url_params <- url_params())
      observe(dc$source_data <- source_data())
      observe(dc$synd_summary <- synd_summary())
      observe(dc$synd_drop_menu <- synd()$synd_drop_menu)
      observe(dc$synd <- synd()$synd_cat)

      # if ALLOW SHINY CREDENTIALS is FALSE, but the
      # profile is still not valid, then hide the api choice
      observe({
        toggleElement(
          "local_or_nssp",
          condition = ALLOW_SHINY_CREDENTIALS || valid_profile()
        )
      }) |> bindEvent(valid_profile())

      # Observe the choice for local vs API. If the latter
      # add the US to state dropdown
      observe({
        if (ALLOW_SHINY_CREDENTIALS == TRUE) {
          if (dc$USE_NSSP && !valid_profile()) {
            credServer("creds", profile, valid_profile)
          }
        }
        # use_nssp changes, reset the states list
        if (dc$USE_NSSP) {
          reset_states()
          updateRadioButtons(inputId = "res", selected = "county")
        } else {
          updateRadioButtons(inputId = "res", selected = character(0))
        }

      }) |> bindEvent(dc$USE_NSSP)

      observe({
        # dc$source_data <- NULL
        file_ext <- tools::file_ext(input$local_file_upload$datapath)

        if (!file_ext %in% c("csv", "cas")) {
          # Note that this is outside a render function, and will be silent
          showElement(id = "file_type_error_message")
          output$file_type_error_message <- renderUI(
            tags$span("Error: file type must be csv/cas", style = "color:red;")
          )
        } else {
          hideElement(id = "file_type_error_message")
        }

      }) |> bindEvent(input$local_file_upload$datapath)

      observe({
        if (local_file_valid() != "valid") {
          dc$source_data <- NULL
          showElement(id = "file_type_error_message")
          output$file_type_error_message <- renderUI(
            tags$span(local_file_valid(), style = "color:red;")
          )
        } else {
          hideElement(id = "file_type_error_message")
        }
      }) |> bindEvent(local_file_valid())

      # hide/show sidebar elements
      observe({
        hide_show_sidebar_elements(
          use_nssp = dc$USE_NSSP,
          url_builder = input$ad_hoc_vs_built,
          file_uploaded = !is.null(input$local_file_upload$datapath)
        )
        if (dc$USE_NSSP && input$ad_hoc_vs_built == "built") {
          accordion_panel_insert(
            id = "main_accordion",
            panel = create_syndrome_acc_panel(ns, base_vals()$ccdd_cats)
          )
          # reset the full set of choices for states in the dropdown
          reset_states()

        } else {
          accordion_panel_remove("main_accordion", "cat_acc_panel")
        }
      }) |> bindEvent(dc$USE_NSSP, input$ad_hoc_vs_built, input$local_file_upload$datapath, profile())


      reset_states <- function() {
        updateSelectInput(
          session = session,
          inputId = "state",
          choices = c(sort(c(state.name, "District of Columbia")), "United States"),
          selected = "Alabama"
        )
      }


      # Gather the base values.. depending on the use nnsp value
      base_vals <- reactive({
        if (valid_profile()) get_base_vals(TRUE, profile())
      }) |> bindEvent(profile())


      # Update the choices for syndromic categories
      observe({
        req(base_vals())
        req(input$synd_cat)
        # get the set of choices
        sc <- list(ccdd = base_vals()$ccdd_cats,
          synd = base_vals()$syndromes,
          subsynd = base_vals()$subsyndromes)[[input$synd_cat]]

        updateSelectInput(
          session = session,
          inputId = "synd_drop_menu",
          choices = sc
        )
      })

      # Set the abbreviation based on the state name selector
      state2 <- reactive({
        if (input$state == "United States") return("US")
        if (input$state == "District of Columbia") return("DC")
        state.abb[state.name == input$state]
      })


      use_pre_calc_matrix <- reactiveVal(FALSE)
      observe(use_pre_calc_matrix(TRUE)) |>
        bindEvent(input$us_matrix, ignoreInit = TRUE, once = TRUE)

      # pre_calc_matrix <- reactive(splineClusterDetector::us_distance_matrix()) |>
      pre_calc_matrix <- reactive(us_distance_matrix()) |>
        bindEvent(use_pre_calc_matrix())


      ## Observe the url box and check for errors
      observe({
        req(dc$custom_url)

        if (input$ad_hoc_vs_built == "ad_hoc") {
          # guess the resolution from the url
          res_guess <- fifelse(
            grepl("zip", dc$custom_url, ignore.case = T),
            "zip",
            "county"
          )

          # update the resolution based on this guess
          updateRadioButtons(session = session, inputId = "res", selected = res_guess)

          # update the states based on this resolution
          states <- get_states_from_location(
            toupper(unique(extract_locations_from_url(dc$custom_url, res_guess))),
            res_guess
          )

          choices <- state.name[which(state.abb %in% states)]
          if ("DC" %in% states) {
            choices <- sort(c(choices, "District of Columbia"))
          }
          updateSelectInput(
            session = session,
            inputId = "state",
            choices = choices
          )

          # Get the dates in url
          dates <- extract_dates_from_url(dc$custom_url)

          # Check validity of URL

          custom_url_valid(check_url_validity(
            url = dc$custom_url,
            states = states,
            dates = dates
          ))

          # What about updating the date input range.. The problem here is
          # that we might enter a reactivity loop. How do we prevent that
          freezeReactiveValue(input, "url_date_change")

          dates <- lapply(dates, \(d) if (is.na(d)) NULL else d)
          updateDateRangeInput(
            inputId = "url_date_change",
            start = dates[["start"]],
            end = dates[["end"]]
          )
        }

      }) |> bindEvent(dc$custom_url, input$ad_hoc_vs_built, dc$USE_NSSP)

      # observe the url date change
      observe({

        req(dc$custom_url)

        tryCatch(
          {
            # new_url = Rnssp::change_dates(
            new_url <- inject_dates_into_url(
              dc$custom_url,
              start_date = input$url_date_change[1],
              end_date = input$url_date_change[2]
            )

            updateTextAreaInput(
              inputId = "custom_url", value = new_url
            )
          },
          # TODO: this captures the error, but does not alert the user
          # because there is no where to place the error message!!
          error = function(e) stop(paste0(str(e), "Error!!"))
        )

      }) |> bindEvent(input$url_date_change)


      # url_comb is a reactive that holds the url
      url_params <- reactive({

        if (dc$data_type == "details" & dc$data_source == "facility") {
          fields <- "HospitalZip"
        } else {
          fields <- NULL
        }
        list(
          state = state2(),
          synd_cat = input$synd_cat,
          synd_drop_menu = input$synd_drop_menu,
          start_date = input$data_load_drange[1],
          end_date = input$data_load_drange[2],
          res = dc$res,
          data_type = dc$data_type,
          data_source = dc$data_source,
          fields = fields
        )
      })

      # source_data
      source_data <- reactive({
        # if USE_NSSP, then we simply construct the url (if built), or the user
        # supplied url, but don't yet call api;
        # the call to api  will be triggered only by the ingest data button
        if (dc$USE_NSSP == TRUE && valid_profile() == TRUE) {
          if (input$ad_hoc_vs_built == "built") return(url_params())
          else (return(dc$custom_url))
        }
        # # otherwise, we read a local file (if the path has been provided)
        if (dc$USE_NSSP == FALSE & !is.null(input$local_file_upload$datapath)) {

          file_ext <- tools::file_ext(input$local_file_upload$datapath)
          req(file_ext %in% c("csv", "cas"))

          # Okay, we have a valid file type. We now need to dispatch a load
          # and validate function
          d <- load_and_validate_local_file(
            input$local_file_upload$datapath,
            ext = file_ext
          )

          local_file_valid(d[["message"]])
          req(d[["valid"]])
          return(d[["data"]])
        }
      })

      # Observe for changes in source_data, and if local, limit states
      # and set the date in the time dropdown
      observe({
        req(source_data())
        if (dc$USE_NSSP == F) {
          # get the states
          states <- get_states_from_location(
            locations = source_data()[["location"]],
            res = dc$res
          )
          choices <- state.name[which(state.abb %in% states)]
          if ("DC" %in% states) {
            choices <- sort(c(choices, "District of Columbia"))
          }
          updateSelectInput(
            session = session,
            inputId = "state",
            choices = c(choices, "United States")
          )
          updateDateRangeInput(
            session = session,
            inputId = "data_load_drange",
            start = source_data()[["date"]] |> min(),
            end = source_data()[["date"]] |> max()
          )
        }
      }) |> bindEvent(source_data(), dc$res)


      synd <- reactive({
        list(
          synd_cat = ifelse(is.null(input$synd_cat) | dc$USE_NSSP == F, "none", input$synd_cat),
          synd_drop_menu = ifelse(is.null(input$synd_drop_menu) | dc$USE_NSSP == F, "none", input$synd_drop_menu)
        )
      })

      synd_summary <- reactive({
        k <- paste0(dc$state, " - ", dc$res, " - ", cc$end_date)
        if (dc$USE_NSSP) {
          k <- paste0(
            k, " - ",
            switch (
              synd()$synd_cat,
              "ccdd" = "CCDD Category",
              "synd" = "Syndrome",
              "subsynd" = "Sub-Syndrome",
              "none" = "undefined",
            ), " - ",
            synd()$synd_drop_menu
          )
        } else {
          k <- paste0(k, " - ", input$outcome_annotation)
        }
        return(tools::toTitleCase(k))
      })

    }
  )
}

hide_show_sidebar_elements <- function(use_nssp, url_builder, file_uploaded) {

  accordion_panel_close("options_accordion", TRUE)

  if (use_nssp == TRUE) {

    hideElement("outcome_annotation")
    hideElement("local_file_upload")
    hideElement("file_type_error_message")
    showElement("main_accordion")
    showElement("options_accordion")

    if (url_builder == "ad_hoc") {
      hideElement("data_type")
      hideElement("data_source")
      hideElement("data_load_drange")
    } else {
      showElement("data_type")
      showElement("data_source")
      showElement("data_load_drange")
    }
  } else {
    showElement("local_file_upload")
    hideElement("data_type")
    hideElement("data_source")

    if (file_uploaded) {
      showElement("main_accordion")
      showElement("options_accordion")
      showElement("outcome_annotation")
    } else {
      hideElement("main_accordion")
      hideElement("options_accordion")
      hideElement("outcome_annotation")
    }
  }
}

# Function to create the syndrome panel
create_syndrome_acc_panel <- function(ns, cats) {

  accordion_panel(
    value = "cat_acc_panel",
    title = "Syndromic Category",
    radioButtons(
      inputId = ns("synd_cat"),
      label = "Syndromic Category:",
      choices = c(
        "CCDD" = "ccdd",
        "Syndrome" = "synd",
        "Sub-Syndrome" = "subsynd"
      )
    ),
    selectInput(
      inputId = ns("synd_drop_menu"),
      label = "Select Type",
      choices = cats
    )
  )
}


# Helper function to check validity of the custom url
check_url_validity <- function(url, states, dates) {
  # Some validity checks:
  if (length(states) == 0) {
    validity_result <- "No Counties or Zip Codes Detected"
  } else if (grepl("tableBuilder", url) == FALSE) {
    validity_result <- "Must be tableBuilder URL"
  } else if (grepl("[/]csv[?]", url)) {
    validity_result <- "Cannot be a csv url / must be json"
  } else if (is.null(dates) || length(dates) != 2 || is.na(dates[["start"]]) || is.na(dates[["end"]])) {
    validity_result <- "Start and/or End Date missing, null, or malformed"
  } else if (dates[["start"]] > dates[["end"]]) {
    validity_result <- "End date is before start date"
  } else if ((dates[["end"]] - dates[["start"]]) > MAX_DATE_RANGE) {
    validity_result <- paste0("Start and End date interval cannot exceed ", MAX_DATE_RANGE, " days")
  } else if (grepl("timeResolution=daily", url) == FALSE) {
    validity_result <- "URL must contain 'timeResolution=daily' as parameter"
  } else {
    validity_result <- "TRUE"
  }
  # return the message
  validity_result
}

load_and_validate_local_file <- function(path, ext) {
  if (ext == "cas") load_and_validate_local_cas(path)
  else if (ext == "csv") load_and_validate_local_csv(path)
  else list(data = NULL, valid = FALSE)
}

load_and_validate_local_cas <- function(path) {


  on_error <- function(m = "Invalid format, reason unspecified") {
    list(data = NULL, valid = FALSE, message = m)
  }

  # this is a cas, we require that there is no header
  d <- data.table::fread(path, header = FALSE, colClasses = "character")

  # make name lower case
  setnames(d, new = tolower(names(d)))

  # check only three columns for now
  if (dim(d)[2] != 3) return(on_error(m = "cas file can only contain 3 columns"))

  if (!all(names(d) %in% c("v1", "v2", "v3"))) return(on_error(m = "CAS file has invalid column names"))

  # set location to character
  d$v1 <- as.character(d$v1)

  # set count to numeric
  if (inherits(try(d$v2 <- as.integer(d$v2), silent = TRUE), "try-error")) {
    return(on_error(m = "count column not correctly parsed as integer"))
  }


  # set date to date
  if (inherits(try(d$v3 <- as.IDate(d$v3), silent = TRUE), "try-error")) {
    return(on_error(m = "Date column not correctly parsed"))
  }

  setnames(d, new = c("location", "count", "date"))

  # valid, return.
  return(list(data = d, valid = TRUE, m = "valid"))
}


load_and_validate_local_csv <- function(path) {

  on_error <- function(m = "Invalid format, reason unspecified") {
    list(data = NULL, valid = FALSE, message = m)
  }

  d <- data.table::fread(path, header = TRUE, colClasses = "character")
  # make name lower case
  setnames(d, new = tolower(names(d)))

  if (!"location" %in% names(d)) return(on_error(m = "csv missing named location column"))
  if (!"date" %in% names(d)) return(on_error(m = "csv missing named date column"))
  if (!"count" %in% names(d)) return(on_error(m = "csv missing named count column"))

  # set location to character
  d$location <- as.character(d$location)

  # set location to date
  if (inherits(try(d$date <- as.IDate(d$date), silent = TRUE), "try-error")) {
    return(on_error(m = "Date column not correctly parsed"))
  }

  # set count to numeric
  if (inherits(try(d$count <- as.integer(d$count), silent = TRUE), "try-error")) {
    return(on_error(m = "count column not correctly parsed as integer"))
  }


  # valid, return.
  return(list(data = d, valid = TRUE, message = "valid"))
}

