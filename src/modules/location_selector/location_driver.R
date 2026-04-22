# © 2025 The Johns Hopkins University Applied Physics Laboratory LLC
# Development of this software was sponsored by the U.S. Government under
# contract no. 75D30124C19958

label_list_ld <- list(
  states = list(
    l = "State(s)",
    m = "Select a list of states to include in query. All subdivisions (based on geographic resolution) for the selected states will be added to the query."
  )
)
  
state_selector_ui <- function(id) {
  ns <- NS(id)
  tagList(
    tags$style(".modal-dialog {max-width: 1400px; width: 95%;}"),
    selectizeInput(
      ns("states"),
      choices=c("ALL States", sort(c("DC",state.abb))),
      multiple=T, # allow multiple,
      selected = DEFAULT_STATES,
      options  = list(
        placeholder = "Type to search states...",
        closeAfterSelect = TRUE, 
        plugins = list("remove_button", "clear_button")
      ),
      label=labeltt(label_list_ld[["states"]])
    ),
    conditionalPanel(
      condition = "input.states.length > 0",
      input_task_button(ns("county_selector_button"), "Customize Counties"),
      ns = ns
    )
  )
}

state_selector_server <- function(id, dc, cache_transitions) {
  moduleServer(
    id,
    function(input, output, session) {
      
      ns = session$ns
      
      # initialize some reactives
      grv <- reactiveValues(
        selected_counties = character(0), modal_done = 0
      )
      
      # if cache reactives change, update the states
      observe(updateSelectizeInput(inputId = "states", selected = cache_transitions$states))
      
      
      # Get the us county geometries and reduce to those in 
      # the dc physical adjacency matrix
      us_sf <- reactive({
        create_us_sf() |> dplyr::filter(GEOID %in% colnames(dc$physical_adj))
      })

      
      # When the selected states change, we downselect the us_sf to the 
      # subset that is relevant to the state(s) selected
      # Note: we do not use req() here, because this will cause the counties_sf to
      # not updated when the last state is deleted!
      counties_sf <- reactive({
        if("ALL States" %in% input$states) us_sf()
        else us_sf() |> dplyr::filter(STUSPS %in% input$states)
      }) |> bindEvent(input$states)
      
      # When the states selected change, the selected counties reactive must be
      # updated and the include ak_hi should change
      observe({
        
        # If "ALL" states are selected or more than 3, we shouldn't start with selected
        if(!"ALL States" %in% input$states & length(input$states)<=3) {
          k = tibble(counties_sf()) |>
            mutate(GEOID = as.character(GEOID)) |>
            select(NAME, GEOID)

          grv$selected_counties <- setNames(k$GEOID, k$NAME)
        } else {
          grv$selected_counties <- character(0)
        }
        # override if cache transition is not null
        if(!is.null(cache_transitions$selected_counties)) {
          grv$selected_counties <- grv$selected_counties[
            which(grv$selected_counties %in% cache_transitions$selected_counties)
          ]
        }

        grv$includes_alaska_hawaii <- any(c("AK", "HI", "ALL States") %in% input$states)

      }) |> bindEvent(counties_sf(), input$states, cache_transitions$selected_counties)
      
      # this is a reactive that simply increments each time the "Choose Counties"
      # button is pressed; its like a flag indicating that the modal has been opened
      open_county_selector <- reactiveVal(0)
      
      # Now, we monitor the customize counties button. When it is pressed
      # we increment the trigger, and we show the modal
      observe({
        
        # the button to choose counties has been pressed;
        # increment the trigger flag by 1
        open_county_selector(open_county_selector() + 1)

        # show modal WITH the module UI
        showModal(
          modalDialog(
            title = "County Selector",
            size  = "xl",
            county_selector_ui(ns("county_sel")),
            easyClose = TRUE,
            footer = NULL
          )
        )
      }) |>
        # bind this to the press of the button
        bindEvent(input$county_selector_button)
      
      # Call the county_selector modal server function
      county_selector_server(
        "county_sel",
        geoms  = counties_sf,
        grv = grv,
        open_trigger = open_county_selector,
        adj_mat = dc$physical_adj
      )
      
      
      # close the modal if done is clicked
      observe({ if(grv$modal_done <=0) return() else removeModal() }) |> 
        bindEvent(grv$modal_done)
      
      
      # update the global reactive with selected counties(s) and state(s)
      observe(dc$selected_counties <-grv$selected_counties)
      observe(dc$states <- input$states)
      observe(dc$includes_alaska_hawaii <- grv$includes_alaska_hawaii)


    }
  )
}

