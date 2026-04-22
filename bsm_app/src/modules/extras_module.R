# © 2025 The Johns Hopkins University Applied Physics Laboratory LLC
# Development of this software was sponsored by the U.S. Government under
# contract no. 75D30124C19958

extras_ui <- function(id) {
  ns <- NS(id)
  
  nav_item(
    # Create a popover, with gear icon
    bslib::popover(
      tags$button(
        type = "button",
        class = "btn btn-link nav-link p-0",
        bsicons::bs_icon("gear", title = "Extras")
      ),
      
      div(
        class = "d-grid gap-2",
        
        # First item in the pop over is a button that when clicked will
        # lead to opening the documentation modal
        actionButton(
          ns("open_docs"),
          label = tagList(
            bsicons::bs_icon("book", title = "Documentation"),
            " Documentation"
          ),
          class = "btn-primary btn-sm"
        ),
        
        actionButton(
          ns("open_tutorial"),
          label = tagList(
            bsicons::bs_icon("box-arrow-up-right", title = "Tutorial"),
            " Tutorial"
          ),
          class = "btn-primary btn-sm"
        ),
        
        # Next, we add the tooltip ui, which is just a button that
        # toggles tool tips on and off
        tooltip_ui(ns("tooltip")),
        
        # Add the light/dark toggle that MP designed
        div(
          class = "btn btn-primary btn-sm settings-toggle-row",
          role = "button",
          div(
            class = "mode-pill-wrap",
            span(class = "mode-side left",
                 span(class = "mode-text", "Light"),
                 span(class = "mode-icon", bsicons::bs_icon("sun-fill"))
            ),
            tags$button(
              type = "button",
              class = "mode-switch",
              onclick = "
                const html = document.documentElement;
                const cur = html.getAttribute('data-bs-theme') || 'dark';
                const next = (cur === 'dark') ? 'light' : 'dark';
                html.setAttribute('data-bs-theme', next);
                return false;
              "
            ),
            span(class = "mode-side right",
                 span(class = "mode-icon", bsicons::bs_icon("moon-stars-fill")),
                 span(class = "mode-text", "Dark")
            )
          )
        )
      ),
      
      # add an id for this popover (we need it to be able to programmatically close)
      id = ns("settings_pop"),
      placement = "bottom",
      options = list(close_button = TRUE)
    )
  )
}

extras_server <- function(id) {
  moduleServer(id, function(input, output, session) {
    
    # Call the tooltip server
    tooltip_server("tooltip")
    
    # Register the documentation module server and keep its UI ready for the modal.
    documentation_server("documentation")
    docs_body <- uiOutput(session$ns("documentation-app_documentation"))
    
    # Open the modal when the documentation book button is clicked
    observe({
      shiny::showModal(
        shiny::modalDialog(
          title = "",
          div(
            style = "max-height: 70vh; overflow-y: auto; padding-right: 0.5rem;",
            docs_body
          ),
          size = "l",
          easyClose = TRUE,
          footer = shiny::modalButton("Close")
        )
      )
      # turn the popover off/close it when documentation is requested
      bslib::toggle_popover("settings_pop", show = FALSE)
    }) |>
      bindEvent(input$open_docs, ignoreInit = TRUE)
    
    observe({
      shinyjs::runjs("window.open('?view=tutorial', '_blank', 'noopener');")
      bslib::toggle_popover("settings_pop", show = FALSE)
    }) |>
      bindEvent(input$open_tutorial, ignoreInit = TRUE)
  })
}
