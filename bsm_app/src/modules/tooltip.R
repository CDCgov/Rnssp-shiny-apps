# © 2025 The Johns Hopkins University Applied Physics Laboratory LLC
# Development of this software was sponsored by the U.S. Government under
# contract no. 75D30124C19958

tooltip_ui <- function(id) {
  ns <- NS(id)
  tagList(
    actionButton(ns("toggle_tooltips"), "Hide Tooltips", class = BUTTON_CLASS)
  )
}

tooltip_server <- function(id) {
  moduleServer(
    id,
    function(input, output, session) {
      
      tooltips_enabled <- reactiveVal(TRUE)
      
      apply_tooltip_state <- function(enabled) {
        shinyjs::runjs(sprintf(
          "window.__tooltipsEnabled = %s;",
          if (enabled) "true" else "false"
        ))
        
        if (enabled) {
          shinyjs::runjs("
            document.querySelectorAll('[data-bs-toggle=\"tooltip\"]').forEach(function(el) {
              var t = bootstrap.Tooltip.getOrCreateInstance(el);
              t.enable();
              el.style.display = '';
            });
          ")
        } else {
          shinyjs::runjs("
            document.querySelectorAll('[data-bs-toggle=\"tooltip\"]').forEach(function(el) {
              var t = bootstrap.Tooltip.getOrCreateInstance(el);
              t.hide();
              t.disable();
              el.style.display = 'none';
            });
            document.querySelectorAll('.tooltip.show').forEach(function(t) {
              t.classList.remove('show');
            });
          ")
        }
      }
      
      # INITIAL APPLY 
      observe({
        apply_tooltip_state(tooltips_enabled())
      })
      
      # Toggle button
      observe({
        tooltips_enabled(!tooltips_enabled())
        apply_tooltip_state(tooltips_enabled())
        
        updateActionButton(
          session,
          "toggle_tooltips",
          label = if (tooltips_enabled()) "Hide Tooltips" else "Show Tooltips"
        )
      }) |> bindEvent(input$toggle_tooltips)
      
      # Install modal hook ONCE
      observe({
        shinyjs::runjs("
          if (!window.__tooltipModalHookInstalled) {
            window.__tooltipModalHookInstalled = true;
            document.addEventListener('shown.bs.modal', function() {
              var enabled = window.__tooltipsEnabled === true;
              document.querySelectorAll('[data-bs-toggle=\"tooltip\"]').forEach(function(el) {
                var t = bootstrap.Tooltip.getOrCreateInstance(el);
                if (enabled) {
                  t.enable();
                  el.style.display = '';
                } else {
                  t.hide();
                  t.disable();
                  el.style.display = 'none';
                }
              });
            });
          }
        ")
      })
      
    }
  )
}
