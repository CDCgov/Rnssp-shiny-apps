global_ui_tags <- tags$head(
  tags$script(src = "legend_drag.js"),
  tags$style(HTML("
        .shiny-output-error-validation {
          color: red;
        }
        .card {
          border: 0;
        }
        [id$='_progress'] {
          white-space: normal !important;
          height: auto !important;
          min-height: 2.2em !important;
          overflow: visible !important;
          margin-top: 0.5rem;
        }
              
      .settings-toggle-row {
        width: 100%;
        display: flex;
        align-items: center;
        justify-content: center;
        cursor: default;              /* it's a container, not the click target */
        padding-top: .25rem;          /* fine-tune to match btn-sm height */
        padding-bottom: .25rem;
      }
      
      /* Layout for your inner toggle UI */
      .settings-toggle-row .mode-pill-wrap {
        width: 100%;
        display: flex;
        align-items: center;
        justify-content: center;      /* or space-between if you prefer */
        gap: .5rem;
      }
      
      /* Optional: prevent the inner switch button from inheriting link/btn spacing */
      .settings-toggle-row .mode-switch {
        margin: 0;
      }
    ")),
  tags$script(HTML("document.documentElement.setAttribute('data-bs-theme','dark');")),
  tags$script(HTML("
    Shiny.addCustomMessageHandler('clear-reactable-filters', function(message) {
      var id = message.id;
      console.log('[clear-reactable-filters] message received for id:', id);

      if (!window.Reactable) {
        console.warn('[clear-reactable-filters] window.Reactable not found');
        return;
      }

      // 1. Clear all column filters + global search
      Reactable.setAllFilters(id, []);
      try {
        Reactable.setSearch(id, undefined);
      } catch (e) {
        console.warn('Reactable.setSearch failed:', e);
      }

      // 2. Reset custom filter UI elements inside this table

      var container = document.getElementById(id);
      if (!container) {
        console.warn('[clear-reactable-filters] table container not found for id:', id);
        return;
      }

      // 2a. Clear text/number/date inputs (min/max, date ranges, etc.)
      var inputs = container.querySelectorAll(
        'input[type=text], input[type=number], input[type=date]'
      );
      inputs.forEach(function(input) {
        input.value = '';
      });

      // 2b. Reset checkboxes:
      //     - uncheck all
      //     - then re-check any with value='__ALL__' (our All-option)
      var checkboxes = container.querySelectorAll('input[type=checkbox]');
      checkboxes.forEach(function(cb) {
        cb.checked = false;
      });

      var allBoxes = container.querySelectorAll('input[type=checkbox][value=\"__ALL__\"]');
      allBoxes.forEach(function(cb) {
        cb.checked = true;
      });
    });
    
    Shiny.addCustomMessageHandler('set-reactable-search', function(message) {
      var id = message.id;
      var value = message.value;

      if (!window.Reactable) {
        console.warn('[set-reactable-search] window.Reactable not found');
        return;
      }

      try {
        Reactable.setSearch(id, value || '');
      } catch (e) {
        console.warn('[set-reactable-search] Reactable.setSearch failed:', e);
      }
    });
  "))
)
