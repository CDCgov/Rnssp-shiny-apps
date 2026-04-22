# The function below are used to filter columns in reactable tables
# (see posterior_info_module.R for example.) A javascript method and an R input
# function is provided for each method.


# Numeric range
numeric_range_filter_method <- reactable::JS("
    function(rows, columnId, filterValue) {
      if (filterValue == null || filterValue === '') {
        return rows;
      }

      // Expect 'min,max'
      var parts = String(filterValue).split(',');
      var min = parts[0] ? parseFloat(parts[0]) : null;
      var max = parts[1] ? parseFloat(parts[1]) : null;

      if ((min == null || isNaN(min)) && (max == null || isNaN(max))) {
        return rows;
      }

      return rows.filter(function(row) {
        var value = row.values[columnId];
        if (value == null) return false;

        if (min != null && !isNaN(min) && value < min) return false;
        if (max != null && !isNaN(max) && value > max) return false;

        return true;
      });
    }
  ")
numeric_range_filter_input <- function(values, name, element_id) {
  min_val <- floor(min(values, na.rm = TRUE))
  max_val <- ceiling(max(values, na.rm = TRUE))
  
  id_min <- paste0(name, "-min")
  id_max <- paste0(name, "-max")
  
  htmltools::tags$div(
    style = "display:flex; flex-direction:column;",
    # Min box
    tags$input(
      id    = id_min,
      type  = "number",
      placeholder = "Min",
      value = "",
      min   = min_val,
      max   = max_val,
      oninput = sprintf(
        "var min = this.value;
           var max = document.getElementById('%s').value || '';
           Reactable.setFilter('%s', '%s', (min || '') + ',' + (max || ''));",
        id_max, element_id, name
      ),
      onchange = sprintf(
        "var min = this.value;
           var max = document.getElementById('%s').value || '';
           Reactable.setFilter('%s', '%s', (min || '') + ',' + (max || ''));",
        id_max, element_id, name
      ),
      "aria-label" = sprintf("Filter by minimum %s", name)
    ),
    # Max box
    tags$input(
      id    = id_max,
      type  = "number",
      placeholder = "Max",
      value = "",
      min   = min_val,
      max   = max_val,
      oninput = sprintf(
        "var max = this.value;
           var min = document.getElementById('%s').value || '';
           Reactable.setFilter('%s', '%s', (min || '') + ',' + (max || ''));",
        id_min, element_id, name
      ),
      onchange = sprintf(
        "var max = this.value;
           var min = document.getElementById('%s').value || '';
           Reactable.setFilter('%s', '%s', (min || '') + ',' + (max || ''));",
        id_min,  element_id, name
      ),
      "aria-label" = sprintf("Filter by maximum %s", name)
    )
  )
}

# Checkboxes (for text columns)
checkbox_filter_method <- reactable::JS("
  function(rows, columnId, filterValue) {
    if (!filterValue) return rows;

    var parts = String(filterValue).split(',').filter(Boolean);

    // If special ALL token is present -> no filtering
    if (parts.indexOf('__ALL__') !== -1) {
      return rows;
    }

    if (!parts.length) return rows;

    return rows.filter(function(row) {
      var value = String(row.values[columnId]);
      return parts.indexOf(value) !== -1;
    });
  }
")


checkbox_filter_input <- function(values, name, table_id) {
  cats <- sort(unique(values))
  
  # IDs for the 'All' checkbox and container
  all_id <- paste0(name, "-all")
  
  htmltools::tags$div(
    style = "display:flex; flex-direction:column; max-height:80px; overflow:auto;",
    
    # --- 'All' checkbox ---
    tags$label(
      style = "font-weight: normal;",
      tags$input(
        id    = all_id,
        type  = "checkbox",
        value = "__ALL__",   # special token
        checked = "checked", # default: All selected
        onchange = sprintf("
          var container = this.closest('.rt-th, .rt-td, div');
          if (!container) container = this.parentElement.parentElement;

          if (this.checked) {
            // Uncheck all other checkboxes
            var others = container.querySelectorAll('input[type=checkbox]');
            others.forEach(function(cb) {
              if (cb !== document.getElementById('%s')) cb.checked = false;
            });
            // Set filter to ALL
            Reactable.setFilter('%s', '%s', '__ALL__');
          } else {
            // If All is unchecked and no other boxes checked, clear filter
            var checked = Array.from(
              container.querySelectorAll('input[type=checkbox]:not(#%s):checked')
            ).map(function(el) { return el.value; });
            if (!checked.length) {
              Reactable.setFilter('%s', '%s', undefined);
            }
          }
        ", all_id, table_id, name, all_id, table_id, name)
      ),
      " All"
    ),
    
    # --- Individual category checkboxes ---
    lapply(cats, function(cat) {
      id <- paste0(name, "-", make.names(cat))
      tags$label(
        style = "font-weight: normal;",
        tags$input(
          id    = id,
          type  = "checkbox",
          value = cat,
          onchange = sprintf("
            var container = this.closest('.rt-th, .rt-td, div');
            if (!container) container = this.parentElement.parentElement;

            // When any category is changed, uncheck 'All'
            var allCb = document.getElementById('%s');
            if (allCb) allCb.checked = false;

            // Collect checked category values (excluding 'All')
            var checked = Array.from(
              container.querySelectorAll('input[type=checkbox]:not(#%s):checked')
            ).map(function(el) { return el.value; });

            if (!checked.length) {
              // If none selected, treat as no filter
              Reactable.setFilter('%s', '%s', undefined);
            } else {
              Reactable.setFilter('%s', '%s', checked.join(','));
            }
          ", all_id, all_id, table_id, name, table_id, name)
        ),
        paste0(" ", cat)
      )
    })
  )
}


# Date filters
date_filter_method <- reactable::JS("
    function(rows, columnId, filterValue) {
      if (filterValue == null || filterValue === '') {
        return rows;
      }

      var parts = String(filterValue).split(',');
      var minStr = parts[0] || null;
      var maxStr = parts[1] || null;

      var min = minStr ? new Date(minStr) : null;
      var max = maxStr ? new Date(maxStr) : null;

      if (!min && !max) return rows;

      return rows.filter(function(row) {
        var value = row.values[columnId];
        if (!value) return false;

        // reactable sends Date columns as JS Date objects
        var d = (value instanceof Date) ? value : new Date(value);

        if (min && d < min) return false;
        if (max && d > max) return false;

        return true;
      });
    }
  ")
date_filter_input <- function(values, name, element_id) {
  # values should be Date objects in R
  min_date <- min(values, na.rm = TRUE)
  max_date <- max(values, na.rm = TRUE)
  
  min_str <- format(min_date, "%Y-%m-%d")
  max_str <- format(max_date, "%Y-%m-%d")
  
  id_min <- paste0(name, "-min")
  id_max <- paste0(name, "-max")
  
  htmltools::tags$div(
    style = "display:flex; flex-direction:column; gap:4px;",
    # Min date input
    tags$input(
      id    = id_min,
      type  = "text",
      placeholder = "Min",
      min   = min_str,
      max   = max_str,
      value = "",
      oninput = sprintf("
          var min = this.value;
          var max = document.getElementById('%s').value || '';
          Reactable.setFilter('%s', '%s', (min || '') + ',' + (max || ''));
        ", id_max, element_id, name),
      "aria-label" = sprintf("Filter by minimum %s", name)
    ),
    # Max date input
    tags$input(
      id    = id_max,
      type  = "text",
      placeholder = "Max",
      min   = min_str,
      max   = max_str,
      value = "",
      oninput = sprintf("
          var max = this.value;
          var min = document.getElementById('%s').value || '';
          Reactable.setFilter('%s', '%s', (min || '') + ',' + (max || ''));
        ", id_min, element_id, name),
      "aria-label" = sprintf("Filter by maximum %s", name)
    )
  )
}