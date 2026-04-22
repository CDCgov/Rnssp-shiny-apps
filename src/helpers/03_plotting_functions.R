# © 2025 The Johns Hopkins University Applied Physics Laboratory LLC
# Development of this software was sponsored by the U.S. Government under
# contract no. 75D30124C19958

## Plotting Functions

#load("test_data_md.RData")
#library(dplyr)

#' Round a Number Up to a "Nice" Threshold with Slack
#'
#' Rounds a numeric value upward to a human-friendly threshold based on order-of-magnitude rounding.
#' The result is guaranteed to be greater than or equal to \code{x}, but avoids overly coarse bounds
#' by recursively tightening the rounding scale when the gap between the bound and \code{x} exceeds a defined slack.
#'
#' @param x A positive numeric value to round upward.
#' @param slack A numeric value (default \code{0.2}) that controls the looseness of the upper bound. If the proposed bound
#'   is more than \code{(1 + slack)} times larger than \code{x}, a smaller magnitude will be tried.
#' @param magnitude (Optional) A power-of-ten step size to use when rounding. If not provided, the function will automatically
#'   determine an appropriate initial magnitude based on the order of \code{x}.
#'
#' @return A numeric value representing the smallest rounded upper bound that is greater than or equal to \code{x},
#' while keeping the upper bound within a user-defined tolerance (slack) of the original value.
#'
#' @details
#' This function is useful when setting axis or color scale limits in visualizations. It balances between "round" breakpoints
#' (e.g., 10, 50, 100, 200) and tightness of fit, by avoiding unnecessarily large upper bounds. Internally, it reduces the
#' rounding magnitude recursively until the result is within \code{slack} of the original value.
#'
#' @examples
#' round_up_max(101)        # → 110
#' round_up_max(57)         # → 60
#'
#' @export
round_up_max <- function(x, slack=0.2, magnitude= NULL) {
  
  
  if(is.na(x)==TRUE || is.null(x)==TRUE || is.infinite(x)) return(NA)
  
  if(x==0) return(1)
  if (is.null(magnitude)){
    magnitude <- 10^floor(log10(x))
    
  }
  
  upper_ratio = ceiling(x/magnitude)
  bound = upper_ratio*magnitude
  if ((bound/x)>1+slack){
    return (round_up_max(x,slack=slack,magnitude = magnitude/10))
  } else{
    return (bound)
  }
}
#' Extract and Format Data for Mapping from an Epistemic Model
#'
#' Prepares a spatially indexed data frame and associated metadata for mapping a specific summary statistic
#' from an epistemic model. This function is typically used as a backend utility for plotting functions such as \code{make_map()}.
#'
#' @param model A model output object from an \pkg{epistemic} modeling pipeline, containing predictions over time and space.
#' @param data_cls A data class object from the \pkg{epistemic} package, used to interpret model output and apply the correct spatial context.
#' @param params A named list specifying which metric to extract and how to compute it. Must include:
#'   \describe{
#'     \item{\code{metric}}{Character string specifying the summary statistic to extract. One of:
#'       \code{"mean"}, \code{"median"}, \code{"quantile"}, or \code{"exceedance"}.}
#'     \item{\code{use_count} (optional)}{Logical. Indicates whether to extract values on the count scale (e.g., case counts)
#'       or the proportion scale (e.g., incidence or prevalence). Only applies to \code{metric = "mean"}, \code{"median"}, or \code{"quantile"}.}
#'     \item{\code{quantile} (optional)}{Numeric value between 0 and 1, required when \code{metric = "quantile"}.}
#'     \item{\code{threshold} (optional)}{Numeric value specifying the exceedance threshold when \code{metric = "exceedance"}.}
#'   }
#'
#' @return A named list with the following elements:
#'   \describe{
#'     \item{\code{data}}{A \code{data.frame} or \code{sf} object containing the mapped data, including geometry and the display column.}
#'     \item{\code{column}}{A character string giving the name of the column in \code{data} that contains the metric values to map.}
#'     \item{\code{name}}{A human-readable name (title) for the mapped variable, for use in legends or tooltips.}
#'     \item{\code{min}}{Numeric minimum value of the metric, used to define the color scale lower bound.}
#'     \item{\code{max}}{Numeric maximum value of the metric, used to define the color scale upper bound.}
#'   }
#'
#' @details
#' This function acts as the data preparation layer for map rendering. It filters and reshapes model output based on the
#' selected \code{metric}, target date, and aggregation options provided in \code{params}. The result can be passed
#' directly to mapping functions like \code{make_map()} for display.
#'
#' @seealso \code{\link{make_map}}, \code{\link[leaflet]{leaflet}}, \code{\link[tigris]{counties}}
#'
#' @examples
#' \dontrun{
#' map_info <- get_map_data(
#'   model = my_model,
#'   data_cls = my_data_cls,
#'   params = list(metric = "quantile", quantile = 0.9, use_count = FALSE)
#' )
#'
#' head(map_info$data)  # inspect result
#' map_info$column      # name of value column to map
#' map_info$max         # useful for setting color scale limits
#' }
#'
#' @export

get_map_data <-function(model,
                        data_cls,
                        params){
  
  if (!("metric" %in% names(params))){
    cli::cli_abort("Key metric must be provided.")
  }
  
  display_col <- NULL
  display_col_name <- NULL
  minv <- 0
  maxv <- NA_real_
  
  if (params$metric  == "mean"){
    if ("use_count" %in% names(params)){
      use_count <- params$use_count
      if (use_count){
        display_col_name <- "Posterior mean (count)"
      } else {
        display_col_name <- "Posterior mean (proportion)"
      }
    } else {
      use_count <- FALSE
      display_col_name <- "Posterior mean (proportion)"
    }
    dt <- epistemic::get_posterior_means(model,
                                         data_cls,
                                         use_suffix=FALSE,
                                         use_count_scale = use_count)
    display_col <- "predicted_mean"
    
    maxv <- round_up_max(max(dt[[display_col]], na.rm = TRUE))
    
  } else if (params$metric  == "median"){
    if ("use_count" %in% names(params)){
      use_count = params$use_count
      if (use_count){
        display_col_name <- "Posterior median (count)"
      } else {
        display_col_name <- "Posterior median (proportion)"
      }
    } else {
      use_count = FALSE
      display_col_name <- "Posterior median (proportion)"
    }
    dt <- epistemic::get_posterior_medians(model,
                                           data_cls,
                                           use_suffix=FALSE,
                                           use_count_scale = use_count)
    display_col <- "0.5quant"
    
    maxv <- round_up_max(max(dt[[display_col]], na.rm = TRUE))
    
  } else if (params$metric  == "quantile"){
    if ("quantile" %in% names(params)){
      q <- params$quantile 
      if (!is.numeric(q) || q < 0 || q > 1) {
        cli::cli_abort("Quantile must be a numeric value between 0 and 1.")
      }
    } else {
      cli::cli_abort("Required parameter 'quantile' for metric 'posterior_quantile' is missing from params.")
    }
    # if (q<0.5){
    #   ci_width <-1-2*q
    #   display_col <- "lower"
    # } else {
    #   ci_width <-2*q-1
    #   display_col <- "upper"
    # }
    display_col <- paste0("q",q)
    if ("use_count" %in% names(params)){
      use_count = params$use_count
      if (use_count){
        display_col_name <- paste0("Posterior quantile q=",q," (count)")
      } else {
        display_col_name <- paste0("Posterior quantile q=",q," (proportion)")
      }
    } else {
      use_count = FALSE
      display_col_name <- paste0("Posterior quantile q=",q," (proportion)")
    }
    dt <- epistemic::get_posterior_quantiles (model,
                                             data_cls,
                                             use_suffix=FALSE,
                                             probs=q,
                                             use_count = use_count
    )
    data.table::setnames(
      dt,
      new = c(data_cls$region_column, data_cls$date_column, display_col)
    )
    
    maxv<- round_up_max(max(dt[[display_col]], na.rm = TRUE))
    
  } else if (params$metric  == "exceedance"){
    if (!("threshold" %in% names(params))){
      cli::cli_abort("Threshold must be provided for metric exceedance.")
    } else {
      threshold <- params$threshold
    }
    if ("use_count" %in% names(params)) {
      use_count = params$use_count
    } else use_count = FALSE
      
    dt <- epistemic::get_exceedance_probs(model,
                                          data_cls,
                                          use_suffix = FALSE,
                                          threshold = threshold,
                                          use_count = use_count
                                          )
    display_col <- "exceedance_prob"
    display_col_name <- paste0("Exceedance Probability (threshold: ",threshold,")")
    minv <- 0
    maxv <- 1
  } else if (params$metric  == "change"){
    cli::cli_abort("Not Implemented Yet!")
    if (!("threshold" %in% names(params))){
      cli::cli_abort("Threshold must be provided for metric exceedance.")
    } else {
      threshold <- params$threshold
    }
    if ("use_count" %in% names(params)){
      use_count = params$use_count
    } else {
      use_count = FALSE
    }
    if ("use_absolute" %in% names(params)){
      use_absolute = params$use_absolute
    } else {
      use_absolute = FALSE
    }
    if (use_count){
      descriptor = "Counts"
    } else {
      descriptor = "Proportions"
    }
    if (use_absolute){
      display_col_name <- paste0("Absolute Change Probability for ",descriptor," (threshold: ",threshold,")")
    } else {
      display_col_name <- paste0("Relative Change Probability for ",descriptor," (threshold: ",threshold,")")
    }
    
    dt <- epistemic::get_probability_of_increase(model,
                                                 data_cls,
                                                 threshold = threshold)
    display_col <- "change_prob"
    display_col_name <- paste0("Change Probability (threshold: ",threshold,")")
    minv <- 0
    maxv <- 1
  } else {
    cli::cli_abort("Invalid metric.")
  }
  return (list(
    data = dt, 
    column = display_col, 
    name = display_col_name, 
    min = minv, 
    max = maxv,
    date_col = data_cls$date_column,
    region_col = data_cls$region_column
  ))

}


#' Generate a Leaflet Map from Epistemic Model Output
#'
#' Creates an interactive choropleth map using outputs from an epistemic model, filtered by a specified target date.
#' The map is rendered using \pkg{leaflet} and U.S. county shapefiles from \pkg{tigris}, with support for flexible
#' metric types and configuration parameters.
#'
#' @param model A model output object, typically produced by an \pkg{epistemic} modeling pipeline.
#' @param data_cls A data class object from the \pkg{epistemic} package, containing metadata for interpreting model output.
#' @param params A named list of parameters defining what to display on the map. Must include:
#'   \describe{
#'     \item{\code{metric}}{Character string indicating the summary statistic to plot. One of:
#'       \code{"mean"}, \code{"median"}, \code{"quantile"}, or \code{"exceedance"}.}
#'     \item{\code{use_count} (optional)}{Logical. Indicates whether model outputs represent **counts** (e.g., number of cases)
#'       or **proportions** (e.g., prevalence). This is used when \code{metric} is \code{"mean"}, \code{"median"}, or \code{"quantile"}.
#'       For \code{"exceedance"}, outputs are assumed to already be on the proportion scale.}
#'     \item{\code{quantile} (optional)}{Numeric value between 0 and 1, required when \code{metric = "quantile"}, specifying
#'       the quantile level to map.}
#'     \item{\code{threshold} (optional)}{Numeric value used when \code{metric = "exceedance"}; defines the threshold above which
#'       the probability is computed.}
#'   }
#' @param map_year Integer. Year of the geographic shapefile used from \pkg{tigris}. Defaults to \code{2020}.
#' @param target_date Date. The model output will be filtered to this date for display. Defaults to \code{2025-02-02}.
#'
#' @return A \code{leaflet} HTML widget showing an interactive choropleth map of the specified model output metric across U.S. counties.
#'
#' @details
#' This function filters model output to a specific \code{target_date} and computes a selected summary metric using the
#' \code{params} list. The appropriate spatial geometry for U.S. counties is obtained from \pkg{tigris}, controlled by \code{map_year}.
#'
#' When plotting \code{"mean"}, \code{"median"}, or \code{"quantile"}, the \code{use_count} flag controls whether to extract
#' count-based summaries or proportion-based summaries from the model. For \code{"exceedance"}, values are assumed to already
#' be proportions, and \code{use_count} is ignored.
#'
#' @examples
#' \dontrun{
#' # Plot exceedance probabilities
#' make_map(
#'   model = my_model,
#'   data_cls = my_data_cls,
#'   params = list(metric = "exceedance", threshold = 0.9),
#'   target_date = as.Date("2025-02-02")
#' )
#'
#' # Plot median counts
#' make_map(
#'   model = my_model,
#'   data_cls = my_data_cls,
#'   params = list(metric = "median", use_count = TRUE),
#'   target_date = as.Date("2025-02-02")
#' )
#' }
#'
#' @import leaflet
#' @importFrom tigris counties
#' @export

# make_map <- function(
#     map_data, 
#     target_date,
#     map_year = 2020
# ){
#   res <- map_data
#   dt <- res$data 
#   display_col <-res$column 
#   display_col_name <- res$name 
#   minv <- res$min 
#   maxv <- res$max
#   
#   date_col <- res$date_col
#   region_col <- res$region_col
#   
#   
#   # Subset the data using dynamic date column
#   dt_sub <- dt[get(date_col) == target_date]
#   
#   # if dt_sub has no rows, return message, with NULL
#   if(nrow(dt_sub)==0) {
#     cli::cli_alert_warning("No map data possible, check target date?")
#     return(NULL)
#   }
#   
#   # Ensure FIPS is a 5-digit string (pad with zeros)
#   dt_sub[, (region_col) := sprintf("%05s", get(region_col))]
#   dt_sub <- dt_sub[, .(countyfips = get(region_col), value = get(display_col))]
#   
#   # Load U.S. county geometries
#   # options(tigris_use_cache = TRUE)
#   #counties_sf <- tigris::counties(cb = TRUE, year = map_year, class = "sf") |>
#   counties_sf <- Rnssp::county_sf |> 
#     filter(!STATEFP %in% c("60", "66", "69", "72", "78")) |>
#     mutate(countyfips = paste0(STATEFP, COUNTYFP))
# 
#   
#   # Merge spatial and value data
#   map_data <- right_join(counties_sf, dt_sub, by = "countyfips")
#   map_data <- sf::st_transform(map_data, crs = 4326)
#   
#   map_data <- map_data |> 
#     mutate(hover_label = paste0("County: ",NAME,"<br>",
#                                 "FIPS Code: ",GEOID,"<br>",
#                                 display_col_name,": ",round(value, 4)))
#   # get centroids for labels
#   centroids <- sf::st_centroid(map_data)
#   centroids <- sf::st_transform(centroids, 4326)
#   coords <- sf::st_coordinates(centroids)
#   
#   # define color scale
#   pal <- leaflet::colorNumeric("plasma", domain = c(minv,maxv), na.color = "transparent")
#   pal_rev <- leaflet::colorNumeric("plasma", domain = c(maxv,minv), reverse = TRUE, na.color = "transparent")
#   # Plot
#   p <- leaflet::leaflet(map_data) |>
#     leaflet::addProviderTiles("CartoDB.Positron") |>
#     leaflet::addPolygons(
#       fillColor = ~pal(value),
#       weight = 1,
#       opacity = 1,
#       color = "white",
#       dashArray = "3",
#       fillOpacity = 0.7,
#       highlightOptions = highlightOptions(
#         weight = 2,
#         color = "#666",
#         fillOpacity = 0.9,
#         bringToFront = TRUE
#       ),
#       label = lapply(map_data$hover_label, htmltools::HTML)
#     )  |> 
#     addLabelOnlyMarkers(
#       lng = coords[, 1],
#       lat = coords[, 2],
#       label = centroids$NAME,
#       labelOptions = labelOptions(
#         noHide = TRUE,
#         direction = 'center',
#         textOnly = TRUE,
#         style = list(
#           "font-size" = "14px",
#           "color" = "black",
#           "text-shadow" = "1px 1px white"
#         )
#       )
#     ) |> 
#     leaflet::addLegend(
#       pal = pal_rev,
#       values = c(minv,maxv),
#       labFormat = leaflet::labelFormat(transform = function(x) sort(x, decreasing = TRUE)),
#       title = display_col_name,
#       position = "bottomright"
#     ) |> 
#     leaflet.extras::addResetMapButton() |> 
#     leaflet.extras::addFullscreenControl()
#   return (p)
# }
# Build HTML hover labels for county polygons, optionally including a mapped
# value and title.
get_hover_label_county <- function(county, fips, values=NULL, value_title="") {
  
  lapply(seq_along(county), \(cty) {
    lbl <- paste0(
      "County: ",county[cty],"<br>",
      "FIPS Code: ",fips[cty]
    )
    if(!is.null(values)) {
      lbl <- paste0(
        lbl,
        "<br>", value_title,": ",round(values[cty], 4)
      )
    }
    lbl
  })
}

# Return a palette as a vector of colors, supporting viridis and
# RColorBrewer names with a viridis fallback.
get_palette_vector <- function(name, n = 256) {
  if (is.null(name) || identical(name, "")) {
    return(viridisLite::viridis(n))
  }
  
  viridis_funs <- list(
    viridis = viridisLite::viridis,
    magma = viridisLite::magma,
    plasma = viridisLite::plasma,
    inferno = viridisLite::inferno,
    cividis = viridisLite::cividis
  )
  
  if (name %in% names(viridis_funs)) {
    return(viridis_funs[[name]](n))
  }
  
  if (name %in% rownames(RColorBrewer::brewer.pal.info)) {
    maxcols  <- RColorBrewer::brewer.pal.info[name, "maxcolors", drop = TRUE]
    base_pal <- RColorBrewer::brewer.pal(min(maxcols, 9), name)
    return(grDevices::colorRampPalette(base_pal)(n))
  }
  
  viridisLite::viridis(n) # Fallback: viridis
}

# Join location geometry to a selected map metric/date and prepare the hover
# labels and legend bounds needed for leaflet rendering.
polygon_info <- function(locs, map_data, target_date) {
  date_col <- map_data$date_col %||% "date"
  value_col <- map_data$column %||% "value"
  region_col <- map_data$region_col %||% "countyfips"
  
  dt_sub <- map_data$data[get(date_col) == target_date, .(
    join_region = as.character(get(region_col)),
    outcome = get(value_col)
  )]
  
  d <- dplyr::left_join(locs, dt_sub, by = c("GEOID" = "join_region"))
  
  minv <- maxv <- NA_real_
  
  # If d[["outcome]] is completely missing, then 
  # we should return NA
  has_values <- !is.null(d[["outcome"]]) &&
    length(d[["outcome"]]) > 0 &&
    any(is.finite(d[["outcome"]]))
  
  if (has_values) {
    # get min and (rounded) max
    minv <- min(d[["outcome"]], na.rm = TRUE)
    maxv <- round_up_max(max(d[["outcome"]], na.rm = TRUE))
  }
  
  hover_vals <- get_hover_label_county(
    county      = d$NAME,
    fips        = d$GEOID,
    values      = d$outcome,
    value_title = map_data$name
  )
  
  list(
    d          = d,
    pal        = NULL,
    pal_rev    = NULL,
    minv       = minv,
    maxv       = maxv,
    hover_vals = hover_vals,
    value_title = map_data$name
  )
}

# Add polygons and an optional legend to an existing leaflet map using the
# precomputed polygon metadata from polygon_info().
update_polygons <- function(
    p,
    pi,
    domain = NULL,
    palette = "viridis",
    legend_position = "bottomright"
) {
  if (!is.null(domain)) {
    
    pal_vec <- get_palette_vector(palette, n = 256)
    
    pal_fill <- leaflet::colorNumeric(
      palette = pal_vec,
      domain  = domain
    )
    pal_legend <- leaflet::colorNumeric(
      # reverse for orientation bug that comes with using domain instead of data directly
      palette = rev(pal_vec),
      domain  = domain
    )
    
    ticks <- seq(domain[1], domain[2], length.out = 6)
    
    p <- p |>
      ## add the polygons
      leaflet::addPolygons(
        data        = pi$d,
        fillColor   = ~pal_fill(outcome),
        weight      = 1,
        opacity     = 1,
        color       = "white",
        dashArray   = "3",
        fillOpacity = 0.7,
        layerId = ~GEOID,
        highlightOptions = highlightOptions(
          weight      = 2,
          color       = "#666",
          fillOpacity = 0.9,
          bringToFront = TRUE
        ),
        label = lapply(pi$hover_vals, htmltools::HTML)
      ) |>
      leaflet::addLegend(
        pal       = pal_legend,
        values    = ticks,
        labFormat = leaflet::labelFormat(
          transform = function(x) sort(x, decreasing = TRUE)
        ),
        title     = pi$value_title,
        position  = legend_position
      )
    
    return(p)
  }
  
  p <- p |> 
    leaflet::addPolygons(
      data = pi$d,
      fillColor = {if (is.null(pi$pal)) "grey" else pi$pal(pi$d$outcome)},
      weight = 1,
      opacity = 1,
      color = "white",
      dashArray = "3",
      fillOpacity = 0.7,
      highlightOptions = highlightOptions(
        weight = 2,
        color = "#666",
        fillOpacity = 0.9,
        bringToFront = TRUE
      ),
      label = lapply(pi$hover_vals, htmltools::HTML)
    )
  
  if(!is.null(pi$pal)) {
    p <- p |>
      ## add the legend
      leaflet::addLegend(
        pal = pi$pal_rev,
        values = c(pi$minv, pi$maxv),
        labFormat = leaflet::labelFormat(transform = function(x) sort(x, decreasing = TRUE)),
        title = pi$value_title,
        position  = legend_position
      )
      
  }
  return(p)
}

################################################
## TIME SERIES PLOTS
################################################

# Build a single prediction time-series panel from data that already contains
# median/lower/upper columns and optional observed values.
plot_ly_time_series <- function(
    dt, 
    show_legend=TRUE, 
    y_title="Outcome", 
    location_display_name = NULL, 
    ci = "95", 
    axis_id=1, 
    include_observed = TRUE,
    max_y_val = NULL
) {
  
  q = as.numeric(ci)
  keep_cols = intersect(c("date", "type", "observed", "median", "lower", "upper"), names(dt))
  dt <- dt[, .SD, .SDcols=keep_cols]
  
  yref <- paste0("y", if (axis_id == 1) "" else axis_id)
  axis_name <- paste0("yaxis", if (axis_id == 1) "" else axis_id)
  
  dt[, hover_text := paste0(
    "Date: ", format(date, "%Y-%m-%d"), "<br>",
    "Estimated: ", round(median, 4), "<br>",
    sprintf("%2.0f%%", q), " CI: [", round(lower, 4), ", ", round(upper, 4), "]"
  )]
  
  # Historical ribbon
  p <- plot_ly() |> 
    add_ribbons(data = dt[type == "Historical"],
                x = ~date, ymin = ~lower, ymax = ~upper,
                fillcolor = 'rgba(173,216,230,0.6)',  # Light blue
                line = list(color = 'rgba(0,0,0,0)'),
                name = sprintf('%2.0f%% Credible Interval (Historical)', q),
                legendgroup = "observed",
                showlegend = FALSE, 
                hoverinfo = "none")
  
  
  # Forecast ribbon
  p <- p |> 
    add_ribbons(data = dt[type == "Forecast"],
                x = ~date, ymin = ~lower, ymax = ~upper,
                fillcolor = 'rgba(144,238,144,0.6)',  # Light green
                line = list(color = 'rgba(0,0,0,0)'),
                name = sprintf('%2.0f%% Credible Interval (Forecast)', q),
                legendgroup = "future",
                showlegend = FALSE, 
                hoverinfo ="none"
              )
  
  
  # Historical line
  p <- p |> 
    add_trace(data = dt[type == "Historical"],
              x = ~date, y = ~median,
              type = 'scatter', mode = 'lines',
              line = list(color = 'blue'),
              name = 'Modeled Outcome',
              legendgroup = 'observed',
              showlegend = show_legend, 
              text = ~hover_text,
              hoverinfo = "text"
    )
  
  # Forecast line
  p <- p |> 
    add_trace(data = dt[type == "Forecast"],
              x = ~date, y = ~median,
              type = 'scatter', mode = 'lines',
              line = list(color = 'green', dash = 'dash'),
              name = 'Future Time Points',
              legendgroup = "future",
              showlegend = show_legend,
              text = ~hover_text,
              hoverinfo = "text"
    )
  
  # Observed Data
  if(include_observed) {
    p <- p |> 
      add_trace(data = dt[type=="Historical"],
                x=~date, y=~observed,
                type="scatter", mode='markers',
                marker=list(color="black"),
                name='Observed Data',
                showlegend = show_legend
      )
  }
     
  
  # Layout
  if(is.null(location_display_name)) location_display_name = "Time Series"
  
  # per-panel or global max
  if(!is.null(max_y_val)) y_max <- max_y_val
  else y_max <- max(dt$upper * 1.1, na.rm = TRUE)

  p <- p |> 
    layout(
      annotations = list(
        text = location_display_name,
        x=0.02,
        y=1,
        xref="paper",
        yref="paper",
       xanchor = "left",
       yanchor = "bottom",
       showarrow=FALSE, 
       font = list(size = 16, color = "black", family="Arial black")
      ),
      xaxis = list(
        title = list(text = "Date", font = list(size=14)),
        tickfont = list(size=12),
        range = c(min(dt$date), max(dt$date))
      ),
      yaxis = list(
        title = list(text = y_title, font = list(size = 14)),
        tickfont = list(size=12),
        range = c(0, y_max)
      ),
      hovermode = "x unified",
      legend = list(orientation='h')
    )
  
  p
  
}

# Legacy helper that assembles multiple prediction time-series panels using
# the older input structure expected by existing callers.
time_series_subplots <- function(ts_inputs, ts_plot_data, display_col = NULL, fixed_y=FALSE, ci = "95", ...) {
  
  global_max <- NULL
  # if fixed_y is TRUE, we have to calculate the max of the request CI
  if(fixed_y) global_max <- get_max_y_over_plots(ts_inputs, ts_plot_data, ci)
  
  plots = lapply(seq_along(ts_inputs), \(i) {
    
    d <- ts_plot_data[[ts_inputs[i]]]
    # if display_col is not null, then we have a column in each of the 
    # ts_plot_data element that holds the name we should display
    display_name <- ts_inputs[i]
    if(!is.null(display_col)) display_name <- unique(d[[display_col]])
    
    plot_ly_time_series(
      dt = d,
      show_legend = (i==1), 
      location_display_name = display_name, 
      axis_id = i,
      ci = ci, 
      max_y_val = global_max,
      ...
    )
  })
  
  p <- subplot(
    plots,
    nrows = ceiling(length(plots)/3),
    shareX = TRUE,
    titleX = TRUE,
    titleY = TRUE,
    margin = 0.04
  ) %>%
    layout(
      legend = list(orientation = "h", x = 0.5, xanchor = "center", y = -0.1),
      margin = list(b = 80)  # extra bottom space for legend
    )
  
  return(p)
}

# Build named region choices for time-series selectors from the stored data.
get_time_series_region_choices <- function(data_cls, display_col = "region") {
  region_col <- data_cls$region_column
  req(!is.null(region_col), region_col %in% names(data_cls$data))
  
  cols <- unique(c(region_col, display_col))
  cols <- cols[cols %in% names(data_cls$data)]
  req(length(cols) >= 1)
  
  dt <- data.table::as.data.table(data_cls$data)[, ..cols]
  dt <- unique(dt)
  
  if (!(display_col %in% names(dt))) {
    dt[, (display_col) := as.character(get(region_col))]
  }
  
  stats::setNames(dt[[region_col]], dt[[display_col]])
}

# Collect the UI-level settings needed to build prediction time-series plots.
build_time_series_plot_spec <- function(
    region_ids,
    ci,
    fixed_y = FALSE,
    display_col = "region",
    use_count = FALSE,
    future_steps = 0
) {
  list(
    regions = region_ids %||% character(0),
    ci = as.numeric(ci),
    fixed_y = isTRUE(fixed_y),
    display_col = display_col,
    use_count = isTRUE(use_count),
    future_steps = as.integer(future_steps %||% 0)
  )
}

# Combine a list of plotly time-series panels into the app's shared subplot
# layout with a single legend.
assemble_time_series_subplots <- function(plots) {
  subplot(
    plots,
    nrows = ceiling(length(plots)/3),
    shareX = TRUE,
    titleX = TRUE,
    titleY = TRUE,
    margin = 0.04
  ) %>%
    layout(
      legend = list(orientation = "h", x = 0.5, xanchor = "center", y = -0.1),
      margin = list(b = 80)
    )
}

# Render the prediction time-series view from precomputed per-region data.
build_time_series_plotly <- function(ts_plot_data, spec, ...) {
  region_ids <- spec$regions %||% character(0)
  validate(need(length(region_ids) > 0, "Must have a least one region selected"))
  
  global_max <- NULL
  if (isTRUE(spec$fixed_y)) {
    global_max <- get_max_y_over_plots(region_ids, ts_plot_data, spec$ci)
  }
  
  plots <- lapply(seq_along(region_ids), function(i) {
    region_id <- region_ids[[i]]
    d <- ts_plot_data[[region_id]]
    validate(need(!is.null(d), sprintf("No time series data available for region %s", region_id)))
    
    display_name <- region_id
    if (!is.null(spec$display_col) && spec$display_col %in% names(d)) {
      display_name <- unique(d[[spec$display_col]])[1]
    }
    
    plot_ly_time_series(
      dt = d,
      show_legend = (i == 1),
      location_display_name = display_name,
      axis_id = i,
      ci = spec$ci,
      max_y_val = global_max,
      ...
    )
  })
  
  assemble_time_series_subplots(plots)
}

# Normalize a quantile probability into the string form used by posterior
# quantile column lookups.
fmt_qname <- function(q, digits = 3) {
  q <- suppressWarnings(as.numeric(q))
  out <- prettyNum(round(q, digits), digits = 12, drop0trailing = TRUE)
  sub("^\\.", "0.", out)
}

# Standardize posterior quantile table column names so downstream code can
# request quantiles by a consistent key.
normalize_qdf_names <- function(qdf) {
  data.table::setDT(qdf)
  old <- names(qdf)
  new <- sub("^(props_|counts?_)", "", old)
  num_like <- !is.na(suppressWarnings(as.numeric(new)))
  if (any(num_like)) new[num_like] <- fmt_qname(new[num_like], digits = 12)
  data.table::setnames(qdf, old, new, skip_absent = TRUE)
  qdf
}

# Extract the region/date keys plus selected quantile columns from a wide
# posterior quantile table.
slice_qdf <- function(qdf, data_cls, cols_keep) {
  data.table::setDT(qdf)
  reg_col <- data_cls$region_column
  date_col <- data_cls$date_column
  if (reg_col %in% names(qdf)) qdf[, (reg_col) := as.character(get(reg_col))]
  keep <- intersect(c(reg_col, date_col, cols_keep), names(qdf))
  if (length(keep) == 0) return(data.table::data.table())
  qdf[, ..keep]
}

# Merge two tables on the app's canonical region/date keys while dropping any
# overlapping value columns from the left-hand table first.
merge_by_region_date <- function(x, y, data_cls) {
  data.table::setDT(x)
  data.table::setDT(y)
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

# Compute a shared y-axis maximum for the "Other Time Series" plot when the
# user requests fixed axes across all panels.
get_other_time_series_global_max <- function(plot_dt) {
  vals <- c(plot_dt$value, plot_dt$upper)
  vals <- vals[is.finite(vals)]
  if (!length(vals)) return(NULL)
  mx <- max(vals, na.rm = TRUE)
  if (!is.finite(mx) || mx <= 0) return(NULL)
  mx * 1.1
}

# Build a single region/feature panel for the "Other Time Series" tab,
# including ribbons for intervals and lines for scalar series.
plot_ly_other_time_series_panel <- function(
    dt,
    panel_title,
    fixed_y_max = NULL,
    color_map = NULL,
    legend_features = character(0)
) {
  feature_labels <- unique(dt$feature_label)
  if (is.null(color_map)) {
    color_map <- grDevices::hcl.colors(max(length(feature_labels), 1L), "Dark 3")
    names(color_map) <- feature_labels
  }
  
  p <- plotly::plot_ly()
  
  for (feat in feature_labels) {
    feat_dt <- dt[dt$feature_label == feat, , drop = FALSE]
    color <- color_map[[feat]] %||% "#1f77b4"
    fill_color <- grDevices::adjustcolor(color, alpha.f = 0.2)
    show_feat_legend <- feat %in% legend_features
    
    has_interval <- all(c("lower", "upper") %in% names(feat_dt)) &&
      any(is.finite(feat_dt$lower) | is.finite(feat_dt$upper), na.rm = TRUE)
    
    if (isTRUE(has_interval)) {
      p <- p |>
        plotly::add_ribbons(
          data = feat_dt[feat_dt$type == "Historical", , drop = FALSE],
          x = ~date,
          ymin = ~lower,
          ymax = ~upper,
          fillcolor = fill_color,
          line = list(color = "rgba(0,0,0,0)"),
          name = paste0(feat, " CI"),
          legendgroup = feat,
          showlegend = show_feat_legend,
          hoverinfo = "none"
        ) |>
        plotly::add_ribbons(
          data = feat_dt[feat_dt$type == "Forecast", , drop = FALSE],
          x = ~date,
          ymin = ~lower,
          ymax = ~upper,
          fillcolor = fill_color,
          line = list(color = "rgba(0,0,0,0)"),
          name = paste0(feat, " Forecast CI"),
          legendgroup = feat,
          showlegend = FALSE,
          hoverinfo = "none"
        )
    }
    
    if ("value" %in% names(feat_dt) && any(is.finite(feat_dt$value), na.rm = TRUE)) {
      hist_dt <- feat_dt[feat_dt$type == "Historical", , drop = FALSE]
      fc_dt <- feat_dt[feat_dt$type == "Forecast", , drop = FALSE]
      
      if (nrow(hist_dt) > 0) {
        p <- p |>
          plotly::add_trace(
            data = hist_dt,
            x = ~date,
            y = ~value,
            type = "scatter",
            mode = "lines",
            line = list(color = color),
            name = feat,
            legendgroup = feat,
            showlegend = show_feat_legend,
            text = ~hover_text,
            hoverinfo = "text"
          )
      }
      
      if (nrow(fc_dt) > 0) {
        p <- p |>
          plotly::add_trace(
            data = fc_dt,
            x = ~date,
            y = ~value,
            type = "scatter",
            mode = "lines",
            line = list(color = color, dash = "dash"),
            name = paste0(feat, " Forecast"),
            legendgroup = feat,
            showlegend = FALSE,
            text = ~hover_text,
            hoverinfo = "text"
          )
      }
    }
  }
  
  y_max <- fixed_y_max
  if (is.null(y_max)) y_max <- get_other_time_series_global_max(dt)
  
  p |>
    plotly::layout(
      annotations = list(
        text = panel_title,
        x = 0.02,
        y = 1,
        xref = "paper",
        yref = "paper",
        xanchor = "left",
        yanchor = "bottom",
        showarrow = FALSE,
        font = list(size = 16, color = "black", family = "Arial black")
      ),
      xaxis = list(
        title = list(text = "Date", font = list(size = 14)),
        tickfont = list(size = 12),
        range = c(min(dt$date), max(dt$date))
      ),
      yaxis = list(
        title = list(text = "Value", font = list(size = 14)),
        tickfont = list(size = 12),
        range = if (!is.null(y_max)) c(0, y_max) else NULL
      ),
      hovermode = "x unified",
      legend = list(orientation = "h")
    )
}

# Assemble the complete "Other Time Series" plotly figure from stored feature
# values, optionally splitting selected features into separate panels.
build_other_time_series_plotly <- function(plot_dt, region_ids, separate_features = FALSE, fixed_y = FALSE) {
  validate(
    need(!is.null(plot_dt), "No plot data available"),
    need(nrow(plot_dt) > 0, "No plot data available"),
    need(length(region_ids) > 0, "Must have at least one region selected")
  )
  
  fixed_y_max <- if (isTRUE(fixed_y)) get_other_time_series_global_max(plot_dt) else NULL
  plots <- list()
  all_features <- unique(plot_dt$feature_label)
  color_map <- grDevices::hcl.colors(max(length(all_features), 1L), "Dark 3")
  names(color_map) <- all_features
  shown_features <- character(0)
  
  if (isTRUE(separate_features)) {
    combos <- unique(plot_dt[, c("region_id", "region_label", "feature_label"), drop = FALSE])
    for (i in seq_len(nrow(combos))) {
      sub_dt <- plot_dt[
        plot_dt$region_id == combos$region_id[[i]] &
          plot_dt$feature_label == combos$feature_label[[i]],
        ,
        drop = FALSE
      ]
      feat <- combos$feature_label[[i]]
      legend_features <- if (!feat %in% shown_features) feat else character(0)
      shown_features <- unique(c(shown_features, feat))
      plots[[length(plots) + 1L]] <- plot_ly_other_time_series_panel(
        sub_dt,
        panel_title = paste(combos$region_label[[i]], combos$feature_label[[i]], sep = " - "),
        fixed_y_max = fixed_y_max,
        color_map = color_map,
        legend_features = legend_features
      )
    }
  } else {
    for (region_id in region_ids) {
      sub_dt <- plot_dt[plot_dt$region_id == region_id, , drop = FALSE]
      if (!nrow(sub_dt)) next
      panel_title <- unique(sub_dt$region_label)[1]
      panel_features <- unique(sub_dt$feature_label)
      legend_features <- setdiff(panel_features, shown_features)
      shown_features <- unique(c(shown_features, panel_features))
      plots[[length(plots) + 1L]] <- plot_ly_other_time_series_panel(
        sub_dt,
        panel_title = panel_title,
        fixed_y_max = fixed_y_max,
        color_map = color_map,
        legend_features = legend_features
      )
    }
  }
  
  validate(need(length(plots) > 0, "No plot data available for the selected region/feature combination"))
  
  subplot(
    plots,
    nrows = ceiling(length(plots) / 3),
    shareX = TRUE,
    titleX = TRUE,
    titleY = TRUE,
    margin = 0.04
  ) %>%
    layout(
      legend = list(orientation = "h", x = 0.5, xanchor = "center", y = -0.1),
      margin = list(b = 80)
    )
}

# Compute a shared y-axis ceiling across a set of prediction time-series
# panels using their stored upper-interval values.
get_max_y_over_plots <- function(names, plot_data, ci) {
  
  
    global_max <- NULL
  
    vals <- vapply(
      names,
      function(f) {
        d <- plot_data[[f]]
        if (is.null(d) || !("upper" %in% names(d))) {
          return(NA_real_)
        }
        mx <- suppressWarnings(max(d[["upper"]], na.rm = TRUE))
        if (!is.finite(mx)) NA_real_ else mx
      },
      numeric(1)
    )
    
    gmax <- max(vals, na.rm = TRUE)
    if (is.finite(gmax) && gmax > 0) {
      global_max <- gmax * 1.1
    }
    
    global_max
}

# Format a posterior probability into the quantile-column naming convention
# used by the older time-series quantile helpers.
format_time_series_prob_name <- function(q) {
  out <- prettyNum(as.numeric(q), digits = 12, drop0trailing = TRUE)
  sub("^\\.", "0.", out)
}

# Extract the stored median and credible-interval columns needed by the
# prediction time-series tab and reshape them into the plotting structure used
# by build_time_series_plotly().
prepare_time_series_feature_plot_data <- function(feature_data, data_cls, spec, ci_feature, median_feature = NULL) {
  byvar <- data_cls$region_column
  date_col <- data_cls$date_column
  req(!is.null(byvar), !is.null(date_col))
  req(!is.null(ci_feature), identical(ci_feature$feature_type, "confidence_interval"))

  req(!is.null(feature_data))
  d <- data.table::as.data.table(feature_data)

  median_col <- median_feature$out_cols[[1]] %||% NULL
  ci_cols <- ci_feature$out_cols %||% character(0)
  req(!is.null(median_col), length(ci_cols) >= 2)
  req(median_col %in% names(d), all(ci_cols[1:2] %in% names(d)))

  keep_cols <- unique(c(
    byvar,
    date_col,
    "region",
    data_cls$numerator_column,
    data_cls$denominator_column,
    median_col,
    ci_cols[1:2]
  ))
  keep_cols <- intersect(keep_cols, names(d))

  d <- d[, ..keep_cols]
  data.table::setnames(
    d,
    old = c(median_col, ci_cols[1], ci_cols[2]),
    new = c("median", "lower", "upper"),
    skip_absent = TRUE
  )

  if (!inherits(d[[date_col]], "Date")) {
    d[, (date_col) := as.Date(get(date_col))]
  }
  if (byvar %in% names(d)) {
    d[, (byvar) := as.character(get(byvar))]
  }

  d[, i := seq_len(.N), by = byvar]
  d[order(get(date_col)), type := fifelse(i > .N - spec$future_steps, "Forecast", "Historical"), by = byvar]
  d[, i := NULL]
  d <- rbind(d, d[type == "Historical"][get(date_col) == max(get(date_col))][, type := "Forecast"], fill = TRUE)

  obs <- d[[data_cls$numerator_column]]
  if (!isTRUE(spec$use_count) && data_cls$denominator_column %in% names(d)) {
    obs <- obs / d[[data_cls$denominator_column]]
  }
  d[, observed := obs]

  d[order(get(date_col))] |> split(by = byvar)
}

# Legacy data-prep path that computes a dense grid of posterior quantiles for
# prediction plots. Newer paths should prefer stored calculated features.
prepare_time_series_plot_data <- function(
    model,
    data_cls,
    spec,
    probs = c(0.005, 0.01, 0.025,seq(0.05, .95, 0.05),0.975, 0.99, 0.995)
) {
  byvar <- data_cls$region_column
  date_col <- data_cls$date_column
  req(!is.null(byvar), !is.null(date_col))
  
  d <- get_posterior_quantiles(
    model, data_cls, use_suffix = FALSE, use_count_scale = isTRUE(spec$use_count), probs = probs
  )

  d[, i:=(1:.N), by=byvar]
  d[order(get(date_col)), type:=fifelse(i>.N-spec$future_steps, "Forecast", "Historical"), by=byvar]
  d <- rbind(d,d[type=="Historical"][get(date_col)==max(get(date_col))][, type:="Forecast"])


  # drop any future dates if use_count is TRUE (but this is too strong,
  # need to also consider the model)
  if(isTRUE(spec$use_count)) d <- d[type=="Historical"]

  # merge back on the original data  
  d <- data_cls$data[d, on=c(names(d)[1:2])]
  
  # generate observed
  
  obs = d[[data_cls$numerator_column]]
  if(!isTRUE(spec$use_count)) obs <- obs/d[[data_cls$denominator_column]]
  d[, observed:=obs]
  
  # return a list, split by county fips
  d[order(get(date_col))] |> split(by=byvar)
  
}

# Compatibility wrapper around prepare_time_series_plot_data() used by older
# callers that predate the time-series plot spec helper.
prepare_plot_ly_ts_data <- function(
    model,
    data_cls,
    use_count=TRUE,
    future_steps=0, 
    probs = c(0.005, 0.01, 0.025,seq(0.05, .95, 0.05),0.975, 0.99, 0.995)
) {
  spec <- build_time_series_plot_spec(
    region_ids = character(0),
    ci = 95,
    use_count = use_count,
    future_steps = future_steps
  )
  
  prepare_time_series_plot_data(
    model = model,
    data_cls = data_cls,
    spec = spec,
    probs = probs
  )
}

# Return county geometry joined to the locations present in the current data
# class so map views can render only the relevant regions.
get_map_locations <- function(dc, county_display="region", county_code="countyfips") {
  locs = dc$data[, .SD, .SDcols = c(county_code, county_display)] |> unique()
  
  map_data <- dplyr::right_join(
    mutate(Rnssp::county_sf, !!county_code:=paste0(STATEFP, COUNTYFP)),
    locs,
    by=county_code
  ) |> 
    sf::st_transform(crs = 4326)
}

# Attach the client-side hook that makes a leaflet legend draggable after the
# widget is rendered.
enable_draggable_legend <- function(map) {
  map |>  htmlwidgets::onRender(
    "function(el, x) { if (window.makeLeafletLegendDraggable) window.makeLeafletLegendDraggable(el); }"
  )
}

# Build a standardized map-data bundle from one stored single-column feature so
# leaflet helpers can render it without recomputing posterior summaries.
get_stored_map_feature_data <- function(feature_data, data_cls, feature) {
  req(!is.null(feature_data), !is.null(data_cls), !is.null(feature))
  
  data.table::setDT(feature_data)
  reg_col <- data_cls$region_column
  date_col <- data_cls$date_column
  out_cols <- feature$out_cols %||% character(0)
  value_col <- out_cols[[1]] %||% NULL
  
  req(!is.null(reg_col), !is.null(date_col), !is.null(value_col))
  req(value_col %in% names(feature_data))
  
  # Keep just the canonical region/date keys plus the selected stored output
  # column so downstream map helpers work with a predictable shape.
  keep_cols <- intersect(c(reg_col, date_col, value_col), names(feature_data))
  dt <- data.table::copy(feature_data)[, ..keep_cols]
  if (reg_col %in% names(dt)) dt[, (reg_col) := as.character(get(reg_col))]
  if (date_col %in% names(dt) && !inherits(dt[[date_col]], "Date")) dt[, (date_col) := as.Date(get(date_col))]
  
  vals <- suppressWarnings(as.numeric(dt[[value_col]]))
  finite_vals <- vals[is.finite(vals)]
  minv <- if (length(finite_vals)) min(finite_vals, na.rm = TRUE) else 0
  maxv <- if (length(finite_vals)) round_up_max(max(finite_vals, na.rm = TRUE)) else 1
  
  list(
    data = dt,
    column = value_col,
    name = feature$label %||% value_col,
    min = minv,
    max = maxv,
    date_col = date_col,
    region_col = reg_col
  )
}

# Summarize one stored feature over time for the draggable map date selector.
# Count-scale features sum across regions; all other scales average.
get_map_feature_sparkline_data <- function(feature_data, data_cls, feature, future_steps = 0L) {
  map_data <- get_stored_map_feature_data(feature_data, data_cls, feature)
  dt <- data.table::as.data.table(map_data$data)
  date_col <- map_data$date_col
  value_col <- map_data$column
  use_sum <- identical(feature$feature_scale %||% "other", "counts")
  
  if (use_sum) {
    series <- dt[, .(value = sum(get(value_col), na.rm = TRUE)), by = date_col]
  } else {
    series <- dt[, .(value = mean(get(value_col), na.rm = TRUE)), by = date_col]
  }
  data.table::setnames(series, date_col, "date")
  data.table::setorder(series, date)
  
  series[, type := "Historical"]
  if (future_steps > 0 && nrow(series) > future_steps) {
    series[(.N - future_steps + 1L):.N, type := "Forecast"]
    bridge <- data.table::copy(series[type == "Historical"][.N])
    if (nrow(bridge)) {
      bridge[, type := "Forecast"]
      series <- data.table::rbindlist(list(series, bridge), use.names = TRUE)
      # Preserve the historical-to-forecast handoff point so the dashed
      # forecast segment visually connects to the observed segment.
      series[, type_order := data.table::fifelse(type == "Historical", 1L, 2L)]
      data.table::setorder(series, date, type_order)
      series[, type_order := NULL]
    }
  }
  
  series[]
}

# Pull the stored time series for one region/feature pair for map popups.
get_map_feature_popup_data <- function(feature_data, data_cls, feature, region_id, future_steps = 0L) {
  map_data <- get_stored_map_feature_data(feature_data, data_cls, feature)
  dt <- data.table::as.data.table(feature_data)
  reg_col <- map_data$region_col
  date_col <- map_data$date_col
  value_col <- map_data$column
  region_label_col <- if ("region" %in% names(feature_data)) "region" else reg_col
  keep <- intersect(c(reg_col, region_label_col, date_col, value_col), names(dt))
  dt <- data.table::copy(dt)[, ..keep]
  
  out <- dt[get(reg_col) == as.character(region_id), .(
    region_label = as.character(get(region_label_col)),
    date = as.Date(get(date_col)),
    value = as.numeric(get(value_col))
  )]
  data.table::setorder(out, date)
  out[, type := "Historical"]
  if (future_steps > 0 && nrow(out) > future_steps) {
    out[(.N - future_steps + 1L):.N, type := "Forecast"]
  }
  
  list(
    ts = out[],
    region_label = out$region_label[[1]] %||% as.character(region_id),
    feature_label = feature$label %||% value_col
  )
}

# Build the full regional leaflet map for one stored feature and selected
# date, including color scaling, legend placement, and optional basemap tiles.
build_regional_feature_map <- function(
    map_locations,
    feature_data,
    data_cls,
    feature,
    target_date,
    includes_alaska_hawaii = TRUE,
    use_global_range = TRUE,
    palette = "viridis",
    legend_position = "bottomright"
) {
  map_data <- get_stored_map_feature_data(feature_data, data_cls, feature)
  pi <- polygon_info(map_locations, map_data, target_date)
  
  # Shared-range mode keeps the legend stable across dates, while per-date
  # mode recomputes the domain from the currently selected slice only.
  domain <- if (isTRUE(use_global_range)) {
    if (is.finite(map_data$max) && map_data$max > 0) c(min(0, map_data$min %||% 0), map_data$max) else c(0, 1)
  } else if (!is.null(pi$minv) && !is.null(pi$maxv) && is.finite(pi$maxv) && pi$maxv > 0) {
    c(min(0, pi$minv), pi$maxv)
  } else if (!is.null(pi$d) && "outcome" %in% names(pi$d) && any(is.finite(pi$d$outcome))) {
    rng <- range(pi$d$outcome, na.rm = TRUE)
    if (all(is.finite(rng)) && rng[2] > 0) c(min(0, rng[1]), rng[2]) else c(0, 1)
  } else {
    c(0, 1)
  }
  
  m <- leaflet::leaflet()
  if (!isTRUE(includes_alaska_hawaii)) {
    m <- leaflet::addProviderTiles(m, "CartoDB.Positron")
  }
  
  update_polygons(
    p = m,
    pi = pi,
    domain = domain,
    palette = palette,
    legend_position = legend_position
  ) |>
    leaflet.extras::addFullscreenControl() |>
    leaflet.extras::addResetMapButton()
}

# Build the draggable region-wide sparkline used to select the current map
# date from stored feature values.
build_map_feature_sparkline <- function(
    series,
    init_date,
    yaxis_title = "Value",
    colors = list(
      line = "#636EFA",
      draggable_line = "red"
    )
) {
  p <- plotly::plot_ly(source = "date_spark_src")
  
  hist_dt <- series[series$type == "Historical", , drop = FALSE]
  fc_dt <- series[series$type == "Forecast", , drop = FALSE]
  
  if (nrow(fc_dt) > 0) {
    p <- p |>
      plotly::add_trace(
        data = fc_dt,
        x = ~date, y = ~value,
        type = "scatter", mode = "lines+markers",
        line = list(color = colors[["line"]], dash = "dash"),
        marker = list(color = colors[["line"]]),
        hoverinfo = "x+y"
      )
  }
  
  if (nrow(hist_dt) > 0) {
    p <- p |>
      plotly::add_trace(
        data = hist_dt,
        x = ~date, y = ~value,
        type = "scatter", mode = "lines+markers",
        line = list(color = colors[["line"]]),
        marker = list(color = colors[["line"]]),
        hoverinfo = "x+y"
      )
  }
  
  p |>
    plotly::layout(
      margin = list(l = 28, r = 6, t = 4, b = 22),
      paper_bgcolor = "rgba(0,0,0,0)",
      plot_bgcolor  = "rgba(0,0,0,0)",
      xaxis = list(
        title = list(text = "", font = list(color = colors[["line"]], size = 10)),
        showticklabels = FALSE, ticks = "", showgrid = FALSE, zeroline = FALSE,
        fixedrange = TRUE
      ),
      yaxis = list(
        title = list(text = yaxis_title, font = list(color = colors[["line"]], size = 10)),
        showticklabels = FALSE, ticks = "", showgrid = FALSE, zeroline = FALSE,
        fixedrange = TRUE
      ),
      showlegend = FALSE,
      shapes = list(vertical_date_line(init_date, color = colors[["draggable_line"]]))
    ) |>
    plotly::config(
      displayModeBar = FALSE,
      edits = list(shapePosition = TRUE),
      scrollZoom = FALSE,
      doubleClick = FALSE
    )
}

# Build the region-level popup chart used when a polygon is clicked on the
# stored-feature map.
build_map_feature_popup_plot <- function(
    ts,
    region_label,
    feature_label,
    v_date,
    colors = list(
      point = "blue",
      line = "black",
      vline = "red"
    )
) {
  if (!nrow(ts)) {
    return(ggplot2::ggplot() + ggplot2::theme_void())
  }
  
  ts <- data.table::as.data.table(ts)
  ts[, type := factor(type, levels = c("Historical", "Forecast"))]
  
  ggplot2::ggplot(ts, ggplot2::aes(x = date, y = value)) +
    ggplot2::geom_point(ggplot2::aes(shape = type), color = colors[["point"]]) +
    ggplot2::geom_line(ggplot2::aes(linetype = type), color = colors[["line"]]) +
    ggplot2::geom_vline(
      ggplot2::aes(xintercept = v_date),
      linetype = "dashed",
      color = colors[["vline"]]
    ) +
    ggplot2::labs(
      title = paste(region_label, "-", feature_label),
      y = feature_label,
      x = "",
      caption = paste0("Selected Date: ", v_date)
    ) +
    ggplot2::scale_linetype_manual(values = c(Historical = "solid", Forecast = "dashed"), drop = FALSE) +
    ggplot2::theme_minimal() +
    ggplot2::theme(plot.caption = ggplot2::element_text(color = colors[["vline"]], size = 8))
}

# Build the draggable vertical reference line used by the map sparkline.
vertical_date_line <- function(cursor_date, color = "red") {
  list(
    type = "line", xref = "x", yref = "paper",
    x0 = cursor_date, x1 = cursor_date, y0 = 0, y1 = 1,
    line = list(color = color, width = 3, dash = "dash")
  )
}

