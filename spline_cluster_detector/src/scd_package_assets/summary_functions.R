# © 2025 The Johns Hopkins University Applied Physics Laboratory LLC
# Development of this software was sponsored by the U.S. Government under
# contract no. 75D30124C19958

#' Summary count-by-location-and-date data, given baseline and test interval
#' lengths, and an end-date for the test interval
#'
#' Function will return a summary data frame of information related to a given
#' count-by-location-and-date dataset, provided the user gives the count data, a
#' set of locations, and the length of the baseline and test intervals, and and
#' end date for the test interval. Note that a guard, a buffer between the end
#' of the baseline interval and the test interval can be provided.
#' @param data data frame with (at least) three columns: location, date, count
#' @param end_date date indicating end of test interval; if not provided the
#'   last date in `dt` will be used
#' @param locations a vector of locations to subset the table; if none provided
#'   then all locations will be used
#' @param baseline_length numeric (default=90) number of days in baseline
#'   interval
#' @param test_length numeric (default=7) number of days in test interval
#' @param guard numeric (default=0) number of days between baseline and test
#'   interval
#' @param cut_vec numeric vector of n cut points to examine categories of daily
#'   mean counts
#' @param cut_labels character vector of labels for the n-1 categories created
#'   by `cut_vec`
#' @export
#' @returns data frame of summary statistics
generate_summary_table <- function(
    data,
    end_date = NULL,
    locations = NULL,
    baseline_length = 90,
    test_length = 7,
    guard = 0,
    cut_vec = c(0, 1.5, 2.5, 5.5, 10.5, Inf),
    cut_labels = c(
      "Nr. Locs, daily mean 1 or less",
      "Nr. Locs, daily mean 2",
      "Nr. Locs, daily mean 3-5",
      "Nr. Locs, daily mean 6-10",
      "Nr. Locs, daily mean >10"
    )) {
  # set as data.table (check should this by data.table::copy())
  dt <- data.table::copy(data)
  data.table::setDT(dt)

  if (all(c("date", "location", "count") %in% names(dt)) == FALSE) {
    cli::cli_abort("one of date, location, or count not in provided data")
  }

  # set the end date as the last date in the fram, if this is not provided
  if (is.null(end_date)) {
    end_date <- max(dt$date, na.rm = TRUE)
  }

  # make sure end_date is Date
  end_date <- as.Date(end_date)

  # if locations not provided, then use them all.
  if (is.null(locations)) {
    locations <- unique(dt[, location])
  } else {
    # subset dt by the locations
    dt <- dt[location %in% locations]
  }

  if (nrow(dt) == 0) {
    cli::cli_abort("No data at these locations")
  }

  # check that length of cutVec is one more than cutLabels, and that the former
  # is numeric and the latter is string
  if (
    is.numeric(cut_vec) == FALSE ||
      is.character(cut_labels) == FALSE ||
      (length(cut_vec) - 1) != length(cut_labels)
  ) {
    cli::cli_abort("cutVec must be numeric, cutLabels must be character, and
         cutLabels length should be one less than cutLabels")
  }

  test_dates <- get_test_dates(end_date, test_length)
  baseline_dates <- get_baseline_dates(
    end_date,
    test_length,
    baseline_length,
    guard
  )

  dt_cases_baseline <- dt[
    data.table::between(date, min(baseline_dates), max(baseline_dates))
  ]

  dt_cases_test <- dt[
    data.table::between(date, min(test_dates), max(test_dates))
  ]

  nr_dates <- c(length(baseline_dates), length(test_dates))

  nr_total_cases <- c(
    sum(dt_cases_baseline$count, na.rm = TRUE),
    sum(dt_cases_test$count, na.rm = TRUE)
  )

  cases_per_day <- round(nr_total_cases / nr_dates, 1)

  nr_data_location <- c(
    dt_cases_baseline[, data.table::uniqueN(location)],
    dt_cases_test[, data.table::uniqueN(location)]
  )

  nr_zero_locations <- length(locations) - nr_data_location

  dt_stats <- data.table::data.table(
    rbind(
      nr_dates,
      nr_total_cases,
      cases_per_day,
      nr_data_location,
      nr_zero_locations
    )
  )
  dt_stats[, names(.SD) := lapply(.SD, as.double)]

  dt_stats <- cbind(
    c(
      "Nr. Dates",
      "Nr. Total Cases",
      "Cases per Day",
      "Nr. Locations with Data",
      "Nr. Locations, no records"
    ),
    dt_stats
  )

  names(dt_stats) <- c(
    "Statistic (rounded means)",
    "Baseline Interval",
    "Test Interval"
  )

  # global declarations to avoid check CMD errors
  count <- location <- loc_mean_cut <- loc_mean <- nr_baseline_zips <- NULL

  # get the means by each of the cut values above
  means <- lapply(list(dt_cases_baseline, dt_cases_test), \(d) {
    dstats <- d[
      ,
      list(
        loc_med = as.double(stats::median(count)),
        loc_mean = as.double(mean(count)),
        loc_max = as.double(max(count))
      ),
      by = "location"
    ]

    dstats[, loc_mean_cut := cut(
      loc_mean,
      breaks = cut_vec,
      labels = cut_labels
    )]

    dstats <- dstats[
      ,
      list(
        nr_baseline_zips = .N
      ),
      by = c("RoundedMeanCount" = "loc_mean_cut")
    ] |>
      _[order(-nr_baseline_zips)]

    merge(
      data.table::data.table(RoundedMeanCount = cut_labels),
      dstats,
      by = "RoundedMeanCount",
      all.x = TRUE
    )
  })

  # combine baseline and test
  combined_means <- cbind(means[[1]], means[[2]][, list(nr_baseline_zips)])

  data.table::setnames(
    combined_means,
    new = c("Statistic (rounded means)", "Baseline Interval", "Test Interval")
  )

  # fill any na values
  combined_means[
    ,
    names(.SD) := lapply(.SD, \(s) data.table::nafill(s, fill = 0)),
    .SDcols = is.numeric
  ]

  # row bind the overall stats and the numbers by cut-values, and return
  return(rbind(dt_stats, combined_means))
}

#' Get heat map data from a set of location, date, count data
#'
#' Generate heat map data frame count information by date and location given an
#' input frame of count-by-location-and-date data.
#' @param data data frame with (at least) three columns: location, date, count
#' @param end_date date indicating end of test interval; if not provided the
#'   last date in `dt` will be used
#' @param locations a vector of locations to subset the table; if none provided
#'   then all locations will be used
#' @param baseline_length numeric (default=90) number of days in baseline
#'   interval
#' @param test_length numeric (default=7) number of days in test interval
#' @param guard numeric (default=0) number of days between baseline and test
#'   interval
#' @param break_points break points for the discrete groups (default =
#'   \code{c(-1,2,4,9,19,Inf)})
#' @param break_labels string vector of labels for the groups (default =
#'   \code{c("0-1", "2-4", "5-9", "10-19", "20+")})
#' @export
#' @returns a data frame of heat map data

generate_heatmap_data <- function(
    data,
    end_date = NULL,
    locations = NULL,
    baseline_length = 90,
    test_length = 7,
    guard = 0,
    break_points = c(-1, 2, 4, 9, 19, Inf),
    break_labels = c("0-1", "2-4", "5-9", "10-19", "20+")) {
  # set as data.table (check should this by data.table::copy())
  dt <- data.table::copy(data)
  data.table::setDT(dt)

  check_vars(data, c("date", "location", "count"))

  # check that break_points and break_labels are the same lengths
  if (length(break_points) != (length(break_labels) + 1)) {
    cli::cli_abort("Break points must be one more than break labels")
  }

  # global declarations to avoid check CMD errors
  location <- count_cat <- count <- NULL

  # set the end date as the last date in the fram, if this is not provided
  if (is.null(end_date)) {
    end_date <- max(dt$date, na.rm = TRUE)
  }
  end_date <- as.Date(end_date)

  # if locations not provided, then use them all.
  if (is.null(locations)) {
    locations <- unique(dt[, location])
  } else {
    # subset dt by the locations
    dt <- dt[location %in% locations]
  }

  if (nrow(dt) == 0) {
    cli::cli_abort("No data at these locations")
  }

  dt[, location := as.character(location)]

  # shouldn't these breaks and labels be user-provided?
  dt[, count_cat := cut(
    count,
    breaks = break_points,
    labels = break_labels
  )]

  start_date <- get_baseline_dates(
    end_date,
    test_length,
    baseline_length,
    guard
  )[1]

  end_date <- data.table::as.IDate(end_date)

  plot_data <- dt[data.table::between(date, start_date, end_date)]
  # set some attributes
  attr(plot_data, "start_date") <- start_date
  attr(plot_data, "baseline_length") <- baseline_length

  return(plot_data)
}




#' Generate heatmap of data
#'
#' Generate a ggplot heatmap of count information by date and location given a
#' frame of count-by-location-and-date data.
#' @param heatmap_data data frame generated by `generate_heatmap_data`
#' @param plot_type string indicating either a "ggplot" or "plotly" result
#' @param ... passed onto plotly
#' @export
#' @returns a ggplot or plotly object
generate_heatmap <- function(
    heatmap_data,
    plot_type = c("ggplot", "plotly"),
    ...) {
  plot_type <- match.arg(plot_type)

  if (plot_type == "ggplot") {
    return(ggplot_heatmap(heatmap_data))
  }

  if (plot_type == "plotly") {
    return(plotly_heatmap(heatmap_data, ...))
  }
}

plotly_heatmap <- function(
    heatmap_data, x = "date", y = "location", z = "count", logscale = FALSE) {
  af <- \(s) stats::as.formula(paste0("~", s))

  v_line <- function(d) {
    list(
      type = "line", y0 = 0, y1 = 1, yref = "paper",
      x0 = d, x1 = d, line = list(color = "red", width = 4)
    )
  }

  hovertext = "Count: %{z}<br>Date: %{x}<br>Loc: %{y}<extra></extra>"
  if (logscale) {
    z <- paste0("log(", z, ")")
    hovertext = paste0("Log ", hovertext)
  }
  
  p <- plotly::plot_ly(
    heatmap_data,
    x = af(x), y = af(y), z = af(z),
    type = "heatmap",
    hoverongaps=F,
    hovertemplate = hovertext,
    colorbar = list(
      title = data.table::fifelse(logscale, "Log Count", "Count")
    )
  ) |>
    plotly::layout(
      xaxis = list(title = "Date"),
      yaxis = list(title = "", autorange="reversed")
    )

  if (!is.null(attr(heatmap_data, "start_date"))) {
    line_date <- attr(heatmap_data, "start_date") +
      attr(heatmap_data, "baseline_length") +
      1
    p <- p |> plotly::layout(shapes = list(v_line(line_date)))
  }


  return(p)
}

ggplot_heatmap <- function(heatmap_data) {
  location <- count_cat <- NULL
  # make the plot
  dt_cases_heatmap <- ggplot2::ggplot(
    data = heatmap_data,
    mapping = ggplot2::aes(
      x = data.table::as.IDate(date),
      y = location,
      fill = factor(count_cat)
    )
  ) +
    ggplot2::geom_tile() +
    ggplot2::geom_vline(
      xintercept = as.numeric(
        attr(heatmap_data, "start_date") +
          attr(heatmap_data, "baseline_length") -
          1
      ),
      linetype = "dashed", color = "red", linewidth = 1
    ) +
    ggplot2::xlab(label = "date") +
    ggplot2::scale_x_date(date_labels = "%b %d %y'", date_breaks = "2 weeks") +
    ggplot2::theme(
      axis.text.y = ggplot2::element_blank(),
      axis.ticks.y = ggplot2::element_blank(),
      panel.background = ggplot2::element_rect(fill = "yellow")
    ) +
    ggplot2::scale_fill_manual(
      values = c("black", "green", "red", "blue", "brown")
    ) +
    ggplot2::labs(fill = "Data Range")

  dt_cases_heatmap
}

#' Generate time series data
#'
#' Function returns a time series of counts-by-location-and-date data,
#' given length of baseline and test intervals, and an end date for the
#' test-interval
#' @param data data frame with (at least) three columns: location, date, count
#' @param end_date date indicating end of test interval; if not provided the
#'   last date in `dt` will be used
#' @param locations a vector of locations to subset the table; if none provided
#'   then all locations will be used
#' @param baseline_length numeric (default=90) number of days in baseline
#'   interval
#' @param test_length numeric (default=7) number of days in test interval
#' @param guard numeric (default=0) number of days between baseline and test
#'   interval
#' @export
#' @returns a ggplot object
generate_time_series_data <- function(
    data,
    end_date = NULL,
    locations = NULL,
    baseline_length = 90,
    test_length = 7,
    guard = 0) {
  # set as data.table (check should this by data.table::copy())
  dt <- data.table::copy(data)
  data.table::setDT(dt)

  check_vars(d = dt, required = c("date", "location", "count"))

  # global declarations to avoid check CMD errors
  location <- count <- NULL

  # set the end date as the last date in the frame, if this is not provided
  if (is.null(end_date)) {
    end_date <- max(dt$date, na.rm = TRUE)
  }
  end_date <- as.Date(end_date)

  # if locations not provided, then use them all.
  if (is.null(locations)) {
    locations <- unique(dt[, location])
  } else {
    # subset dt by the locations
    dt <- dt[location %in% locations]
  }

  if (nrow(dt) == 0) {
    cli::cli_abort("No data at these locations")
  }

  baseline_dates <- get_baseline_dates(
    end_date,
    test_length,
    baseline_length,
    guard
  )

  dt_cases_ts <- dt[
    date >= baseline_dates[1],
    list(date_count = sum(count, na.rm = TRUE)),
    by = c("date")
  ]

  return(dt_cases_ts)
}


#' Generate timeseries plot data
#'
#' Generate a timeseries plot of count information by date and location given a
#' frame of count-by-location-and-date data and an optional end_date
#' @param time_series_data data frame generated by `generate_time_series_data`
#' @param end_date optional end date to truncate date
#' @param plot_type string indicating either a "ggplot" or "plotly" result
#' @param ... passed onto plotly
#' @export
#' @returns a ggplot or plotly object

generate_time_series_plot <- function(
    time_series_data,
    end_date = NULL,
    plot_type = c("ggplot", "plotly"),
    ...) {
  plot_type <- match.arg(plot_type)

  if (is.null(end_date)) end_date <- time_series_data[, max(date)]

  if (plot_type == "ggplot") {
    p <- generate_ggplot_time_series(time_series_data[date <= end_date])
  }

  if (plot_type == "plotly") {
    p <- generate_plotly_time_series(time_series_data[date <= end_date], ...)
  }
  return(p)
}


generate_ggplot_time_series <- function(
    time_series_data) {
  date_count <- NULL
  time_series_plot <- ggplot2::ggplot(
    data = time_series_data,
    ggplot2::aes(x = as.Date(date), y = date_count)
  ) +
    ggplot2::geom_line() +
    ggplot2::ylab("Record Count") +
    ggplot2::scale_x_date(date_labels = "%b-%d-%y", date_breaks = "2 weeks") +
    ggplot2::xlab("Date") +
    ggplot2::ggtitle("Daily Record Count for All Locations")

  time_series_plot
}

generate_plotly_time_series <- function(
    time_series_data,
    ...) {
  
  time_series_plot <- plotly::plot_ly(
    data = time_series_data[order(date)],
    x = ~date,
    y = ~date_count,
    type = "scatter",
    mode = "line"
  ) |>
    plotly::layout(
      title = "Daily Record Count for All Locations",
      xaxis = list(title = "Date", dtick = 1209600000, tickformat = "%b-%d-%y"),
      yaxis = list(title = "Record Count")
    )

  time_series_plot
}
